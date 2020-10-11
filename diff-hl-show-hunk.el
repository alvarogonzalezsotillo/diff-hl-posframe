;;; diff-hl-show-hunk.el --- Integrate posframe/tooltip and diff-hl-diff-goto-hunk -*- lexical-binding: t -*-


;; Author:   Álvaro González Sotillo <alvarogonzalezsotillo@gmail.com>
;; URL:      https://github.com/alvarogonzalezsotillo/diff-hl-show-hunk.
;; Keywords: vc, diff, diff-hl, posframe
;; Version:  1.0
;; Package-Requires: ((diff-hl "1.8.7") (posframe "0.8.0"))

;;; Commentary:

;; `diff-hl-show-hunk-mode' shows a posframe/tooltip with the current
;; modified hunk when clicking in the margin or the fringe.  It
;; fallbacks to `diff-hl-diff-goto-hunk' if there is not a
;; `diff-hl-show-hunk-function` defined

;;
;; To use it in all buffers:
;;
;; (global-diff-hl-show-hunk-mode)
;;

;;; Code:



(require 'posframe)
(require 'diff-hl)

(defun diff-hl-show-hunk--log (&rest args)
  (apply 'message args))


(defvar diff-hl-show-hunk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<left-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    map)
  "Keymap for command `diff-hl-show-hunk-mode'.")

(defvar diff-hl-show-hunk-buffer-name "*diff-hl-show-hunk-buffer*" "Name of the posframe used by diff-hl-show-hunk.")
(defvar diff-hl-show-hunk--frame nil "The postframe frame used in function `diff-hl-show-hunk-posframe'.")

(defgroup diff-hl-show-hunk-group nil
  "Show vc diffs in a posframe."
  :group 'convenience)

(defcustom diff-hl-show-hunk-boundary "^@@.*@@"
  "Regex that marks the boundary of a hunk in *vc-diff* buffer."
  :type 'string)


(defcustom diff-hl-show-hunk-posframe-internal-border-width 2
  "Internal border width of the posframe."
  :type 'integer)


(defcustom diff-hl-show-hunk-posframe-internal-border-color "#00ffff"
  "Internal border color of the posframe.  If it doesn't work, try with `internal-border` face."
  :type 'color)

(defcustom diff-hl-show-hunk-narrow t
  "Narrow the differences to the current hunk."
  :type 'boolean)

(defcustom diff-hl-show-hunk-posframe-poshandler nil
  "Poshandler of the posframe (see `posframe-show`)."
  :type 'function)

(defcustom diff-hl-show-hunk-posframe-parameters nil
  "The frame parameters used by helm-posframe."
  :type 'string)

(defcustom diff-hl-show-hunk-function 'diff-hl-show-hunk-function-default
  "The function used to reder the hunk.
The function receives as first parameter a buffer with the
contents of the hunk, and as second parameter the line number
corresponding to the clicked line in the original buffer.  There
are some built in funcions: `diff-hl-show-hunk-function-default',
`diff-hl-show-hunk-popup' and `diff-hl-show-hunk-posframe'"
  :type 'function)

(defface diff-hl-show-hunk-clicked-line-face
  '((t (:inverse-video t)))
  "Face for the clicked line in the diff output.")

(defface diff-hl-show-hunk-face
  '((t (:height 0.8)))
  "Face for the posframe.")

(defun diff-hl-show-hunk-ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing a posframe or a popup."
  (member command '(ignore diff-hl-show-hunk handle-switch-frame diff-hl-show-hunk--click)))

(defun diff-hl-show-hunk--hide-handler  (_info)
  "Hide the posframe if the event is outside the posframe (after the posframe has been opened)."

  (if (not (frame-visible-p diff-hl-show-hunk--frame))
      t
    (let* ((ignore-command-p (diff-hl-show-hunk-ignorable-command-p this-command))
           (command-in-posframe-p (eq last-event-frame diff-hl-show-hunk--frame))
           (keep-open-p (or command-in-posframe-p ignore-command-p)))
      (not keep-open-p))))


(defun diff-hl-show-hunk-buffer ()
  "Create the buffer with the contents of the hunk at point.
The buffer has the point in the corresponding line of the hunk.
Returns a list with the buffer and the line number of the clicked line."

  (let ((content)
        (point-in-buffer)
        (line)
        (overlay)
        (inhibit-redisplay t) ;;https://emacs.stackexchange.com/questions/35680/stop-emacs-from-updating-display
        (buffer (get-buffer-create diff-hl-show-hunk-buffer-name)))
    

    ;; Get differences
    (save-window-excursion
      (save-excursion
        (diff-hl-diff-goto-hunk)
        (with-current-buffer "*vc-diff*"
          (setq content (buffer-string))
          (setq point-in-buffer (point)))))

    (with-current-buffer buffer

      (erase-buffer)
      (insert content)
      
      ;; Highlight the clicked line
      (goto-char point-in-buffer)
      (setq overlay (make-overlay (point-at-bol) (min (point-max) (1+ (point-at-eol)))))
      (overlay-put overlay 'face 'diff-hl-show-hunk-clicked-line-face)
      
      ;; diff-mode, highlight hunks boundaries
      (diff-mode)
      (highlight-regexp diff-hl-show-hunk-boundary)
      

      ;; Change face size
      (buffer-face-set 'diff-hl-show-hunk-face)
      

      ;;  Find the hunk and narrow to it
      (when diff-hl-show-hunk-narrow
        (re-search-backward diff-hl-show-hunk-boundary nil 1)
        (forward-line 1)
        (let* ((start (point)))
          (re-search-forward diff-hl-show-hunk-boundary nil 1)
          (move-beginning-of-line nil)
          (narrow-to-region start (point)))
        ;; Come back to the clicked line
        (goto-char (overlay-start overlay)))
      

      (setq line (line-number-at-pos)))
    
    (list buffer line)))


(defun diff-hl-show-hunk--click (event)
  "Called when user clicks on margins.  EVENT is click information."
  (interactive "event")

  ;; Go to clicked spot
  (posn-set-point (event-start event))
  (diff-hl-show-hunk))


(defun diff-hl-show-hunk--popup-up ()
  (interactive)
  (when diff-hl-show-hunk--popup
    (popup-scroll-up diff-hl-show-hunk--popup)))

(defun diff-hl-show-hunk--popup-pageup ()
  (interactive)
  (when diff-hl-show-hunk--popup
    (popup-scroll-up diff-hl-show-hunk--popup (popup-height diff-hl-show-hunk--popup))))

(defun diff-hl-show-hunk--popup-pagedown ()
  (interactive)
  (when diff-hl-show-hunk--popup
    (popup-scroll-down diff-hl-show-hunk--popup (popup-height diff-hl-show-hunk--popup))))

(defun diff-hl-show-hunk--popup-down ()
  (interactive)
  (when diff-hl-show-hunk--popup
    (popup-scroll-down diff-hl-show-hunk--popup)))

(defun diff-hl-show-hunk--popup-hide ()
  (interactive)
  (diff-hl-show-hunk--popup-transient-mode -1)
  (when diff-hl-show-hunk--popup
    (popup-hide diff-hl-show-hunk--popup)
    (setq diff-hl-show-hunk--popup nil)))

(defvar diff-hl-show-hunk--popup nil "Popup where show the current hunk.")

(defvar diff-hl-show-hunk--popup-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<prior>") 'diff-hl-show-hunk--popup-pageup)
    (define-key map (kbd "M-v") 'diff-hl-show-hunk--popup-pageup)
    (define-key map (kbd "<next>") 'diff-hl-show-hunk--popup-pagedown)
    (define-key map (kbd "C-v") 'diff-hl-show-hunk--popup-pagedown)
    (define-key map (kbd "<up>") 'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "C-p") 'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<down>") 'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "C-n") 'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "C-g") 'diff-hl-show-hunk--popup-hide)
    (define-key map [escape] 'diff-hl-show-hunk--popup-hide)
    ;;http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
    (define-key map (kbd "<mouse-4>") 'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<wheel-up>") 'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<mouse-5>") 'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "<wheel-down>") 'diff-hl-show-hunk--popup-down)
    
    map)
  "Keymap for command `diff-hl-show-hunk--popup-transient-mode'.
Capture all the vertical movement of the point, and converts it
to scroll in the popup")

(define-minor-mode diff-hl-show-hunk--popup-transient-mode
  "Temporal minor mode to control diff-hl popup."

  :global nil
  :group diff-hl-show-hunk-group
  
  (diff-hl-show-hunk--log "diff-hl-show-hunk--popup-transient-mode:%s" diff-hl-show-hunk--popup-transient-mode)
  
  (remove-hook 'post-command-hook #'diff-hl-show-hunk--popup-post-command-hook nil)
  (when diff-hl-show-hunk--popup-transient-mode
    (add-hook 'post-command-hook #'diff-hl-show-hunk--popup-post-command-hook nil)))

(defun diff-hl-show-hunk--popup-post-command-hook ()
  "Called each time the region is changed."
  (let ((allowed-command (or
                          (diff-hl-show-hunk-ignorable-command-p this-command)
                          (string-match-p "diff-hl-" (symbol-name this-command)))))
    (diff-hl-show-hunk--log "this-command:%s %s" this-command  allowed-command)
    (unless allowed-command
      (diff-hl-show-hunk--popup-hide))))



(defun diff-hl-show-hunk-popup (buffer line)
  "Implementation to show the hunk in a posframe.  BUFFER is a buffer with the hunk, and the central line should be LINE."
  
  (unless (featurep 'popup)
    (error "Required package for diff-hl-show-hunk-popup not available: popup.  Please customize diff-hl-show-hunk-function"))

  (require 'popup)

  
  (diff-hl-show-hunk--popup-hide)
  
  (let* ((lines (split-string (with-current-buffer buffer (buffer-string)) "[\n\r]+" ))
         (popup (popup-create (point) 80 20  :around t :scroll-bar t))
         (line (max 0 (- line 1)))
         (clicked-line (propertize (nth line lines) 'face 'diff-hl-show-hunk-clicked-line-face)))
    (setq diff-hl-show-hunk--popup popup)
    (setcar (nthcdr line lines) clicked-line)
    (popup-set-list popup lines)
    (popup-scroll-down popup line)
    (popup-select popup line)
    (popup-draw popup)
    (diff-hl-show-hunk--log "popup shown")
    (diff-hl-show-hunk--popup-transient-mode))
  t)

(defun diff-hl-show-hunk-function-default (buffer line)
  "Show a posframe or a popup with the hunk in BUFFER, at  LINE."
  (cond ((and (featurep 'posframe) (posframe-workable-p))
         (diff-hl-show-hunk-posframe buffer line))
        ((featurep 'popup)
         (diff-hl-show-hunk-popup buffer line))
        (t
         nil)))



(defun diff-hl-show-hunk-posframe (buffer line)
  "Implementation to show the hunk in a posframe.  BUFFER is a buffer with the hunk, and the central line should be LINE."

  (unless (featurep 'posframe)
    (error "Required package for diff-hl-show-hunk-posframe not available: posframe.  Please customize diff-hl-show-hunk-function"))

  (require 'posframe)

  (setq posframe-mouse-banish nil)
  (setq
   diff-hl-show-hunk--frame
   (posframe-show buffer
                  :position (point)
                  :poshandler diff-hl-show-hunk-posframe-poshandler
                  :internal-border-width diff-hl-show-hunk-posframe-internal-border-width
                  :accept-focus  nil
                  :internal-border-color diff-hl-show-hunk-posframe-internal-border-color ; Doesn't always work, better define internal-border face
                  :hidehandler 'diff-hl-show-hunk--hide-handler
                  :respect-header-line t
                  :override-parameters diff-hl-show-hunk-posframe-parameters))

  ;; Recenter arround point
  (with-selected-frame diff-hl-show-hunk--frame
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1- line))
      (select-window (window-main-window diff-hl-show-hunk--frame))
      (recenter)))
  t)

(defun diff-hl-show-hunk ()
  "Show a the diffs with vc last version in a posframe, if available.
If not, it fallbacks to `diff-hl-diff-goto-hunk`."
  (interactive)
  (cond ((not (vc-backend buffer-file-name))
         (message "The buffer is not under version control"))
        ((not (diff-hl-hunk-overlay-at (point)))
         (message "There is no modified hunk at pos %s" (point)))
        ((not diff-hl-show-hunk-function)
         (diff-hl-diff-goto-hunk))
        ((not (let ((buffer-and-line (diff-hl-show-hunk-buffer)))
                (apply diff-hl-show-hunk-function buffer-and-line)))
         (diff-hl-show-hunk--log "Ha devuelto nil")
         (diff-hl-diff-goto-hunk))))



;;;###autoload
(define-minor-mode diff-hl-show-hunk-mode
  "Enables the margin and fringe to show a posframe with vc diffs when clicked.
By default, the posframe shows only the current hunk, and the line of the hunk that matches the current position is highlighted.
The posframe face, border and other visual preferences are customizable.
The posframe can be also invoked with the command `diff-hl-show-hunk`"
  :group diff-hl-show-hunk-group)

;;;###autoload
(define-globalized-minor-mode global-diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode)

(provide 'diff-hl-show-hunk)
;;; diff-hl-show-hunk.el ends here
