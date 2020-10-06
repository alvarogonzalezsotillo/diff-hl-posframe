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

(defvar diff-hl-show-hunk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<left-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    map)
  "Keymap for command `diff-hl-show-hunk-mode'.")

(defvar diff-hl-show-hunk-buffer-name "*diff-hl-show-hunk-buffer*" "Name of the posframe used by diff-hl-show-hunk.")
(defvar diff-hl-show-hunk-frame nil "The postframe frame used in diff-hl-show-hunk package.")

(defgroup diff-hl-show-hunk-group nil
  "Show vc diffs in a posframe."
  :group 'convenience)

(defcustom diff-hl-show-hunk-boundary "^@@.*@@"
  "Regex that marks the boundary of a hunk in *vc-diff* buffer."
  :type 'string)


(defcustom diff-hl-show-hunk-internal-border-width 2
  "Internal border width of the posframe.  The color can be customized with `internal-border` face."
  :type 'integer)


(defcustom diff-hl-show-hunk-internal-border-color "#00ffff"
  "Internal border color of the posframe.  If it doesn't work, try with `internal-border` face."
  :type 'color)

(defcustom diff-hl-show-hunk-narrow t
  "Narrow the differences to the current hunk."
  :type 'boolean)

(defcustom diff-hl-show-hunk-poshandler nil
  "Poshandler of the posframe (see `posframe-show`)."
  :type 'function)

(defcustom diff-hl-show-hunk-parameters nil
  "The frame parameters used by helm-posframe."
  :type 'string)

(defface diff-hl-show-hunk-clicked-line-face
  '((t (:inverse-video t)))
  "Face for the clicked line in the diff output.")

(defface diff-hl-show-hunk-face
  '((t (:height 0.8)))
  "Face for the posframe.")

(defun diff-hl-show-hunk--hide-handler  (_info)
  "Hide the posframe if the event is outside the posframe (after the posframe has been opened)."

  (if (not (frame-visible-p diff-hl-show-hunk-frame))
      t
    (let* ((invoking-command-p (or
                                (eq this-command 'diff-hl-show-hunk--click)
                                (eq this-command 'diff-hl-show-hunk)
                                (eq this-command 'handle-switch-frame)
                                ))
           (ignore-command-p (eq this-command 'ignore))
           (command-in-posframe-p (eq last-event-frame diff-hl-show-hunk-frame))
           (keep-open-p (or invoking-command-p command-in-posframe-p ignore-command-p)))
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
      (highlight-regexp diff-hl-show-hunk-hunk-boundary)
      

      ;; Change face size
      (buffer-face-set 'diff-hl-show-hunk-face)
      

      ;;  Find the hunk and narrow to it
      (when diff-hl-show-hunk-narrow
        (re-search-backward diff-hl-show-hunk-hunk-boundary nil 1)
        (forward-line 1)
        (let* ((start (point)))
          (re-search-forward diff-hl-show-hunk-hunk-boundary nil 1)
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


(defun diff-hl-popup--up ()
  (interactive)
  (when diff-hl-popup
    (popup-scroll-up diff-hl-popup)))

(defun diff-hl-popup--pageup ()
  (interactive)
  (when diff-hl-popup
    (popup-scroll-up diff-hl-popup (popup-height diff-hl-popup))))

(defun diff-hl-popup--pagedown ()
  (interactive)
  (when diff-hl-popup
    (popup-scroll-down diff-hl-popup (popup-height diff-hl-popup))))

(defun diff-hl-popup--down ()
  (interactive)
  (when diff-hl-popup
    (popup-scroll-down diff-hl-popup)))

(defun diff-hl-popup--hide ()
  (interactive)
  (when diff-hl-popup
    (diff-hl-popup-transient-mode -1)
    (popup-hide diff-hl-popup)
    (setq diff-hl-popup nil)))

(defvar diff-hl-popup nil "Popup where show the current hunk.")
  
(defvar diff-hl-popup-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<prior>") 'diff-hl-popup--pageup)
    (define-key map (kbd "M-v") 'diff-hl-popup--pageup)
    (define-key map (kbd "<next>") 'diff-hl-popup--pagedown)
    (define-key map (kbd "C-v") 'diff-hl-popup--pagedown)
    (define-key map (kbd "<up>") 'diff-hl-popup--up)
    (define-key map (kbd "C-p") 'diff-hl-popup--up)
    (define-key map (kbd "<down>") 'diff-hl-popup--down)
    (define-key map (kbd "C-n") 'diff-hl-popup--down)
    (define-key map (kbd "C-g") 'diff-hl-popup--hide)
    (define-key map [escape] 'diff-hl-popup--hide)
    
    map)
  "Keymap for command `diff-hl-popup-transient-mode'.")

(define-minor-mode diff-hl-popup-transient-mode
  "Temporal minor mode to control diff-hl popup."

  :global nil
  :group diff-hl-show-hunk-group
  
  (message "diff-hl-popup-transient-mode:%s" diff-hl-popup-transient-mode)
  
  (remove-hook 'post-command-hook #'diff-hl-popup--post-command-hook nil)
  (when diff-hl-popup-transient-mode
    (add-hook 'post-command-hook #'diff-hl-popup--post-command-hook nil)))



(defun diff-hl-popup--post-command-hook ()
  "Called each time the region is changed."
  (message "last-command-event:%s %s" last-command-event (type-of last-command-event)))



(defun diff-hl-show-hunk-popup (buffer line)
  "Implementation to show the hunk in a posframe.  BUFFER is a buffer with the hunk, and the central line should be LINE."
  
  (let ((popup (popup-tip buffer :scroll-bar t :nowait t )))
    (setq diff-hl-popup popup)
    (popup-scroll-down popup line)
    (popup-select popup line)
    (diff-hl-popup-transient-mode)
    (message "popup shown")
    ))

(defun diff-hl-show-hunk-posframe (buffer line)
  "Implementation to show the hunk in a posframe.  BUFFER is a buffer with the hunk, and the central line should be LINE."
  (setq posframe-mouse-banish nil)
  (setq
   diff-hl-show-hunk-frame
   (posframe-show buffer
                  :position (point)
                  :poshandler diff-hl-show-hunk-poshandler
                  :internal-border-width diff-hl-show-hunk-internal-border-width
                  :accept-focus  nil
                  :internal-border-color diff-hl-show-hunk-internal-border-color ; Doesn't always work, better define internal-border face
                  :hidehandler 'diff-hl-show-hunk--hide-handler
                  :override-parameters diff-hl-show-hunk-parameters))

  ;; Recenter arround point
  (with-selected-frame diff-hl-show-hunk-frame
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1- line))
      (insert "hola")
      (select-window (window-main-window diff-hl-show-hunk-frame))
      (recenter))))

(defun diff-hl-show-hunk ()
  "Show a the diffs with vc last version in a posframe, if available.
If not, it fallbacks to `diff-hl-diff-goto-hunk`."
  (interactive)
  (cond ((not (vc-backend buffer-file-name))
         (message "The buffer is not under version control"))
        ((not (diff-hl-hunk-overlay-at (point)))
         (message "There is no modified hunk at pos %s" (point)))
        ((not (posframe-workable-p))
         (diff-hl-diff-goto-hunk))
        (t

         (let* ((buffer-and-line (diff-hl-show-hunk-buffer))
                (buffer (elt buffer-and-line 0))
                (line (elt buffer-and-line 1)))
           (diff-hl-show-hunk-posframe buffer line)))))



;;;###autoload
(define-minor-mode diff-hl-show-hunk-mode
  "Enables the margin and fringe to show a posframe with vc diffs when clicked.
By default, the posframe shows only the current hunk, and the line of the hunk that matches the current position is highlighted.
The posframe face, border and other visual preferences are customizable.
The posframe can be also invoked with the command `diff-hl-show-hunk`"
  :group diff-hl-show-hunk-group

  (unless (and (featurep 'diff-hl) (featurep 'posframe) )
    (error "Required packages not available: diff-hl-show-hunk-mode needs diff-hl and posframe")))

;;;###autoload
(define-globalized-minor-mode diff-hl-show-hunk-global-mode
  diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode)

(provide 'diff-hl-show-hunk)
;;; diff-hl-show-hunk.el ends here
