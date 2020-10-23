;;; diff-hl-show-hunk.el --- Integrate popup and diff-hl-diff-goto-hunk -*- lexical-binding: t -*-


;; Author:   Álvaro González Sotillo <alvarogonzalezsotillo@gmail.com>
;; URL:      https://github.com/alvarogonzalezsotillo/diff-hl-show-hunk.
;; Keywords: vc, diff, diff-hl, popup
;; Version:  1.0
;; Package-Requires: ((diff-hl "1.8.7"))

;;; Commentary:

;; `diff-hl-show-hunk' shows a posframe/tooltip with the modified hunk
;; at point.  `diff-hl-show-hunk-function' contains the backend used
;; to show the hunk.  It fallbacks to `diff-hl-diff-goto-hunk' if
;; there is not a `diff-hl-show-hunk-function' defined.

;; `diff-hl-show-hunk-mode' shows the posframe/popup when clicking
;; in the margin or the fringe.

;;
;; To use it in all buffers:
;;
;; (global-diff-hl-show-hunk-mode)
;;

;;; Code:


(require 'diff-hl)

(defun diff-hl-show-hunk--log (&rest _args)
  "Used for debugging purposes."
  ;;(apply 'message args)
  nil)


(defvar diff-hl-show-hunk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-margin> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<left-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "<right-fringe> <mouse-1>") 'diff-hl-show-hunk--click)
    (define-key map (kbd "C-x v +") 'diff-hl-show-hunk)
    map)
  "Keymap for command `diff-hl-show-hunk-mode'.")

(defvar diff-hl-show-hunk-buffer-name "*diff-hl-show-hunk-buffer*" "Name of the posframe used by diff-hl-show-hunk.")
(defvar diff-hl-show-hunk--frame nil "The postframe frame used in function `diff-hl-show-hunk-posframe'.")
(defvar diff-hl-show-hunk--original-frame nil "The frame from which posframe is shown.")


(defgroup diff-hl-show-hunk-group nil
  "Show vc diffs in a posframe or popup."
  :group 'convenience)

(defcustom diff-hl-show-hunk-boundary "^@@.*@@"
  "Regex that marks the boundary of a hunk in *vc-diff* buffer."
  :type 'string)

(defcustom diff-hl-show-hunk-posframe-show-head-line t
  "Show some useful buttons at the top of the diff-hl posframe."
  :type 'boolean)


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
corresponding to the clicked line in the original buffer.  The
function should return t if the hunk is show, or nil if not.
There are some built in funcions:
`diff-hl-show-hunk-function-default', `diff-hl-show-hunk-popup'
and `diff-hl-show-hunk-posframe'"
  :type 'function)

(defvar diff-hl-show-hunk--hide-function nil "Function to call to close the shown hunk.")

(defface diff-hl-show-hunk-clicked-line-face
  '((t (:inverse-video t)))
  "Face for the clicked line in the diff output.")

(defface diff-hl-show-hunk-face
  '((t (:height 0.8)))
  "Face for the posframe.")

(defun diff-hl-show-hunk-ignorable-command-p (command)
  "Decide if COMMAND is a command allowed while showing a posframe or a popup."
  (member command '(ignore diff-hl-show-hunk handle-switch-frame diff-hl-show-hunk--click)))


(defun diff-hl-show-hunk-buffer ()
  "Create the buffer with the contents of the hunk at point.
The buffer has the point in the corresponding line of the hunk.
Returns a list with the buffer and the line number of the clicked line."

  (let ((content)
        (point-in-buffer)
        (line)
        (line-overlay)
        (inhibit-redisplay t) ;;https://emacs.stackexchange.com/questions/35680/stop-emacs-from-updating-display
        (buffer (get-buffer-create diff-hl-show-hunk-buffer-name)))
    

    ;; Get differences
    (save-window-excursion
      (save-excursion
        (diff-hl-diff-goto-hunk)
        (with-current-buffer "*vc-diff*"
          (setq content (buffer-substring-no-properties (point-min) (point-max)))
          (setq point-in-buffer (point)))))

    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert content)
      
      ;; Highlight the clicked line
      (goto-char point-in-buffer)
      (setq line-overlay (make-overlay (point-at-bol) (min (point-max) (1+ (point-at-eol)))))
      (overlay-put line-overlay 'face 'diff-hl-show-hunk-clicked-line-face)
      
      ;; diff-mode, highlight hunks boundaries
      (diff-mode)
      (highlight-regexp diff-hl-show-hunk-boundary)
      (read-only-mode 1)

      ;; put an overlay to override read-only-mode keymap
      (let ((full-overlay (make-overlay 1 (1+ (buffer-size)))))
        (overlay-put full-overlay 'keymap diff-hl-show-hunk--posframe-transient-mode-map))
      

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
        (goto-char (overlay-start line-overlay)))
      

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
    (define-key map (kbd "<prior>") #'diff-hl-show-hunk--popup-pageup)
    (define-key map (kbd "M-v") #'diff-hl-show-hunk--popup-pageup)
    (define-key map (kbd "<next>") #'diff-hl-show-hunk--popup-pagedown)
    (define-key map (kbd "C-v") #'diff-hl-show-hunk--popup-pagedown)
    (define-key map (kbd "<up>") #'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "C-p") #'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<down>") #'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "C-n") #'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "C-g") #'diff-hl-show-hunk--popup-hide)
    (define-key map [escape] #'diff-hl-show-hunk--popup-hide)
    (define-key map (kbd "q") #'diff-hl-show-hunk--popup-hide)
    ;;http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
    (define-key map (kbd "<mouse-4>") #'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<wheel-up>") #'diff-hl-show-hunk--popup-up)
    (define-key map (kbd "<mouse-5>") #'diff-hl-show-hunk--popup-down)
    (define-key map (kbd "<wheel-down>") #'diff-hl-show-hunk--popup-down)
    
    map)
  "Keymap for command `diff-hl-show-hunk--popup-transient-mode'.
Capture all the vertical movement of the point, and converts it
to scroll in the popup")


(defun diff-hl-show-hunk--posframe-hide ()
  (interactive)
  (diff-hl-show-hunk--log "diff-hl-show-hunk--posframe-hide")
  (diff-hl-show-hunk--posframe-transient-mode -1)
  (when (frame-live-p diff-hl-show-hunk--frame)
    (make-frame-invisible diff-hl-show-hunk--frame))
  (when diff-hl-show-hunk--original-frame
    (select-frame-set-input-focus diff-hl-show-hunk--original-frame)
    (setq diff-hl-show-hunk--original-frame nil)))


(define-minor-mode diff-hl-show-hunk--popup-transient-mode
  "Temporal minor mode to control diff-hl popup."

  :global nil
  :group diff-hl-show-hunk-group
  
  (diff-hl-show-hunk--log "diff-hl-show-hunk--popup-transient-mode:%s" diff-hl-show-hunk--popup-transient-mode)
  
  (remove-hook 'post-command-hook #'diff-hl-show-hunk--popup-post-command-hook nil)
  (when diff-hl-show-hunk--popup-transient-mode
    (add-hook 'post-command-hook #'diff-hl-show-hunk--popup-post-command-hook nil)))


(defvar diff-hl-show-hunk--posframe-transient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape]    #'diff-hl-show-hunk--posframe-hide)
    (define-key map (kbd "q")   #'diff-hl-show-hunk--posframe-hide)
    (define-key map (kbd "C-g") #'diff-hl-show-hunk--posframe-hide)
    (define-key map (kbd "n")   #'ignore)
    (define-key map (kbd "p")   #'ignore)
    map)
  "Keymap for command `diff-hl-show-hunk--posframe-transient-mode'.
Capture all the vertical movement of the point, and converts it
to scroll in the posframe")


(define-minor-mode diff-hl-show-hunk--posframe-transient-mode
  "Temporal minor mode to control diff-hl posframe."
  :group 'diff-hl-show-hunk-group
  :global t
  
  (diff-hl-show-hunk--log "diff-hl-show-hunk--posframe-transient-mode:%s" diff-hl-show-hunk--posframe-transient-mode)
  
  (remove-hook 'post-command-hook #'diff-hl-show-hunk--posframe-post-command-hook nil)
  (when diff-hl-show-hunk--posframe-transient-mode
    (add-hook 'post-command-hook #'diff-hl-show-hunk--posframe-post-command-hook nil)))



(defun diff-hl-show-hunk--popup-post-command-hook ()
  "Called each time the region is changed."
  (let ((allowed-command (or
                          (diff-hl-show-hunk-ignorable-command-p this-command)
                          (string-match-p "diff-hl-" (symbol-name this-command)))))
    (diff-hl-show-hunk--log "this-command:%s %s" this-command  allowed-command)
    (unless allowed-command
      (diff-hl-show-hunk--popup-hide))))


(defun diff-hl-show-hunk--posframe-post-command-hook ()
  "Called for each command while in `diff-hl-show-hunk--posframe-transient-mode."
  (let* ((allowed-command (or
                           (diff-hl-show-hunk-ignorable-command-p this-command)
                           (and (symbolp this-command) (string-match-p "diff-hl-" (symbol-name this-command)))))
         (event-in-frame (eq last-event-frame diff-hl-show-hunk--frame))
         (has-focus (and (frame-live-p diff-hl-show-hunk--frame)(functionp 'frame-focus-state) (eq (frame-focus-state diff-hl-show-hunk--frame) t)))
         (still-visible (or event-in-frame allowed-command has-focus)))
    (diff-hl-show-hunk--log "post-command-hook: this-command:%s allowed-command:%s event-in-frame:%s has-focus:%s"
             this-command allowed-command event-in-frame has-focus)
    (unless still-visible
      (diff-hl-show-hunk--posframe-hide))))



(defun diff-hl-show-hunk-popup (buffer line)
  "Implementation to show the hunk in a posframe.  BUFFER is a buffer with the hunk, and the central line should be LINE."
  
  (unless (featurep 'popup)
    (error "Required package for diff-hl-show-hunk-popup not available: popup.  Please customize diff-hl-show-hunk-function"))

  (require 'popup)
  
  (diff-hl-show-hunk--popup-hide)
  (setq diff-hl-show-hunk--hide-function #'diff-hl-show-hunk--popup-hide)
  
  
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
  (let* ((posframe-used (when (featurep 'posframe)
                          (require 'posframe)
                          (when (posframe-workable-p)
                            (diff-hl-show-hunk-posframe buffer line))))
         (popup-used (when (not posframe-used)
                       (when (featurep 'popup)
                         (diff-hl-show-hunk-popup buffer line))))
         (success (or posframe-used popup-used)))
    (when (not success)
      (warn "diff-hl-show-hunk: Please install posframe or popup, or customize diff-hl-show-hunk-function"))
    success))
                       

(defun diff-hl-show-hunk--posframe-button (text help-echo action)
  (concat
   " "
   (propertize (concat " " text " ")
               'help-echo help-echo
               'face '(:height 0.7)
               'mouse-face '(:box (:style released-button))
               'keymap (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "<header-line> <mouse-1>") action)
                         map))
   " "))


(defun diff-hl-show-hunk-posframe (buffer line)
  "Implementation to show the hunk in a posframe.  BUFFER is a buffer with the hunk, and the central line should be LINE."

  (unless (featurep 'posframe)
    (error "Required package for diff-hl-show-hunk-posframe not available: posframe.  Please customize diff-hl-show-hunk-function"))

  (require 'posframe)
  (unless (posframe-workable-p)
    (error "Package posframe is not workable.  Please customize diff-hl-show-hunk-function"))

  (diff-hl-show-hunk--posframe-hide)
  (setq diff-hl-show-hunk--hide-function #'diff-hl-show-hunk--posframe-hide)

  (setq posframe-mouse-banish nil)
  (setq diff-hl-show-hunk--original-frame last-event-frame)
  (setq
   diff-hl-show-hunk--frame
   (posframe-show buffer
                  :position (point)
                  :poshandler diff-hl-show-hunk-posframe-poshandler
                  :internal-border-width diff-hl-show-hunk-posframe-internal-border-width
                  :accept-focus  t
                  ;; internal-border-color Doesn't always work, if not customize internal-border face
                  :internal-border-color diff-hl-show-hunk-posframe-internal-border-color
                  :hidehandler nil ;'diff-hl-show-hunk--hide-handler
                  :respect-header-line diff-hl-show-hunk-posframe-show-head-line
                  :respect-tab-line nil
                  :respect-mode-line nil
                  :override-parameters diff-hl-show-hunk-posframe-parameters))

  ;; Recenter arround point
  (with-selected-frame diff-hl-show-hunk--frame
    (with-current-buffer buffer
      (when diff-hl-show-hunk-posframe-show-head-line
        (setq header-line-format (concat
                                  (diff-hl-show-hunk--posframe-button
                                   "Close"
                                   "Close (\\[diff-hl-show-hunk--posframe-hide])"
                                   #'diff-hl-show-hunk--posframe-hide)
                                  (diff-hl-show-hunk--posframe-button
                                   "Previous hunk"
                                   nil
                                   (lambda ()
                                     (interactive) (diff-hl-show-hunk--posframe-hide) (diff-hl-previous-hunk) (run-with-timer 0.1 nil #'diff-hl-show-hunk)))
                                  
                                  (diff-hl-show-hunk--posframe-button
                                   "Next hunk"
                                   nil
                                   (lambda ()
                                     (interactive) (diff-hl-show-hunk--posframe-hide) (diff-hl-next-hunk) (run-with-timer 0.1 nil #'diff-hl-show-hunk)))

                                  (diff-hl-show-hunk--posframe-button
                                   "Revert hunk"
                                   nil
                                   (lambda ()
                                     (interactive) (diff-hl-show-hunk--posframe-hide) (diff-hl-revert-hunk))))))

      (goto-char (point-min))
      (forward-line (1- line))
      (setq buffer-quit-function #'diff-hl-show-hunk--posframe-hide)
      (select-window (window-main-window diff-hl-show-hunk--frame))
      (setq cursor-type 'box)
      (recenter)))
  (select-frame-set-input-focus diff-hl-show-hunk--frame)
  (diff-hl-show-hunk--posframe-transient-mode 1)
  t)

;;;###autoload
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
  :group 'diff-hl-show-hunk-group)

;;;###autoload
(define-globalized-minor-mode global-diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode
  diff-hl-show-hunk-mode)

(provide 'diff-hl-show-hunk)
;;; diff-hl-show-hunk.el ends here
