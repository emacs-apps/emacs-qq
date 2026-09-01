;;; qq-modes.el --- Global presentation modes for emacs-qq -*- lexical-binding: t; -*-

;;; Commentary:

;; Telega-style global mode-line status for unread QQ activity.

;;; Code:

(require 'cl-lib)
(require 'appkit-mode-line)
(require 'qq-customize)
(require 'qq-state)
(require 'qq-root)

(defvar qq-mode-line-mode)

(defvar qq-mode-line-string ""
  "Cached emacs-qq mode-line string.")

(defcustom qq-mode-line-format
  '(qq-mode-line-mode ("" qq-mode-line-string))
  "Mode-line construct installed in `mode-line-misc-info'."
  :type 'sexp
  :group 'qq-modes
  :risky t)

(defun qq-mode-line--counts ()
  "Return (UNREAD . MENTIONS) counts for current QQ sessions.

UNREAD is the exact number of messages in unmuted sessions, or nil when any
applicable Badge Count is unavailable. MENTIONS independently counts sessions
carrying an unread @self or @all marker, including muted sessions because native
QQ mentions are priority activity."
  (let ((unread 0)
        (unread-complete-p t)
        (mentions 0))
    (dolist (session (qq-state-sessions))
      (unless (qq-root--session-muted-p session)
        (let ((badge (alist-get 'unread-badge-count session)))
          (if (and (integerp badge) (>= badge 0))
              (cl-incf unread badge)
            (setq unread-complete-p nil))))
      (when (qq-root--session-mention-kinds session)
        (cl-incf mentions)))
    (cons (and unread-complete-p unread) mentions)))

(defun qq-mode-line-open-root ()
  "Open the QQ root buffer from the mode line."
  (interactive)
  (qq-root-open))

(defun qq-mode-line-open-unread ()
  "Open QQ root and move to the next unread session."
  (interactive)
  (qq-root-open)
  (qq-root-next-unread))

(defun qq-mode-line-open-mention ()
  "Open QQ root and move to a session with an unread mention."
  (interactive)
  (qq-root-open)
  (unless (qq-root--move-linewise
           1
           (lambda ()
             (when-let* ((session (qq-root--session-at-point)))
               (qq-root--session-mention-kinds session)))
           t)
    (message "qq: no unread mentions")))

(defun qq-mode-line-icon ()
  "Return clickable QQ label for the mode line."
  (appkit-mode-line-indicator
   "QQ" :face 'mode-line-emphasis
   :command #'qq-mode-line-open-root :help-echo "Open QQ"))

(defun qq-mode-line-unread-unmuted ()
  "Return mode-line text for unmuted unread messages."
  (let ((count (car (qq-mode-line--counts))))
    (when (and (integerp count) (> count 0))
      (appkit-mode-line-indicator
       (number-to-string count) :prefix " " :face 'qq-mode-line-unread
       :command #'qq-mode-line-open-unread
       :help-echo "Open unread QQ chats"))))

(defun qq-mode-line-mentions ()
  "Return mode-line text for sessions with unread native mentions."
  (let ((count (cdr (qq-mode-line--counts))))
    (unless (zerop count)
      (appkit-mode-line-indicator
       (format "@%d" count) :prefix " " :face 'qq-mode-line-mention
       :command #'qq-mode-line-open-mention
       :help-echo "Open QQ chats with unread mentions"))))

(defun qq-mode-line-update (&rest _ignored)
  "Refresh the cached QQ mode-line status."
  (when qq-mode-line-mode
    (appkit-mode-line-update-cache
     'qq-mode-line-string qq-mode-line-string-format)))

;;;###autoload
(define-minor-mode qq-mode-line-mode
  "Toggle global QQ unread and mention status in the mode line."
  :init-value nil
  :global t
  :group 'qq-modes
  (if qq-mode-line-mode
      (progn
        (appkit-mode-line-install 'qq-mode-line-format)
        (add-hook 'qq-state-change-hook #'qq-mode-line-update)
        (qq-mode-line-update)
        (force-mode-line-update t))
    (appkit-mode-line-uninstall 'qq-mode-line-format)
    (setq qq-mode-line-string "")
    (remove-hook 'qq-state-change-hook #'qq-mode-line-update)
    (force-mode-line-update t)))

(provide 'qq-modes)

;;; qq-modes.el ends here
