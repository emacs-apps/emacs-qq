;;; qq-search-preview.el --- Context previews for QQ search -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'appkit-core)
(require 'qq-chat)
(require 'qq-search)

(cl-defstruct (qq-search-preview--state
               (:constructor qq-search-preview--state-create)
               (:copier nil))
  source buffer window jump handle)

(defvar-local qq-search-preview--state nil
  "The context preview owned by this search surface.")

(defun qq-search-preview--release (state)
  "Revoke only the pending chat jump owned by preview STATE."
  (let ((source (qq-search-preview--state-source state))
        (buffer (qq-search-preview--state-buffer state)))
    (when (buffer-live-p source)
      (with-current-buffer source
        (when (eq qq-search-preview--state state)
          (setq qq-search-preview--state nil))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (qq-chat--cancel-jump (qq-search-preview--state-jump state))))))

(defun qq-search-preview-cancel (&optional promote)
  "Cancel this result view's pending preview.
When PROMOTE is non-nil, resume ordinary read tracking in its chat window.
A canceled passive preview otherwise remains passive until the user
operates its window."
  (when-let* ((state qq-search-preview--state))
    (setq qq-search-preview--state nil)
    (when promote
      (let ((window (qq-search-preview--state-window state)))
        (when (and (window-live-p window)
                   (eq (cdr-safe (window-parameter window 'qq-chat-preview))
                       state))
          (set-window-parameter window 'qq-chat-preview nil))))
    (appkit-cancel-handle (qq-search-preview--state-handle state))))

;;;###autoload
(defun qq-search-preview-hit (&optional hit)
  "Show HIT's full chat context in another window, retaining result focus."
  (interactive)
  (setq hit (qq-search--require-hit hit))
  (unless (appkit-surface-live-p qq-search--view)
    (user-error "qq: This search view is no longer available"))
  (qq-search-preview-cancel)
  (let ((source (current-buffer))
        (view qq-search--view)
        (account qq-search--account-id)
        (source-window (selected-window))
        (source-point (point))
        (source-start (window-start))
        ;; Opening an existing chat may synchronously check its scroll observer.
        (qq-auto-mark-read nil))
    (save-selected-window
      (unwind-protect
          (qq-runtime-with-account account
            (let* ((buffer (qq-chat--open-buffer (plist-get hit :session-key)))
                   (window
                    (display-buffer
                     buffer
                     '((display-buffer-reuse-window display-buffer-pop-up-window)
                       (inhibit-same-window . t))))
                   (state (qq-search-preview--state-create
                           :source source :buffer buffer :window window)))
              (unless (window-live-p window)
                (user-error "qq: Could not display a context preview"))
              (setq qq-search-preview--state state)
              (set-window-parameter window 'qq-chat-preview (cons buffer state))
              (setf (qq-search-preview--state-handle state)
                    (appkit-register-handle
                     view 'function (lambda () (qq-search-preview--release state))))
              (condition-case condition
                  (with-current-buffer buffer
                    (setf (qq-search-preview--state-jump state)
                          (qq-chat-goto-message (plist-get hit :center) t window)))
                (error
                 (qq-search-preview-cancel)
                 (signal (car condition) (cdr condition))))
              ;; A synchronous callback may have stopped the originating view.
              (unless (and (appkit-surface-live-p view)
                           (buffer-live-p source)
                           (eq (buffer-local-value 'qq-search-preview--state source)
                               state))
                (qq-search-preview--release state))))
        (when (and (window-live-p source-window)
                   (eq (window-buffer source-window) source))
          (set-window-point source-window source-point)
          (set-window-start source-window source-start t))))))

(provide 'qq-search-preview)
;;; qq-search-preview.el ends here
