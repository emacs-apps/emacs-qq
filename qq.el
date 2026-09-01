;;; qq.el --- Native QQ chat client -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: comm
;; Version: 2.0.0
;; URL: https://github.com/0WD0/emacs-qq
;; Package-Requires: ((emacs "29.1") (websocket "1.16") (transient "0.7") (appkit "0.2.19") (browser-session "0.1.0"))

;;; Commentary:

;; emacs-qq provides a native QQ client inside Emacs.
;;
;; Version 2 deliberately speaks one native protocol:
;; - connect to the long-lived native service over WebSocket
;; - select and control complete lifecycles for multiple QQ accounts
;; - browse recent sessions in an appkit-backed root buffer
;; - open chat buffers, fetch history, and exchange structured messages
;; - keep stable managed-account state updated from closed protocol events
;; - transient menus for root / chat / message / attachments

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-rpc)
(require 'qq-account)
(require 'qq-resource)
(require 'qq-favorite-emoji)
(require 'qq-remote-media)
(require 'qq-message)
(require 'qq-read)
(require 'qq-directory)
(require 'qq-profile)
(require 'qq-core)
(require 'qq-login)
(require 'qq-chat)
(require 'qq-media)
(require 'qq-forward)
(require 'qq-user)
(require 'qq-group)
(require 'qq-contacts)
(require 'qq-group-requests)
(require 'qq-root)
(require 'qq-presence)
(require 'qq-transient)
(require 'qq-notifications)
(require 'qq-modes)
(require 'qq-evil)

(defconst qq--client-major-modes
  '(qq-chat-mode
    qq-contacts-mode
    qq-forward-mode
    qq-group-mode
    qq-group-requests-mode
    qq-root-mode
    qq-user-mode)
  "Major modes whose buffers contain account-scoped QQ client data.")

(defconst qq--reset-drain-limit 128
  "Maximum reentrant runtime/buffer cleanup passes during one QQ reset.")

(defvar qq--resetting-p nil
  "Non-nil while `qq-reset-session-state' is draining account resources.")

(defun qq--foreign-live-qq-view-p (buffer)
  "Return non-nil when BUFFER belongs to another live QQ Appkit app."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (when-let* ((view (appkit-current-view)))
           (and (appkit-view-live-p view)
                (eq 'qq (appkit-app-kind (appkit-view-app view)))
                (not (eq qq-runtime--app (appkit-view-app view))))))))

(defun qq--collect-client-buffers ()
  "Return live buffers owned by the current QQ client session.

The Appkit registry finds renamed views by ownership, while the explicit
major-mode list also finds legacy QQ buffers that are not Appkit views."
  (let (buffers)
    (when (appkit-app-p qq-runtime--app)
      (maphash
       (lambda (_id view)
         (when-let* ((buffer (and (appkit-view-p view)
                                  (appkit-view-buffer view))))
           (when (and (eq qq-runtime--app (appkit-view-app view))
                      (buffer-live-p buffer)
                      (not (memq buffer buffers)))
             (push buffer buffers))))
       (appkit-app-view-registry qq-runtime--app)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (apply #'derived-mode-p qq--client-major-modes))
                 ;; A QQ major mode is only a fallback ownership signal.  A
                 ;; reciprocal live view belonging to another QQ app is an
                 ;; explicit foreign owner and must never be destroyed here.
                 (not (qq--foreign-live-qq-view-p buffer))
                 (not (memq buffer buffers)))
        (push buffer buffers)))
    (nreverse buffers)))

(defun qq--kill-client-buffer (buffer)
  "Kill account-scoped QQ BUFFER without allowing a query to retain it.

Normal kill hooks run first so legacy buffer-owned work is cancelled.  If a
broken hook signals, force the already-selected QQ buffer closed so generated
account data is not left visible."
  (when (buffer-live-p buffer)
    (condition-case error-data
        (with-current-buffer buffer
          (let ((kill-buffer-query-functions nil)
                (buffer-offer-save nil))
            (set-buffer-modified-p nil)
            (unless (kill-buffer buffer)
              (error "QQ buffer refused normal closure"))))
      (error
       (message "qq: buffer cleanup failed for %s; forcing close: %s"
                (buffer-name buffer) (error-message-string error-data))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((kill-buffer-hook nil)
                 (kill-buffer-query-functions nil)
                 (buffer-offer-save nil))
             (set-buffer-modified-p nil)
             (unless (kill-buffer buffer)
               (error "QQ buffer refused forced closure")))))))))

(defun qq--kill-client-buffers (buffers)
  "Kill every live account-scoped QQ buffer in BUFFERS."
  (dolist (buffer buffers)
    (condition-case error-data
        (qq--kill-client-buffer buffer)
      (error
       ;; Continue closing the remaining client buffers.  If Emacs refuses to
       ;; kill this one even with hooks and queries disabled, at least remove
       ;; the old account's generated contents.
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (widen)
             (erase-buffer)
             (set-buffer-modified-p nil))))
       (message "qq: could not close %s: %s"
                (if (buffer-live-p buffer) (buffer-name buffer) "QQ buffer")
                (error-message-string error-data))))))

(defun qq--stop-current-runtime-for-reset ()
  "Detach and stop current Gateway/account UI apps without losing replacements.

Registries are revoked before Appkit shutdown.  If a shutdown hook creates a
replacement runtime reentrantly, that new app remains visible to the drain
loop instead of being overwritten by a trailing unconditional assignment."
  (let ((gateway (and (appkit-app-p qq-runtime--app) qq-runtime--app))
        (account-apps
         (mapcar #'qq-runtime-account-app (qq-runtime-accounts))))
    (when gateway
      (setq qq-runtime--app nil))
    (dolist (runtime (qq-runtime-accounts))
      (remhash (qq-runtime-account-id runtime) qq-runtime--accounts))
    (dolist (app (append account-apps (and gateway (list gateway))))
      (condition-case error-data
          (appkit-app-close app)
        (error
         (message "qq: runtime cleanup failed: %s"
                  (error-message-string error-data)))
        (quit
         (message "qq: runtime cleanup was interrupted"))))
    (or gateway (car account-apps))))

(defun qq--drain-reset-resources (&optional initial-buffers)
  "Stop and kill account resources until reentrant creation quiesces.

INITIAL-BUFFERS preserves the registry snapshot taken before the first Appkit
detach.  Each pass then discovers a replacement current runtime and any new
current, detached, or legacy QQ buffers created by shutdown/kill hooks."
  (let ((pending (copy-sequence initial-buffers))
        (passes 0)
        done)
    (while (not done)
      (let ((buffers (delete-dups
                      (append pending (qq--collect-client-buffers))))
            (app (and (appkit-app-p qq-runtime--app) qq-runtime--app))
            (account-runtimes (qq-runtime-accounts)))
        (setq pending nil)
        (if (and (null app) (null account-runtimes) (null buffers))
            (setq done t)
          (cl-incf passes)
          (when (> passes qq--reset-drain-limit)
            (error "QQ reset did not quiesce after %d cleanup passes"
                   qq--reset-drain-limit))
          (when (or app account-runtimes)
            (qq--stop-current-runtime-for-reset))
          (qq--kill-client-buffers buffers))))
    (setq qq-runtime--app nil)
    t))

;;;###autoload
(defun qq ()
  "Start emacs-qq, open its root buffer, and select a QQ account.

After the service becomes ready, the selector combines managed accounts,
available EasyLogin identities, and a new-account action.  An authorization
interaction already in progress remains visible instead of being replaced."
  (interactive)
  (qq-runtime-gateway-app)
  (qq-root-open-gateway)
  (unless (qq-login-active-p)
    (qq-login)))

;;;###autoload
(defun qq-connect ()
  "Connect Emacs to the native QQ service."
  (interactive)
  (qq-core-connect))

;;;###autoload
(defun qq-disconnect ()
  "Disconnect Emacs without changing any managed QQ account state."
  (interactive)
  (qq-core-disconnect))

;;;###autoload
(defun qq-refresh ()
  "Refresh runtime data from the native QQ service.

When transport is not connected yet, start it and wait for bootstrap.
When transport is already open, request a fresh snapshot immediately."
  (interactive)
  (if (qq-core-running-p)
      (qq-core-refresh)
    (progn
      (qq-connect)
      (message "qq: connecting; initial refresh will run when the service is ready"))))

;;;###autoload
(defun qq-reset-session-state ()
  "Destructively log out of the in-memory emacs-qq session.

Transport and Appkit ownership are stopped first.  Notification timers/history
and media requests/caches are then revoked before store reset hooks run, and
finally all account-scoped QQ buffers are closed.  Buffers are collected before
Appkit detaches renamed views; legacy QQ major modes are included too."
  (interactive)
  (unless qq--resetting-p
    (qq-login-cancel)
    (let ((qq--resetting-p t)
          ;; Keep notification callbacks inert for the entire QQ transaction,
          ;; not merely during each individual notification cleanup pass.
          (qq-notifications--resetting-p t)
          (buffers (qq--collect-client-buffers)))
      (unwind-protect
          (unwind-protect
              (unwind-protect
                  (qq--stop-current-runtime-for-reset)
                (qq-notifications-reset-session-state))
            (qq-media-clear-cache))
        (unwind-protect
            (progn
              (qq-core-reset-session-state)
              (qq-state-reset))
          (unwind-protect
              (qq--drain-reset-resources buffers)
            (unwind-protect
                ;; Kill/cancel hooks may try to retain notification state or
                ;; create another current runtime.  Revoke and drain once more
                ;; before the dynamic barriers are lifted.
                (qq-notifications-reset-session-state)
              (qq--drain-reset-resources))))))
    (message "qq: session state reset; client buffers closed")))

(provide 'qq)

;;; qq.el ends here
