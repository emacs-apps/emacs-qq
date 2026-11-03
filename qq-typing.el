;;; qq-typing.el --- Native private input-state projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Input states are account-local transient observations.  The Gateway's
;; timeout is authoritative; its timestamp is an ordering token, not a clock.
;; Appkit owns expiry wakeups.  No chat history or unread state is synthesized.

;;; Code:
(require 'cl-lib)
(require 'appkit-chat-action)
(require 'qq-account)
(require 'qq-message)
(require 'qq-runtime)
(require 'qq-state)

(defun qq-typing--uint32-p (value)
  "Whether VALUE is a native unsigned 32-bit scalar."
  (and (integerp value) (<= 0 value #xffffffff)))

(defun qq-typing--check (data)
  "Validate and return the closed input-status observation in DATA."
  (let* ((status (alist-get 'input_status data))
         (conversation (alist-get 'conversation status)))
    (unless (and
             (qq-server-wire-exact-object-keys-p data '(account_id input_status))
             (qq-protocol-non-empty-string-p (alist-get 'account_id data))
             (qq-server-wire-exact-object-keys-p
              status '(conversation status timeout_seconds timestamp interval words))
             (or (qq-server-wire-exact-object-keys-p conversation '(kind peer_uid))
                 (qq-server-wire-exact-object-keys-p conversation '(kind peer_uid peer_uin)))
             (equal (alist-get 'kind conversation) "private")
             (qq-protocol-non-empty-string-p (alist-get 'peer_uid conversation))
             (or (not (assq 'peer_uin conversation))
                 (qq-protocol-user-uin-p (alist-get 'peer_uin conversation)))
             (member (alist-get 'status status) '("typing" "idle" "speaking"))
             (qq-typing--uint32-p (alist-get 'timeout_seconds status))
             (qq-typing--uint32-p (alist-get 'interval status))
             (qq-protocol-uint64-decimal-p (alist-get 'timestamp status) t)
             (stringp (alist-get 'words status)))
      (error "qq: malformed private input-status event"))
    status))

(defun qq-typing--peer-uin (state uid)
  "Return the exact cached UIN for UID in STATE, if known."
  (appkit-data-map-get
   (qq-state-store-peer-uins state)
   (qq-message--peer-key (qq-state-store-account-id state) uid)))

(defun qq-typing--put-action (state uid entry)
  "Project live ENTRY for UID when its private conversation can be resolved."
  (if-let* ((uin (qq-typing--peer-uin state uid)))
      (cons (appkit-data-with state
              ((:actions) (qq-state-session-key 'private uin) (list (cons uin entry)))
              ((:pending-input-status) uid appkit-data-delete)) t)
    (cons (appkit-data-with state ((:pending-input-status) uid entry)) nil)))

(defun qq-typing--clear-peer (state uid)
  "Clear UID's active and pending observations without losing ordering."
  (when-let* ((uin (qq-typing--peer-uin state uid)))
    (appkit-data-step state
      (qq-state-clear-session-actions state (qq-state-session-key 'private uin))))
  (cons (appkit-data-with state ((:pending-input-status) uid appkit-data-delete)) nil))

(defun qq-typing--observe (state status now)
  "Reduce checked native STATUS at local NOW, preserving previous snapshots."
  (let* ((conversation (alist-get 'conversation status))
         (uid (alist-get 'peer_uid conversation))
         (uin (alist-get 'peer_uin conversation))
         (revision (string-to-number (alist-get 'timestamp status)))
         (kind (alist-get 'status status))
         (previous (appkit-data-map-get (qq-state-store-input-status-revisions state) uid)))
    (when uin
      (appkit-data-step state
        (qq-message--remember-peer-identity state (qq-state-store-account-id state) uid uin)))
    ;; Duplicates do not extend a deadline.  At the same native timestamp a
    ;; clear wins over active states; older packets cannot resurrect typing.
    (if (and previous
             (or (< revision (car previous))
                 (and (= revision (car previous))
                      (or (equal kind (cdr previous)) (equal (cdr previous) "idle")))))
        (qq-typing--resolve-pending state now)
      (setq state (appkit-data-with state
                    ((:input-status-revisions) uid
                     (cons revision (if (= (alist-get 'timeout_seconds status) 0) "idle" kind)))))
      (if (or (equal kind "idle") (= (alist-get 'timeout_seconds status) 0))
          (qq-typing--clear-peer state uid)
        (qq-typing--put-action
         state uid
         (list :peer-uid uid :status kind :timestamp revision
               :updated-at now :expires-at (+ now (alist-get 'timeout_seconds status))
               :text (if (equal kind "speaking") "正在讲话…" "正在输入…")))))))

(defun qq-typing--expire (state now)
  "Remove expired active and unresolved observations at NOW."
  (appkit-data-map-each
   (lambda (key actions)
     (let ((live (seq-filter (lambda (entry) (appkit-chat-action-live-p (cdr entry) now)) actions)))
       (unless (equal live actions)
         (setq state (appkit-data-with state ((:actions) key (or live appkit-data-delete)))))))
   (qq-state-store-actions state))
  (appkit-data-map-each
   (lambda (uid entry)
     (unless (appkit-chat-action-live-p entry now)
       (setq state (appkit-data-with state ((:pending-input-status) uid appkit-data-delete)))))
   (qq-state-store-pending-input-status state))
  (cons state nil))

(defun qq-typing--resolve-pending (state now)
  "Resolve cached identities without extending pending observations' lifetime."
  (appkit-data-step state (qq-typing--expire state now))
  (appkit-data-map-each
   (lambda (uid entry)
     (when (qq-typing--peer-uin state uid)
       (appkit-data-step state (qq-typing--put-action state uid entry))))
   (qq-state-store-pending-input-status state))
  (cons state nil))

(defun qq-typing--clear-all (state)
  "Forget all activity and ordering information on a connection boundary."
  (cons (appkit-data-with state
          (() :actions appkit-data-map-empty
           :input-status-revisions appkit-data-map-empty
           :pending-input-status appkit-data-map-empty)) nil))

(defun qq-typing--schedule (owner)
  "Replace OWNER's expiry wakeup with the nearest live observation deadline."
  (when-let* ((runtime (qq-runtime-account owner)))
    (appkit-cancel-handle (qq-runtime-account-input-status-expiry runtime))
    (let* ((state (qq-runtime-state owner))
           (entries (appkit-data-map-values (qq-state-store-pending-input-status state)))
           (app (qq-runtime-account-app runtime)))
      (appkit-data-map-each
       (lambda (_key actions) (setq entries (append (mapcar #'cdr actions) entries)))
       (qq-state-store-actions state))
      (setf (qq-runtime-account-input-status-expiry runtime)
            (appkit-chat-action-watch-expiry
             app (appkit-chat-action-next-expiry entries)
             (lambda ()
               (qq-runtime-with-account owner
                 (qq-runtime-transact #'qq-typing--expire (float-time)))
               (qq-typing--schedule owner)))))))

(defun qq-typing--handle-event (_event data)
  "Commit an input-status event to its exact live managed account."
  (let* ((status (qq-typing--check data))
         (owner (alist-get 'account_id data))
         (account (qq-account-get owner)))
    (unless account (error "qq: input-status event belongs to an unknown account"))
    (when (and (equal (alist-get 'phase account) "online")
               (not (equal (alist-get 'uid account)
                           (alist-get 'peer_uid (alist-get 'conversation status)))))
      (qq-runtime-with-account owner
        (qq-runtime-transact #'qq-typing--observe status (float-time)))
      (qq-typing--schedule owner))))

(defun qq-typing--message-arrived (state session-key)
  "Clear private peer activity when a live message arrives in SESSION-KEY."
  (appkit-data-map-each
   (lambda (uid revision)
     (when-let* ((uin (qq-typing--peer-uin state uid)))
       (when (equal session-key (qq-state-session-key 'private uin))
         (appkit-data-step state (qq-typing--clear-peer state uid))
         (setq state (appkit-data-with state
                       ((:input-status-revisions) uid (cons (car revision) "idle")))))))
   (qq-state-store-input-status-revisions state))
  (cons state nil))

(defun qq-typing--state-changed (event)
  "Reconcile activity after committed identity or message EVENT."
  (let ((owner (plist-get event :account-id)))
    (when (qq-runtime-account owner)
      (qq-runtime-with-account owner
        (cond
         ((and (eq (plist-get event :type) 'message)
               (eq (plist-get event :source) 'event)
               (eq (plist-get event :mutation) 'create)
               (not (alist-get 'self-p (plist-get event :message))))
          (qq-runtime-transact #'qq-typing--message-arrived (plist-get event :session-key)))
         ((memq (plist-get event :type) '(friends-refreshed group-members-refreshed session))
          (qq-runtime-transact #'qq-typing--resolve-pending (float-time)))))
      (when (memq (plist-get event :type) '(action message friends-refreshed group-members-refreshed session))
        (qq-typing--schedule owner)))))

(defun qq-typing--clear-accounts (&optional owner)
  "Clear transient state in live OWNER, or every live account."
  (dolist (id (if owner (list owner) (qq-runtime-account-ids)))
    (when (qq-runtime-account id)
      (qq-runtime-with-account id (qq-runtime-transact #'qq-typing--clear-all))
      (qq-typing--schedule id))))

(defun qq-typing--connection-changed (state)
  "Forget activity when the Gateway transport leaves ready STATE."
  (unless (eq state 'ready) (qq-typing--clear-accounts)))

(defun qq-typing--accounts-changed (reason owner)
  "Discard obsolete activity on registry REASON for OWNER."
  (when (or (memq reason '(ready resync removed))
            (and owner (not (equal (alist-get 'phase (qq-account-get owner)) "online"))))
    (qq-typing--clear-accounts owner)))

(defun qq-typing--desynchronized (_body)
  "Discard activity after event-stream continuity was lost."
  (qq-typing--clear-accounts))

(qq-rpc-register-event "conversation.input_status" #'qq-typing--handle-event)
(add-hook 'qq-runtime-change-hook #'qq-typing--state-changed)
(add-hook 'qq-server-state-hook #'qq-typing--connection-changed)
(add-hook 'qq-account-registry-changed-hook #'qq-typing--accounts-changed)
(add-hook 'qq-account-desync-hook #'qq-typing--desynchronized)

(provide 'qq-typing)
;;; qq-typing.el ends here
