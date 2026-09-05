;;; qq-state.el --- In-memory store for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Central state for sessions, messages, contacts, and connection metadata.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-protocol)


(defvar qq-state-change-hook nil
  "Hook called with one event plist argument after state mutations.

Event plists always include `:type'.  Message-related emitters should also
include:

- `:session-key' — affected QQ session key
- `:mutation' — one of `create', `update', `delete', `read', `history',
  `session' (coarse kind; see `qq-state--emit')
- `:source' — `local', `event', `response', `notice', when known
- `:message' / `:message-anchor' / `:previous-anchor' — when a single
  message is the subject (anchor prefers NT snowflake `server-id')
- `:message-patch' — a pure ID-scoped patch when the subject is not cached
- `:observation-token' — message-patch observation clock for request windows

Chat views project canonical state after every relevant mutation; anchors and
resource identities let the shared timeline redraw only affected rows.")

(defvar qq-state--connection-status 'disconnected)
(defvar qq-state--last-heartbeat nil)
(defvar qq-state--self-info nil)
(defvar qq-state--status nil)
(defvar qq-state--sessions (make-hash-table :test #'equal))
(defvar qq-state--recent-session-keys nil
  "Canonical keys in the latest authoritative recent-contact snapshot.")
(defvar qq-state--recent-session-key-set (make-hash-table :test #'equal)
  "Set form of `qq-state--recent-session-keys' for constant-time membership.")
(defvar qq-state--messages-by-session (make-hash-table :test #'equal))
(defvar qq-state--message-patch-journal (make-hash-table :test #'equal)
  "Notice state keyed by (SESSION-KEY . MESSAGE-ANCHOR).

Recall tombstones remain authoritative for the lifetime of the state store.
Reaction patches are retained only while an older materialization request is
active.  Such a request may replay patches observed after it started, but a
future request must accept its own authoritative reaction snapshot unchanged.")
(defvar qq-state--message-observation-clock 0
  "Monotonic token for stable-anchor message patch observations.")
(defvar qq-state--materialization-request-counter 0
  "Monotonic identity counter for materialization request owners.")
(defvar qq-state--materialization-request-owners
  (make-hash-table :test #'eql)
  "Active materialization request owners keyed by their numeric identity.")
(defvar qq-state--friends-by-id (make-hash-table :test #'equal))
(defvar qq-state--known-user-names (make-hash-table :test #'equal)
  "Most recently observed UIN to display-name map, learned from ordinary
message senders.  QQ fills some GrayTip name parameters with the raw UIN,
so non-friend senders resolve through this local map before falling back to
the bare UIN.  Presentation only; never a peer identity source.")
(defvar qq-state--friend-order nil
  "Friend UINs in the authoritative snapshot order.")
(defvar qq-state--friend-categories nil
  "Authoritative ordered friend categories from Linux QQ.")
(defvar qq-state--friend-categories-loaded-p nil
  "Non-nil after an authoritative friend-category snapshot was applied.")
(defvar qq-state--groups-by-id (make-hash-table :test #'equal))
(defvar qq-state--group-order nil
  "Joined group codes in the authoritative snapshot order.")
(defvar qq-state--groups-loaded-p nil
  "Non-nil after an authoritative joined-group snapshot was applied.")
(defvar qq-state--requests nil)
(defvar qq-state--message-session-index (make-hash-table :test #'equal)
  "Map exact authored message IDs to their known session.

Service-row field 40006 is not a unique message identity and must never enter
this index.  Service rows use their canonical row key for presentation.")
(defvar qq-state--local-message-session-index (make-hash-table :test #'equal))
(defvar qq-state--message-order-counter 0)
(defvar qq-state--local-message-counter 0)
(defvar qq-state--session-summary-observation-clock 0
  "Monotonic token for root latest-message summary observations.")
(defvar qq-state--actions (make-hash-table :test #'equal)
  "Peer chat-actions by session-key (telega telega--actions counterpart).

Value is an alist of (SENDER-ID . ACTION), where SENDER-ID is a UIN string
and ACTION is an alist:

- type: symbol, currently only typing
- text: display string from the Gateway or a local fallback
- event-type: raw native event type
- expires-at: float-time auto-clear deadline
- timer: Emacs timer that clears this sender's action")

(defconst qq-state--partition-variables
  '(qq-state--connection-status
    qq-state--last-heartbeat
    qq-state--self-info
    qq-state--status
    qq-state--sessions
    qq-state--recent-session-keys
    qq-state--recent-session-key-set
    qq-state--messages-by-session
    qq-state--message-patch-journal
    qq-state--message-observation-clock
    qq-state--materialization-request-counter
    qq-state--materialization-request-owners
    qq-state--friends-by-id
    qq-state--friend-order
    qq-state--friend-categories
    qq-state--friend-categories-loaded-p
    qq-state--groups-by-id
    qq-state--group-order
    qq-state--groups-loaded-p
    qq-state--requests
    qq-state--message-session-index
    qq-state--local-message-session-index
    qq-state--message-order-counter
    qq-state--local-message-counter
    qq-state--session-summary-observation-clock
    qq-state--actions)
  "Mutable store variables isolated by one stable Gateway account ID.")

(cl-defstruct (qq-state-partition
               (:constructor qq-state-partition--create))
  "One account-owned canonical QQ state partition."
  account-id
  values)

(defvar qq-state--partitions (make-hash-table :test #'equal)
  "Canonical state partitions keyed by stable Gateway account ID.")

(defvar qq-state--active-account-id nil
  "Account whose partition is temporarily installed in store variables.")

(defun qq-state--fresh-partition-values ()
  "Return fresh initial values for one account state partition."
  `((qq-state--connection-status . disconnected)
    (qq-state--last-heartbeat)
    (qq-state--self-info)
    (qq-state--status)
    (qq-state--sessions . ,(make-hash-table :test #'equal))
    (qq-state--recent-session-keys)
    (qq-state--recent-session-key-set . ,(make-hash-table :test #'equal))
    (qq-state--messages-by-session . ,(make-hash-table :test #'equal))
    (qq-state--message-patch-journal . ,(make-hash-table :test #'equal))
    (qq-state--message-observation-clock . 0)
    (qq-state--materialization-request-counter . 0)
    (qq-state--materialization-request-owners . ,(make-hash-table :test #'eql))
    (qq-state--friends-by-id . ,(make-hash-table :test #'equal))
    (qq-state--friend-order)
    (qq-state--friend-categories)
    (qq-state--friend-categories-loaded-p)
    (qq-state--groups-by-id . ,(make-hash-table :test #'equal))
    (qq-state--group-order)
    (qq-state--groups-loaded-p)
    (qq-state--requests)
    (qq-state--message-session-index . ,(make-hash-table :test #'equal))
    (qq-state--local-message-session-index . ,(make-hash-table :test #'equal))
    (qq-state--message-order-counter . 0)
    (qq-state--local-message-counter . 0)
    (qq-state--session-summary-observation-clock . 0)
    (qq-state--actions . ,(make-hash-table :test #'equal))))

(defun qq-state--capture-values ()
  "Capture the currently installed store values without copying them."
  (mapcar (lambda (variable)
            (cons variable (symbol-value variable)))
          qq-state--partition-variables))

(defun qq-state--install-values (values)
  "Install account partition VALUES into the canonical store variables."
  (dolist (variable qq-state--partition-variables)
    (set variable (alist-get variable values)))
  values)

(defun qq-state-partition (account-id)
  "Return ACCOUNT-ID's state partition, creating it when necessary."
  (unless (and (stringp account-id) (not (string-empty-p account-id)))
    (error "qq: state partition requires a stable account ID"))
  (or (gethash account-id qq-state--partitions)
      (let ((partition
             (qq-state-partition--create
              :account-id (copy-sequence account-id)
              :values (qq-state--fresh-partition-values))))
        (puthash (copy-sequence account-id) partition qq-state--partitions)
        partition)))

(defun qq-state-partition-account-ids ()
  "Return stable account IDs that currently own canonical state."
  (let (account-ids)
    (maphash (lambda (account-id _partition)
               (push (copy-sequence account-id) account-ids))
             qq-state--partitions)
    (nreverse account-ids)))

(defun qq-state-active-account-id ()
  "Return the account whose state is installed during the current call."
  (and qq-state--active-account-id
       (copy-sequence qq-state--active-account-id)))

(defun qq-state--save-active-partition ()
  "Save installed scalar values back to the active partition."
  (when-let* ((account-id qq-state--active-account-id)
              (partition (gethash account-id qq-state--partitions)))
    (setf (qq-state-partition-values partition)
          (qq-state--capture-values))))

(defun qq-state-select-account (account-id)
  "Install ACCOUNT-ID as the synchronous UI state context.

Emacs executes commands serially.  Account-scoped buffers select their owner
before command dispatch, while asynchronous work uses
`qq-state-call-with-account' and restores this UI context afterward."
  (unless (equal account-id qq-state--active-account-id)
    (qq-state--save-active-partition)
    (qq-state--install-values
     (qq-state-partition-values (qq-state-partition account-id)))
    (setq qq-state--active-account-id (copy-sequence account-id)))
  (qq-state-partition account-id))

(defun qq-state-call-with-account (account-id function)
  "Call FUNCTION with ACCOUNT-ID's canonical state installed."
  (unless (functionp function)
    (error "qq: account state callback must be a function"))
  (if (equal account-id qq-state--active-account-id)
      (funcall function)
    (let ((previous-account-id qq-state--active-account-id)
          (previous-values (qq-state--capture-values))
          (partition (qq-state-partition account-id)))
      (qq-state--save-active-partition)
      (qq-state--install-values (qq-state-partition-values partition))
      (setq qq-state--active-account-id (copy-sequence account-id))
      (unwind-protect
          (funcall function)
        (qq-state--save-active-partition)
        (qq-state--install-values previous-values)
        (setq qq-state--active-account-id previous-account-id)))))

(defmacro qq-state-with-account (account-id &rest body)
  "Evaluate BODY against ACCOUNT-ID's canonical state partition."
  (declare (indent 1) (debug t))
  `(qq-state-call-with-account ,account-id (lambda () ,@body)))

(defun qq-state-drop-partition (account-id)
  "Reset and forget ACCOUNT-ID's canonical state partition."
  (when (gethash account-id qq-state--partitions)
    (qq-state-with-account account-id
      (qq-state-reset))
    (remhash account-id qq-state--partitions)
    (when (equal account-id qq-state--active-account-id)
      (setq qq-state--active-account-id nil)
      (qq-state--install-values (qq-state--fresh-partition-values)))
    t))

(defun qq-state--emit (type &rest plist)
  "Emit state TYPE event with extra PLIST fields.

Preferred keys (callers should populate when applicable):

`:type' (always)  event class: `message', `history', `session', `reset', …
`:mutation'       coarse change kind for views: `create', `update', `delete',
                  `read', `history', or `session'
`:session-key'    QQ session key string
`:source'         provenance: `local' | `event' | `response' | `notice'
`:message'        normalized message alist (copy)
`:message-anchor' stable timeline key (server-id or local-id)
`:previous-anchor' prior key when rekeying (pending local-id → snowflake)"
  (let ((event
         (append (list :type type)
                 (and qq-state--active-account-id
                      (list :account-id qq-state--active-account-id))
                 plist)))
    (run-hook-wrapped
     'qq-state-change-hook
     (lambda (function value)
       (condition-case error-data
           (funcall function (copy-tree value))
         (error
          (message "qq: State change hook %S failed: %s"
                   function (error-message-string error-data))))
       nil)
     event)))

(defun qq-state-message-anchor (message)
  "Return stable timeline anchor for MESSAGE.

Prefer the Gateway NT snowflake `server-id', then `local-id', then `id'."
  (and message
       (or (alist-get 'server-id message)
           (alist-get 'local-id message)
           (alist-get 'id message))))

(defun qq-state--normalize-id (value)
  "Return VALUE normalized as a string ID, or nil."
  (and value (format "%s" value)))

(defun qq-state--normalize-time (value)
  "Return VALUE normalized as an integer UNIX timestamp."
  (cond
   ((integerp value) value)
   ((floatp value) (truncate value))
   ((stringp value)
    (truncate (string-to-number value)))
   (t 0)))

(defun qq-state--normalize-reaction-count (value)
  "Return reaction count VALUE as a non-negative integer."
  (max 0
       (cond
        ((integerp value) value)
        ((numberp value) (truncate value))
        ((and (stringp value)
              (string-match-p "\\`[0-9]+\\'" value))
         (string-to-number value))
        (t 0))))

(defun qq-state--infer-reaction-emoji-type (emoji-id)
  "Infer QQ reaction type from string EMOJI-ID."
  (if (> (length (or emoji-id "")) 3) "2" "1"))


(defun qq-state-message-reactions (message)
  "Return normalized reactions stored on MESSAGE."
  (or (and (listp message) (alist-get 'reactions message)) '()))

(defun qq-state--present-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-state--first-present-string (&rest values)
  "Return the first non-empty string in VALUES, or nil."
  (seq-find #'qq-state--present-string values))



(defun qq-state--next-message-order ()
  "Return the next local message ordering number."
  (cl-incf qq-state--message-order-counter))

(defun qq-state--cancel-action-timer (action)
  "Cancel any expire timer stored on ACTION alist."
  (when-let* ((timer (and (listp action) (alist-get 'timer action))))
    (when (timerp timer)
      (cancel-timer timer))))

(defun qq-state--cancel-session-action-timers (actions)
  "Cancel timers for every ACTION in the session ACTIONS alist."
  (dolist (cell actions)
    (when (consp cell)
      (qq-state--cancel-action-timer (cdr cell)))))

(defun qq-state-reset ()
  "Reset all in-memory emacs-qq state."
  (setq qq-state--connection-status 'disconnected)
  (setq qq-state--last-heartbeat nil)
  (setq qq-state--self-info nil)
  (setq qq-state--status nil)
  (setq qq-state--requests nil)
  (setq qq-state--recent-session-keys nil)
  (clrhash qq-state--recent-session-key-set)
  (setq qq-state--friend-order nil)
  (setq qq-state--friend-categories nil)
  (setq qq-state--friend-categories-loaded-p nil)
  (setq qq-state--group-order nil)
  (setq qq-state--groups-loaded-p nil)
  (setq qq-state--message-order-counter 0)
  (setq qq-state--local-message-counter 0)
  (setq qq-state--session-summary-observation-clock 0)
  (maphash (lambda (_key actions)
             (qq-state--cancel-session-action-timers actions))
           qq-state--actions)
  (clrhash qq-state--actions)
  (clrhash qq-state--sessions)
  (clrhash qq-state--messages-by-session)
  (clrhash qq-state--message-patch-journal)
  (clrhash qq-state--materialization-request-owners)
  (clrhash qq-state--friends-by-id)
  (clrhash qq-state--known-user-names)
  (clrhash qq-state--groups-by-id)
  (clrhash qq-state--message-session-index)
  (clrhash qq-state--local-message-session-index)
  (qq-state--emit 'reset))

(defun qq-state-connection-status ()
  "Return current transport connection status symbol."
  qq-state--connection-status)

(defun qq-state-session-summary-observation-start ()
  "Return a freshness token for a latest-message summary observation.

Asynchronous summary callers capture this before dispatch.  Live events
allocate at receipt, so a response that began earlier cannot replace a later
observation of the same or a newer message.  This clock is deliberately
independent of unread state ownership."
  (cl-incf qq-state--session-summary-observation-clock))

(defun qq-state-set-connection-status (status)
  "Set current transport STATUS symbol."
  (unless (eq qq-state--connection-status status)
    (setq qq-state--connection-status status)
    (qq-state--emit 'connection :status status))
  qq-state--connection-status)



(defun qq-state-self-info ()
  "Return current self info object."
  (copy-tree qq-state--self-info))

(defun qq-state-set-self-info (info)
  "Store self INFO object."
  (setq qq-state--self-info (copy-tree info))
  (qq-state--emit 'self-info :self-info (qq-state-self-info))
  qq-state--self-info)

(defun qq-state-status ()
  "Return current status object."
  (copy-tree qq-state--status))


(defun qq-state-self-user-id ()
  "Return the current canonical QQ user UIN, or nil."
  (let ((value (alist-get 'user_id qq-state--self-info)))
    (cond
     ((null value) nil)
     ((qq-protocol-user-uin-p value) value)
     (t (error "qq: self identity is not a canonical uint64 UIN: %S" value)))))





(defun qq-state--canonical-peer-uid (value context)
  "Return opaque peer UID VALUE unchanged after validating CONTEXT."
  (unless (and (stringp value) (not (string-empty-p value)))
    (error "qq: %s requires a non-empty native peer UID, got %S"
           context value))
  value)

(defun qq-state--canonical-dataline-variant (variant)
  "Return canonical DataLine VARIANT string, or signal an error."
  (pcase variant
    ((or 'desktop "desktop") "desktop")
    ((or 'mobile "mobile") "mobile")
    (_ (error "qq: dataline session requires desktop or mobile variant, got %S"
              variant))))

(defun qq-state-guild-channel-session-key (guild-id channel-id)
  "Build a canonical channel session key from GUILD-ID and CHANNEL-ID."
  (unless (qq-protocol-uint64-decimal-p guild-id)
    (error "qq: Guild identity requires a canonical uint64 string"))
  (unless (qq-protocol-uint64-decimal-p channel-id)
    (error "qq: Guild channel identity requires a canonical uint64 string"))
  (format "guild:%s:channel:%s" guild-id channel-id))

(defun qq-state-session-key (type target-id &optional variant)
  "Build a canonical session key from TYPE, TARGET-ID, and VARIANT.

Private and group identities must already be canonical uint64 text.  DataLine
keys require VARIANT to be `desktop' or `mobile'.  TARGET-ID is an opaque
native peer UID for DataLine and service sessions; it is preserved
byte-for-byte, including any colon characters."
  (pcase type
    ((or 'private "private")
     (when variant
       (error "qq: private session does not accept a variant"))
     (unless (qq-protocol-user-uin-p target-id)
       (error "qq: private session requires a canonical uint64 user UIN"))
     (format "private:%s" target-id))
    ((or 'group "group")
     (when variant
       (error "qq: group session does not accept a variant"))
     (unless (qq-protocol-group-uin-p target-id)
       (error "qq: group session requires a canonical uint64 group UIN"))
     (format "group:%s" target-id))
    ((or 'dataline "dataline")
     (format "dataline:%s:%s"
             (qq-state--canonical-dataline-variant variant)
             (qq-state--canonical-peer-uid target-id "dataline session")))
    ((or 'service "service")
     (when variant
       (error "qq: service session does not accept a variant"))
     (format "service:%s"
             (qq-state--canonical-peer-uid target-id "service session")))
    (_ (error "qq: unsupported session type %S" type))))

(defun qq-state-session-key-identity (session-key)
  "Decode canonical SESSION-KEY into its complete immutable identity.

The result contains `type', `target-id', `chat-type', `peer-uid', and
`variant'.  Opaque peer UIDs are decoded only by removing their fixed prefix;
they are never split, normalized, escaped, or reconstructed from metadata."
  (unless (stringp session-key)
    (error "qq: session key must be a string, got %S" session-key))
  (let (type target-id chat-type peer-uid variant guild-id)
    (cond
     ((string-prefix-p "private:" session-key)
      (setq type 'private
            target-id (substring session-key (length "private:"))
            chat-type "1"))
     ((string-prefix-p "group:" session-key)
      (setq type 'group
            target-id (substring session-key (length "group:"))
            chat-type "2"))
     ((string-match
       "\\`guild:\\([1-9][0-9]*\\):channel:\\([1-9][0-9]*\\)\\'"
       session-key)
      (setq type 'guild-channel
            guild-id (match-string 1 session-key)
            target-id (match-string 2 session-key)
            chat-type "4"
            peer-uid target-id
            variant nil))
     ((string-prefix-p "dataline:desktop:" session-key)
      (setq type 'dataline
            target-id (substring session-key (length "dataline:desktop:"))
            chat-type "8"
            peer-uid target-id
            variant "desktop"))
     ((string-prefix-p "dataline:mobile:" session-key)
      (setq type 'dataline
            target-id (substring session-key (length "dataline:mobile:"))
            chat-type "134"
            peer-uid target-id
            variant "mobile"))
     ((string-prefix-p "service:" session-key)
      (setq type 'service
            target-id (substring session-key (length "service:"))
            chat-type "103"
            peer-uid target-id))
     (t
      (error "qq: unsupported canonical session key %S" session-key)))
    (pcase type
      ('private
       (unless (qq-protocol-user-uin-p target-id)
         (error "qq: malformed canonical private session key %S" session-key)))
      ('group
       (unless (qq-protocol-group-uin-p target-id)
         (error "qq: malformed canonical group session key %S" session-key)))
      ('guild-channel
       (unless (and (qq-protocol-uint64-decimal-p guild-id)
                    (qq-protocol-uint64-decimal-p target-id))
         (error "qq: malformed canonical Guild channel session key %S"
                session-key)))
      ((or 'dataline 'service)
       (unless (and (stringp peer-uid) (not (string-empty-p peer-uid)))
         (error "qq: malformed canonical %s session key %S"
                type session-key))))
    `((type . ,type)
      (target-id . ,target-id)
      (chat-type . ,chat-type)
      (peer-uid . ,peer-uid)
      (variant . ,variant)
      ,@(when (eq type 'guild-channel)
          `((guild-id . ,guild-id)
            (channel-id . ,target-id))))))

(defun qq-state-session-key-type (session-key)
  "Return session type symbol extracted from SESSION-KEY."
  (alist-get 'type (qq-state-session-key-identity session-key)))

(defun qq-state-session-sendable-p (session-key)
  "Return non-nil when SESSION-KEY supports outbound messages."
  (condition-case nil
      (let* ((identity (qq-state-session-key-identity session-key))
             (type (alist-get 'type identity)))
        (pcase type
          ((or 'private 'group 'dataline) t)
          (_ nil)))
    (error nil)))

(defun qq-state-session-key-target-id (session-key)
  "Return target id extracted from SESSION-KEY."
  (alist-get 'target-id (qq-state-session-key-identity session-key)))

(defun qq-state--cached-private-title (target-id)
  "Return best cached title for private TARGET-ID."
  (let ((friend (gethash (qq-state--normalize-id target-id) qq-state--friends-by-id)))
    (or (and (listp friend)
             (let ((remark (alist-get 'remark friend))
                   (nickname (alist-get 'nickname friend)))
               (cond
                ((and (stringp remark) (not (string-empty-p remark))) remark)
                ((and (stringp nickname) (not (string-empty-p nickname))) nickname)
                (t nil))))
        (qq-state--normalize-id target-id))))

(defun qq-state--cached-group-title (target-id)
  "Return best cached title for group TARGET-ID."
  (let ((group (gethash (qq-state--normalize-id target-id) qq-state--groups-by-id)))
    (or (and (listp group)
             (let ((name (alist-get 'group_name group)))
               (and (stringp name)
                    (not (string-empty-p name))
                    name)))
        (qq-state--normalize-id target-id))))


(defun qq-state--default-session-title (session)
  "Return default title for SESSION using local caches."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('group
       (qq-state--cached-group-title target-id))
      ('dataline
       (or (qq-state--first-present-string
            (alist-get 'peer-name session)
            (alist-get 'remark session))
           "我的手机"))
      ('service
       (or (qq-state--first-present-string
            (alist-get 'peer-name session)
            (alist-get 'remark session))
           (qq-state--cached-private-title
            (or (alist-get 'peer-uin session) target-id))))
      (_
       (qq-state--cached-private-title target-id)))))

(defun qq-state--session-template (session-key)
  "Return base session object for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (target-id (alist-get 'target-id identity)))
    `((key . ,session-key)
      ,@identity
      (title . ,target-id)
      ;; Nil is the first-class "unknown" state. Ordinary Message Count and
      ;; the active adapter's explicitly named Badge Count are independent.
      (unread-message-count . nil)
      (unread-badge-count . nil)
      ;; Retained only for protocol-specific projections not yet migrated to
      ;; the ordinary-message/named-badge split (currently guild navigation).
      (unread-count . nil)
      (unread-at-me-message-id . nil)
      (unread-at-me-message-seq . nil)
      (unread-at-all-message-id . nil)
      (unread-at-all-message-seq . nil)
      (muted-p . nil)
      (message-notify-mode . unspecified)
      (first-unread-message-id . nil)
      (first-unread-message-seq . nil)
      (read-position-available . nil)
      (read-latest-message-id . nil)
      (last-message-time . 0)
      (last-message-preview . "")
      (last-message-sender-id . nil)
      (last-message-sender-name . nil)
      (last-message-self-p . nil)
      (last-message-summary-token . 0)
      (last-message-local-id . nil)
      (last-message-order . nil)
      (last-message-seq . nil)
      (last-message-id . nil)
      (last-message-gateway-account-id . nil)
      (last-message-status . nil)
      (oldest-message-id . nil))))

(defun qq-state--merge-alists (old new)
  "Return OLD merged with NEW by symbol key.

Values from NEW replace values in OLD."
  (let ((merged (copy-tree old)))
    (dolist (pair new)
      (setf (alist-get (car pair) merged nil nil #'eq) (cdr pair)))
    merged))

(defun qq-state-message-recalled-p (message)
  "Return non-nil when local MESSAGE has status `recalled'."
  (eq (alist-get 'status message) 'recalled))

(defun qq-state-service-message-p (message)
  "Return non-nil when MESSAGE is a service-owned timeline row."
  (and (listp message)
       (eq (alist-get 'timeline-class message) 'service)))

(defun qq-state-gray-tip-message-data (message)
  "Return MESSAGE's normalized GrayTip semantic data, or nil."
  (when (listp message)
    (when-let* ((segment
                 (seq-find
                  (lambda (candidate)
                    (equal (alist-get 'type candidate) "gray-tip"))
                  (alist-get 'segments message))))
      (alist-get 'data segment))))

(defun qq-state-gray-tip-message-p (message)
  "Return non-nil when MESSAGE contains one typed GrayTip segment."
  (and (qq-state-service-message-p message)
       (qq-state-gray-tip-message-data message)
       t))

(defun qq-state-poke-message-data (message)
  "Return typed Poke GrayTip data from MESSAGE, or nil."
  (when-let* ((data (qq-state-gray-tip-message-data message))
              ((equal (alist-get 'kind data) "poke")))
    data))

(defun qq-state--poke-display-names (data)
  "Return (ACTOR . TARGET) display names for poke DATA.

Mobile-style self wording is shared by every presentation surface: ACTOR is
\"你\" when the initiating UIN is the current account; TARGET is \"自己\" for a
self-initiated self-targeted poke and \"你\" when the target is the current
account.  Falls back to the wire names, either of which may be nil."
  (let* ((actor-id (qq-state--present-string (alist-get 'actor-id data)))
         (target-id (qq-state--present-string (alist-get 'target-id data)))
         (self-uin (qq-state-self-user-id))
         (actor-self-p (and actor-id self-uin (equal actor-id self-uin)))
         (target-self-p (and target-id self-uin (equal target-id self-uin)))
         (actor (if actor-self-p
                    "你"
                  (qq-state--present-string (alist-get 'actor-name data))))
         (target (cond
                  ((and actor-self-p target-self-p) "自己")
                  (target-self-p "你")
                  (t (qq-state--present-string
                      (alist-get 'target-name data))))))
    (cons actor target)))

(defun qq-state-poke-message-p (message)
  "Return non-nil when MESSAGE is a typed Poke GrayTip service row."
  (and (qq-state-poke-message-data message) t))


(defun qq-state--as-recalled-message (message)
  "Return MESSAGE with recalled stub fields (kept in store for optional display)."
  (qq-state--merge-alists
   message
   '((status . recalled)
     (segments . nil)
     (raw-message . "[message recalled]")
     (preview . "[message recalled]"))))

(defun qq-state-session (session-key)
  "Return session object for SESSION-KEY."
  (copy-tree (gethash session-key qq-state--sessions)))

(defun qq-state--hydrate-session (session)
  "Return SESSION with title hydrated from contact caches when possible."
  (let* ((target-id (alist-get 'target-id session))
         (title (alist-get 'title session))
         (default-title (qq-state--default-session-title session))
         (group (and (eq (alist-get 'type session) 'group)
                     (gethash target-id qq-state--groups-by-id))))
    (when (and default-title
               (or (null title)
                   (equal title target-id)
                   (string-empty-p title)))
      (setf (alist-get 'title session nil nil #'eq) default-title))
    (when-let* ((entry (and group
                            (assq 'message-notify-mode group))))
      (let ((mode (cdr entry)))
        (setf (alist-get 'message-notify-mode session nil nil #'eq) mode
              (alist-get 'muted-p session nil nil #'eq)
              (and (not (eq mode 'notify)) t))))
    session))

(defun qq-state-upsert-session (session-key fields &optional emit)
  "Insert or update SESSION-KEY with FIELDS.

When EMIT is non-nil, fire one session mutation event (`:mutation' `session')."
  (let* ((identity (qq-state-session-key-identity session-key))
         (canonical-fields
          (seq-filter
           (lambda (field)
             (or (memq (car field) '(type target-id chat-type variant))
                 (cdr field)))
           identity))
         (existing (or (copy-tree (gethash session-key qq-state--sessions))
                       (qq-state--session-template session-key)))
         (session (qq-state--merge-alists existing fields)))
    ;; The key is the sole source of routing identity.  Metadata refreshes and
    ;; message payloads may enrich display fields, but may never retarget an
    ;; existing session.
    ;; A private/group key cannot derive the kernel peer UID; retain that UID
    ;; when a native payload later supplies it as capability metadata.  All
    ;; encoded routing fields (including a nil non-DataLine variant) still
    ;; overwrite mutable payload metadata.
    (setq session (qq-state--merge-alists session canonical-fields))
    (setf (alist-get 'key session nil nil #'eq) session-key)
    (setq session (qq-state--hydrate-session session))
    (puthash session-key session qq-state--sessions)
    (when emit
      (qq-state--emit 'session
                      :session-key session-key
                      :session (copy-tree session)
                      :mutation 'session))
    session))

(defun qq-state-sessions ()
  "Return all known sessions sorted by last activity."
  (let (sessions)
    (maphash (lambda (_key session)
               (push (copy-tree session) sessions))
             qq-state--sessions)
    (sort sessions
          (lambda (left right)
            (let ((left-time (qq-state--normalize-time (alist-get 'last-message-time left)))
                  (right-time (qq-state--normalize-time (alist-get 'last-message-time right))))
              (if (/= left-time right-time)
                  (> left-time right-time)
                (string-lessp (or (alist-get 'title left) "")
                              (or (alist-get 'title right) ""))))))))

(defun qq-state--short-media-label (value &optional fallback)
  "Return a short label for media VALUE, never a full URL or CQ blob.

Prefer basename of local paths; drop http(s) URLs entirely."
  (let ((fallback (or fallback "media")))
    (cond
     ((not (stringp value)) fallback)
     ((string-empty-p (string-trim value)) fallback)
     ((string-match-p "\\`https?://" value) fallback)
     ((string-match-p "\\`\\[CQ:" value) fallback)
     ((or (file-name-absolute-p value)
          (string-match-p "/" value)
          (string-match-p "\\\\" value))
      (let ((base (file-name-nondirectory value)))
        (if (or (string-empty-p base)
                (string-match-p "\\`https?:" base))
            fallback
          (truncate-string-to-width base 28 nil nil t))))
     (t (truncate-string-to-width (string-trim value) 28 nil nil t)))))

(defun qq-state-preview-one-line (value)
  "Return VALUE as a compact single-line message preview.

Like telega's `telega-ins--one-lined', line breaks and repeated horizontal
whitespace are presentation details rather than part of a root-row preview."
  (if (stringp value)
      (string-trim
       (replace-regexp-in-string "[[:space:]\u00a0]+" " " value))
    ""))

(defun qq-state--unsupported-preview (data)
  "Return preview text for an unsupported segment DATA.

The gateway deliberately models an element it cannot fully parse as
`unsupported' so it never claims, for example, that a mention is plain text.
That is a constraint on the *model*, not an instruction to hide readable text
from the user: when the gateway kept the element's visible text in
`fallback_text', show it and mark it, rather than replacing the whole element
with a diagnostic.  The `summary' stays available as a tooltip for debugging."
  (let* ((fallback (alist-get 'fallback_text data))
         (fallback (and (stringp fallback)
                        (let ((trimmed (qq-state-preview-one-line fallback)))
                          (and (not (string-empty-p trimmed)) trimmed))))
         (summary (alist-get 'summary data))
         (summary (if (stringp summary)
                      (replace-regexp-in-string "[\n\r\t ]+" " " summary)
                    "unknown element")))
    (if fallback
        (propertize (concat fallback " ⁇")
                    'help-echo (format "unsupported QQ element: %s" summary))
      (format "[unsupported QQ element: %s]"
              (truncate-string-to-width summary 80 nil nil t)))))

(defun qq-state-message-preview-from-segments (segments)
  "Return a human-readable plain-text preview for message SEGMENTS.

Never emit legacy CQ wire strings.  Reply segments are omitted (shown via
reply chrome elsewhere).  Media becomes short placeholders like
`[image]' / `[face:178]'."
  (qq-state-preview-one-line
   (mapconcat
    (lambda (segment)
      (let ((type (alist-get 'type segment))
            (data (alist-get 'data segment)))
        (pcase type
          ("text" (or (alist-get 'text data) ""))
          ("at" (concat "@" (or (alist-get 'name data)
                                (alist-get 'qq data)
                                "mention")))
          ;; Reply chrome is rendered separately in chatbuf / composer.
          ("reply" "")
          ("__unsupported" (qq-state--unsupported-preview data))
          ;; Native gateway app card.  Display only: never follow an Ark
          ;; action or open a URL from this segment.
          ("light_app"
           (or (qq-state--present-string (alist-get 'prompt data))
               (let ((app (qq-state--present-string (alist-get 'app data))))
                 (if (equal app "com.tencent.multimsg")
                     "[聊天记录]"
                   (format "[card:%s]" (or app "app"))))))
          ;; Native gateway group file.  `file_size' is a decimal string.
          ("group_file"
           (format "[file:%s]"
                   (qq-state--short-media-label
                    (alist-get 'file_name data) "file")))
          ("face"
           (let* ((raw (alist-get 'raw data))
                  (text (or (alist-get 'description data)
                            (alist-get 'faceText data)
                            (alist-get 'face_text data)
                            (and (listp raw) (alist-get 'faceText raw))
                            (and (listp raw) (alist-get 'face_text raw))))
                  (id (or (alist-get 'id data)
                          (alist-get 'faceIndex data)
                          (and (listp raw) (alist-get 'faceIndex raw))))
                  (named (and id
                              (fboundp 'qq-media-face-text-fallback)
                              (qq-media-face-text-fallback id))))
             (cond
              ((and (stringp text) (not (string-empty-p text))) text)
              ((and (stringp named) (not (string-empty-p named))) named)
              (id (format "[face:%s]" id))
              (t "[face]"))))
          ;; Compact previews stay telega-short; chat body uses media cards.
          ("image" "[image]")
          ((or "mface" "favorite_emoji") "[sticker]")
          ("file"
           (format "[file:%s]"
                   (qq-state--short-media-label
                    (or (alist-get 'file_name data)
                        (alist-get 'name data)
                        (alist-get 'file data))
                    "file")))
          ("record" "[voice]")
          ("video" "[video]")
          ("mail"
           (or (qq-state--present-string (alist-get 'prompt data))
               (let ((parts (delq nil
                                  (list (qq-state--present-string
                                         (alist-get 'sender data))
                                        (qq-state--present-string
                                         (alist-get 'subject data))))))
                 (and parts (string-join parts ": ")))
               "[mail]"))
          ("wallet"
           (let* ((receiver (alist-get 'receiver data))
                  (sender (alist-get 'sender data))
                  (kind (alist-get 'wallet_kind data)))
             (or (qq-state--present-string (alist-get 'notice receiver))
                 (qq-state--present-string (alist-get 'title receiver))
                 (qq-state--present-string (alist-get 'notice sender))
                 (qq-state--present-string (alist-get 'title sender))
                 (pcase kind
                   ("transfer" "[转账]")
                   ("red-packet" "[QQ红包]")
                   ("password-red-packet" "[口令红包]")
                   (_ "[QQ钱包]")))))
          ("card"
           (or (qq-state--present-string (alist-get 'prompt data))
               (qq-state--present-string (alist-get 'title data))
               (qq-state--present-string (alist-get 'content data))
               "[card]"))
          ("json" "[card]")
          ("xml" "[xml]")
          ("gray-tip"
           (if (equal (alist-get 'kind data) "poke")
               (let* ((names (qq-state--poke-display-names data))
                      (actor (car names))
                      (action
                       (qq-state--present-string (alist-get 'action data)))
                      (target (cdr names))
                      (detail
                       (qq-state--present-string (alist-get 'detail data))))
                 (or (and actor action target
                          (concat actor " " action " " target detail))
                     (and actor action (concat actor " " action detail))
                     (and action target (concat action " " target detail))
                     (and action (concat action detail))
                     (and target (concat "戳了戳 " target))
                     "[poke]"))
             (or (qq-state--present-string (alist-get 'text data))
                 "QQ system notice")))
          ("dice" "[dice]")
          ("rps" "[rps]")
          ("share" "[share]")
          ("location" "[location]")
          ("music" "[music]")
          ("forward" "[forward]")
          ("node" "[forward]")
          ("markdown" (or (alist-get 'content data)
                          (alist-get 'text data)
                          "[markdown]"))
          (_ (format "[%s]" (or type "message"))))))
    (or segments '())
    " ")))

(defun qq-state--mention-kinds-from-segments (segments)
  "Return native QQ mention kinds found in SEGMENTS.

`at-me' denotes a direct mention of the current account and `at-all' denotes
QQ's @全体成员.  Ordinary mentions of another member are deliberately ignored."
  (let ((self-id (qq-state-self-user-id))
        kinds)
    (dolist (segment (or segments '()))
      (when (equal (alist-get 'type segment) "at")
        (let* ((data (alist-get 'data segment))
               (target (qq-state--normalize-id (alist-get 'qq data))))
          (cond
           ((equal target "all") (cl-pushnew 'at-all kinds))
           ((and self-id (equal target self-id))
            (cl-pushnew 'at-me kinds))))))
    (nreverse kinds)))

(defun qq-state-message-mention-kinds (message)
  "Return normalized native mention kinds for MESSAGE."
  (copy-sequence (or (alist-get 'mention-kinds message) '())))

(defun qq-state-message-mentions-self-p (message)
  "Return non-nil when MESSAGE directly mentions the current account."
  (and (memq 'at-me (qq-state-message-mention-kinds message)) t))

(defun qq-state-message-mentions-all-p (message)
  "Return non-nil when MESSAGE contains QQ's @全体成员."
  (and (memq 'at-all (qq-state-message-mention-kinds message)) t))

(defun qq-state--cq-looks-p (string)
  "Return non-nil when STRING contains a legacy CQ wire token."
  (and (stringp string)
       (string-match-p "\\[CQ:" string)))


(defun qq-state-message-preview (message)
  "Return human-readable preview text for normalized MESSAGE.

Native messages carry structured segments.  Suppress legacy CQ text rather
than parsing a second, lossy message protocol in the UI."
  (qq-state-preview-one-line
   (or (let ((from-segments
              (qq-state-message-preview-from-segments
               (alist-get 'segments message))))
         (and (stringp from-segments)
              (not (string-empty-p from-segments))
              from-segments))
       (let ((stored (alist-get 'preview message)))
         (and (stringp stored)
              (not (string-empty-p (string-trim stored)))
              (not (qq-state--cq-looks-p stored))
              stored))
       (let ((raw (alist-get 'raw-message message)))
         (and (stringp raw)
              (not (string-empty-p (string-trim raw)))
              (not (qq-state--cq-looks-p raw))
              raw))
       "")))






(defun qq-state--gray-tip-user-name (user-id explicit-name)
  "Return a GrayTip display name for USER-ID, preferring EXPLICIT-NAME.

QQ fills name parameters with the raw UIN when a poke action carries no
name, so an explicit name identical to the UIN is a placeholder, not a
display name.  The lookup then falls back to the local self/friend tables
and the recently observed sender map before showing the bare UIN."
  (let* ((user-id (qq-state--normalize-id user-id))
         (friend (and user-id (gethash user-id qq-state--friends-by-id)))
         (self-p (and user-id
                      (equal user-id (qq-state-self-user-id))))
         (explicit (and (stringp explicit-name)
                        (not (string-empty-p explicit-name))
                        (not (equal explicit-name user-id))
                        explicit-name))
         (known (and user-id (gethash user-id qq-state--known-user-names)))
         (name (or explicit
                   (and self-p
                        (qq-state--present-string
                         (alist-get 'nickname qq-state--self-info)))
                   (and friend
                        (qq-state--present-string
                         (alist-get 'remark friend)))
                   (and friend
                        (qq-state--present-string
                         (alist-get 'nickname friend)))
                   (and (stringp known)
                        (not (string-empty-p known))
                        known)
                   user-id)))
    (or name "某人")))




(defun qq-state--native-reply-data (target)
  "Map one validated native reply TARGET to timeline segment data."
  (unless (and (listp target)
               (equal (alist-get 'kind target) "native")
               (qq-protocol-uint64-decimal-p
                (alist-get 'sequence target)))
    (error "qq: native reply target is invalid"))
  `((message_seq . ,(alist-get 'sequence target))
    ,@(when (assq 'sender target)
        `((sender . ,(copy-tree (alist-get 'sender target)))))
    ,@(when (assq 'sender_name target)
        `((sender_name . ,(alist-get 'sender_name target))))
    ,@(when (assq 'sent_at target)
        `((sent_at . ,(alist-get 'sent_at target))))))





(defun qq-state--pending-message (session-key segments &optional raw-message)
  "Return a local pending message for SESSION-KEY with SEGMENTS.

RAW-MESSAGE, when non-nil, becomes the optimistic raw-message field shown in the
chat timeline and used by weak pending-message matching."
  (let* ((local-id (format "local-%d" (cl-incf qq-state--local-message-counter)))
         (time (truncate (float-time)))
         (self-name (or (qq-state--first-present-string
                         (alist-get 'nickname qq-state--self-info))
                        (qq-state-self-user-id)
                        "me"))
         (segments (copy-tree (or segments '())))
         (preview (qq-state-message-preview-from-segments segments))
         (raw-message (or raw-message preview "")))
    ;; Backquote may share constant cons cells such as `(status . pending)'
    ;; across calls.  Timeline messages are mutable projection objects, so
    ;; return an entirely owned tree before any operation enriches one.
    (copy-tree
     `((id . ,local-id)
       (local-id . ,local-id)
       (session-key . ,session-key)
       (time . ,time)
       (sender-id . ,(qq-state-self-user-id))
       (sender-name . ,self-name)
       (sender-secondary-name . nil)
       (sender-card . nil)
       (sender-nickname . ,self-name)
       (sender-remark . nil)
       (self-p . t)
       (status . pending)
       (timeline-class . authored)
       (segments . ,segments)
       (raw-message . ,raw-message)
       (preview . ,preview)
       (order . ,(qq-state--next-message-order))))))


(defun qq-state--message-sort< (left right)
  "Return non-nil when LEFT should sort before RIGHT."
  (let ((left-time (qq-state--normalize-time (alist-get 'time left)))
        (right-time (qq-state--normalize-time (alist-get 'time right))))
    (if (/= left-time right-time)
        (< left-time right-time)
      (< (or (alist-get 'order left) 0)
         (or (alist-get 'order right) 0)))))

(defun qq-state--sort-messages (messages)
  "Return MESSAGES sorted from oldest to newest."
  (sort messages #'qq-state--message-sort<))

(defun qq-state--find-message (messages predicate)
  "Return first element in MESSAGES matching PREDICATE."
  (seq-find predicate messages))

(defun qq-state--direct-message-match (messages message)
  "Return direct match in MESSAGES for normalized MESSAGE."
  (let ((server-id (alist-get 'server-id message))
        (local-id (alist-get 'local-id message))
        (id (alist-get 'id message)))
    (qq-state--find-message
     messages
     (lambda (it)
       (or (and server-id (equal (alist-get 'server-id it) server-id))
           (and local-id (equal (alist-get 'local-id it) local-id))
           ;; Some protocol domains, notably QQ Guild forum feeds, own
           ;; stable opaque row identities which are neither message
           ;; snowflakes nor optimistic local ids.
           (and id
                (null server-id)
                (null local-id)
                (null (alist-get 'server-id it))
                (null (alist-get 'local-id it))
                (equal (alist-get 'id it) id)))))))

(defun qq-state--native-message-correlation-key (message)
  "Return authored MESSAGE's transport correlation key, or nil.

The key is scoped by the caller's session and is not a public identity.  It
exists only to correlate an id-less authored history row with its live form.
Service Timeline Messages are independently addressable rows and may share a
sequence, so they must converge only by exact row identity."
  (let ((sequence (alist-get 'message-seq message))
        (random (alist-get 'native-random message)))
    (when (and (not (qq-state-service-message-p message))
               (qq-protocol-uint64-decimal-p sequence))
      (if (equal (alist-get 'message-type message) "group")
          ;; Group sequence is itself the conversation-scoped native locator
          ;; used by history, reply seek, and recall.  Some history builds omit
          ;; random, so requiring it would leave a duplicate when a live push
          ;; later supplies the exact snowflake.
          (cons 'group sequence)
        (when (and (integerp random)
                   (<= 0 random #xffffffff))
          (list 'private sequence random))))))

(defun qq-state--native-message-correlation-match
    (messages message &optional excluded)
  "Return a transport-correlated row for MESSAGE in MESSAGES, or nil.

EXCLUDED, when non-nil, is ignored.  This lets a caller detect two rows with
different projected ids but the same native transport record."
  (when-let* ((key (qq-state--native-message-correlation-key message)))
    (qq-state--find-message
     messages
     (lambda (candidate)
       (and (not (eq candidate excluded))
            (equal (qq-state--native-message-correlation-key candidate)
                   key))))))

(defun qq-state--pending-segment-signature (segment)
  "Return stable optimistic reconciliation signature for one SEGMENT.

Only fields preserved across send payloads and self websocket events are used.
Media resource ids and paths are deliberately excluded because the kernel
rewrites them during upload; equal media-only sends are reconciled FIFO by the
surrounding message matcher."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment)))
    (pcase type
      ("text"
       (let ((text (alist-get 'text data)))
         (and (stringp text) (list type text))))
      ("at"
       (when-let* ((target (qq-state--normalize-id (alist-get 'qq data))))
         (list type target)))
      ("reply"
       (when-let* ((message-id (alist-get 'id data)))
         (list type message-id)))
      ("face"
       (when-let* ((face-id (qq-state--normalize-id (alist-get 'id data))))
         (list type face-id)))
      ("image"
       (list type (and (member (alist-get 'sub_type data) '(1 "1")) t)))
      ("favorite_emoji" (list "image" t))
      ((or "mface" "video" "record" "file")
       (list type))
      (_ nil))))

(defun qq-state--pending-message-signature (message)
  "Return stable segment signature for optimistic reconciliation of MESSAGE."
  (when-let* ((segments (alist-get 'segments message))
              ((listp segments)))
    (let ((signature (mapcar #'qq-state--pending-segment-signature segments)))
      (and (not (memq nil signature)) signature))))

(defun qq-state--weak-pending-match (messages message)
  "Return weak pending match in MESSAGES for self-sent MESSAGE."
  (let ((self-p (alist-get 'self-p message))
        (raw-message (alist-get 'raw-message message))
        (signature (qq-state--pending-message-signature message))
        (time (qq-state--normalize-time (alist-get 'time message))))
    (when self-p
      (qq-state--find-message
       messages
       (lambda (it)
         (and (alist-get 'self-p it)
              (alist-get 'local-id it)
              ;; Weak matching is FIFO ownership among unresolved local rows.
              ;; A settled row is matched directly by server-id; admitting it
              ;; here lets the next equal media event overwrite the first.
              (eq (alist-get 'status it) 'pending)
              (null (alist-get 'server-id it))
              (let ((pending-signature
                     (qq-state--pending-message-signature it)))
                ;; Stable structure wins whenever both sides expose it.  Raw
                ;; preview text is only a compatibility path for unsupported
                ;; segment kinds; equal display names must not merge distinct
                ;; @targets, replies, or base faces.
                (if (and signature pending-signature)
                    (equal pending-signature signature)
                  (equal (alist-get 'raw-message it) raw-message)))
              (<= (abs (- time (qq-state--normalize-time (alist-get 'time it))))
                  qq-self-message-dedupe-window)))))))

(defun qq-state--poke-echo-match (messages message)
  "Return the nearest opposite-provenance poke echo for MESSAGE.

The Gateway may deliver the push before or after the `message.send_poke'
response.  Match exactly one local/remote pair by actor, target and time while
leaving repeated pokes as distinct timeline records."
  (when (qq-state-poke-message-p message)
    (let* ((sender-id (alist-get 'sender-id message))
           (target-id (alist-get 'target-id message))
           (time (qq-state--normalize-time (alist-get 'time message)))
           (local-p (alist-get 'local-poke-p message))
           (candidates
            (seq-filter
             (lambda (it)
               (and (qq-state-poke-message-p it)
                    (not (alist-get 'poke-echo-reconciled-p it))
                    (not (eq (and (alist-get 'local-poke-p it) t)
                             (and local-p t)))
                    (equal (alist-get 'sender-id it) sender-id)
                    (equal (alist-get 'target-id it) target-id)
                    (<= (abs (- time
                                (qq-state--normalize-time
                                 (alist-get 'time it))))
                        qq-self-message-dedupe-window)))
             messages)))
      (car
       (sort candidates
             (lambda (left right)
               (< (abs (- time
                          (qq-state--normalize-time (alist-get 'time left))))
                  (abs (- time
                          (qq-state--normalize-time
                           (alist-get 'time right)))))))))))

(defun qq-state--index-message (message)
  "Refresh lookup indexes for MESSAGE."
  (let ((server-id (alist-get 'server-id message))
        (local-id (alist-get 'local-id message))
        (session-key (alist-get 'session-key message)))
    (when (and server-id session-key
               (not (qq-state-service-message-p message)))
      (puthash server-id session-key qq-state--message-session-index))
    (when (and local-id session-key)
      (puthash local-id session-key qq-state--local-message-session-index))))

(defun qq-state--unindex-message (message)
  "Remove lookup entries currently owned by MESSAGE."
  (let ((server-id (alist-get 'server-id message))
        (local-id (alist-get 'local-id message))
        (session-key (alist-get 'session-key message)))
    (when (and server-id session-key
               (not (qq-state-service-message-p message))
               (equal (gethash server-id qq-state--message-session-index)
                      session-key))
      (remhash server-id qq-state--message-session-index))
    (when (and local-id session-key
               (equal (gethash local-id qq-state--local-message-session-index)
                      session-key))
      (remhash local-id qq-state--local-message-session-index))))


(defun qq-state--session-summary-position-compare
    (fields session &optional current-local-resolved-p)
  "Compare candidate summary FIELDS with current SESSION position.

Return -1, 0, or 1.  Compare server timestamps first and exact per-session
message sequences inside the same second.  NT message ids are identities, not
ordering keys: self and incoming messages can occupy incomparable id ranges.
Local insertion order and freshness tokens break otherwise unknown ties."
  (let ((candidate-id (alist-get 'last-message-id fields))
        (candidate-local-id (alist-get 'last-message-local-id fields))
        (candidate-seq (alist-get 'last-message-seq fields))
        (candidate-order (or (alist-get 'last-message-order fields) 0))
        (current-id (alist-get 'last-message-id session))
        (current-seq (alist-get 'last-message-seq session))
        (current-order (or (alist-get 'last-message-order session) 0))
        (candidate-time
         (qq-state--normalize-time (alist-get 'last-message-time fields)))
        (current-time
         (qq-state--normalize-time (alist-get 'last-message-time session))))
    (cond
     ((< candidate-time current-time) -1)
     ((> candidate-time current-time) 1)
     ((and (qq-protocol-uint64-decimal-p candidate-seq)
           (qq-protocol-uint64-decimal-p current-seq))
      (qq-protocol-decimal-string-compare candidate-seq current-seq))
     ;; The same canonical identity denotes the same frontier even when one
     ;; observation carries less sequence metadata than the other.
     ((and candidate-id (equal candidate-id current-id)) 0)
     ;; A promotion is the same optimistic row acquiring its server id.
     ((and (stringp current-id)
           (string-prefix-p "local-" current-id)
           (equal candidate-local-id current-id))
      0)
     ;; Once canonical storage proves that the current optimistic row acquired
     ;; a server id, its local frontier no longer shields equal-second server
     ;; messages.  The caller supplies the latest sequence/time candidate from
     ;; a cache which includes that promoted row.
     ((and current-local-resolved-p
           (qq-protocol-uint64-decimal-p candidate-id)
           (stringp current-id)
           (string-prefix-p "local-" current-id))
      1)
     ((and (stringp candidate-id) (string-prefix-p "local-" candidate-id)
           (stringp current-id) (string-prefix-p "local-" current-id))
      (cond ((< candidate-order current-order) -1)
            ((> candidate-order current-order) 1)
            (t 0)))
     ;; Equal-second server snapshots cannot displace an incomparable local
     ;; optimistic row.  Conversely, a newly inserted local row follows the
     ;; last known server message even when their integer timestamps tie.
     ((and (stringp current-id) (string-prefix-p "local-" current-id)) -1)
     ((and (stringp candidate-id) (string-prefix-p "local-" candidate-id)) 1)
     ((< candidate-order current-order) -1)
     ((> candidate-order current-order) 1)
     (t 0))))

(defun qq-state--apply-session-summary
    (session-key fields &optional observation-token low-information-p
                 current-local-resolved-p)
  "Apply latest-message summary FIELDS to SESSION-KEY when still fresh.

OBSERVATION-TOKEN belongs to the request that observed FIELDS.  When omitted,
allocate a token for a synchronous local/live observation.  A candidate must
both be no older than the accepted message frontier and own a strictly newer
observation token.  LOW-INFORMATION-P preserves structured sender facts when a
fallback describes the already accepted exact message."
  (let* ((token (or observation-token
                    (qq-state-session-summary-observation-start)))
         (session (or (gethash session-key qq-state--sessions)
                      (qq-state--session-template session-key)))
         (current-token (or (alist-get 'last-message-summary-token session) 0))
         (position
          (qq-state--session-summary-position-compare
           fields session current-local-resolved-p)))
    (unless (and (integerp token) (> token 0))
      (error "qq: session summary observation token must be positive"))
    (when (and (> token current-token) (>= position 0))
      ;; A same-position fallback can improve preview text, but it has no
      ;; authority to erase sender facts learned from a structured message.
      ;; Clearing belongs only to a proven advance to a different frontier.
      (when (and low-information-p
                 (= position 0)
                 (equal (alist-get 'last-message-id fields)
                        (alist-get 'last-message-id session)))
        (setq fields
              (assq-delete-all
               'last-message-local-id
               (assq-delete-all
                'last-message-order
                (assq-delete-all
                 'last-message-self-p
                 (assq-delete-all
                  'last-message-sender-name
                  (assq-delete-all
                   'last-message-sender-id
                   (copy-tree fields))))))))
      (qq-state-upsert-session
       session-key
       (append (copy-tree fields)
               `((last-message-summary-token . ,token)))
       nil)
      t)))

(defun qq-state--message-summary-fields (message)
  "Return root latest-message summary fields for normalized MESSAGE."
  (let ((special-p (qq-state-service-message-p message)))
    `((last-message-time
       . ,(qq-state--normalize-time (alist-get 'time message)))
      (last-message-id . ,(or (alist-get 'server-id message)
                              (alist-get 'id message)))
      (last-message-seq . ,(or (alist-get 'root-message-seq message)
                               (alist-get 'message-seq message)))
      (last-message-local-id . ,(alist-get 'local-id message))
      (last-message-order . ,(alist-get 'order message))
      (last-message-preview . ,(qq-state-message-preview message))
      (last-message-sender-id
       . ,(unless special-p
            (or (alist-get 'sender-id message)
                (alist-get 'sender-native-id message))))
      (last-message-sender-name
       . ,(unless special-p (alist-get 'sender-name message)))
      (last-message-self-p
       . ,(and (not special-p) (alist-get 'self-p message) t))
      (last-message-gateway-account-id
       . ,(alist-get 'gateway-account-id message))
      (last-message-status . ,(alist-get 'status message)))))

(defun qq-state--latest-summary-message (messages)
  "Return the newest summary candidate in normalized MESSAGES.

Do not inherit timeline list order here: canonical rendering deliberately uses
arrival order to break equal-second ties.  Root summary ownership uses server
time, then the exact per-session message sequence.  Local rows and messages
without sequence metadata fall back to explicit local insertion order."
  (let (latest)
    (dolist (message messages latest)
      (if (null latest)
          (setq latest message)
        (let* ((candidate-fields (qq-state--message-summary-fields message))
               (latest-fields (qq-state--message-summary-fields latest))
               (position
                (qq-state--session-summary-position-compare
                 candidate-fields latest-fields)))
          (when (or (> position 0)
                    (and (= position 0)
                         (> (or (alist-get 'order message) 0)
                            (or (alist-get 'order latest) 0))))
            (setq latest message)))))))

(defun qq-state--sync-session-summary (session-key &optional observation-token)
  "Sync timeline bounds and fresh latest summary for SESSION-KEY.

Keep sender metadata separate from the content preview.  Pokes and gray tips
already describe their actor or service meaning in their content, so they do
not project a root-row sender prefix.  OBSERVATION-TOKEN is captured before an
asynchronous materialization request; nil denotes a live/local observation."
  (let* ((messages (or (gethash session-key qq-state--messages-by-session) '()))
         (oldest (seq-find (lambda (it) (alist-get 'server-id it)) messages))
         (latest (qq-state--latest-summary-message messages))
         (session (gethash session-key qq-state--sessions))
         (current-id (and session (alist-get 'last-message-id session)))
         (current-local-resolved-p
          (and (stringp current-id)
               (string-prefix-p "local-" current-id)
               (seq-some
                (lambda (message)
                  (and (equal (alist-get 'local-id message) current-id)
                       (qq-protocol-uint64-decimal-p (alist-get 'server-id message))))
                messages))))
    (qq-state-upsert-session
     session-key
     `((oldest-message-id . ,(alist-get 'server-id oldest)))
     nil)
    (when latest
      (qq-state--apply-session-summary
       session-key (qq-state--message-summary-fields latest)
       observation-token nil current-local-resolved-p))))

(defun qq-state--set-session-summary (session-key latest)
  "Authoritatively set SESSION-KEY's Conversation Head to LATEST or nil."
  (let* ((messages (or (gethash session-key qq-state--messages-by-session) '()))
         (oldest (seq-find (lambda (it) (alist-get 'server-id it)) messages))
         (token (qq-state-session-summary-observation-start))
         (cleared '((last-message-time . 0)
                    (last-message-preview . "")
                    (last-message-sender-id . nil)
                    (last-message-sender-name . nil)
                    (last-message-self-p . nil)
                    (last-message-local-id . nil)
                    (last-message-order . nil)
                    (last-message-seq . nil)
                    (last-message-id . nil)
                    (last-message-gateway-account-id . nil)
                    (last-message-status . nil))))
    (qq-state-upsert-session
     session-key
     (append
      (if latest
          (qq-state--message-summary-fields latest)
        cleared)
      `((last-message-summary-token . ,token)
        (oldest-message-id . ,(alist-get 'server-id oldest))))
     nil)))


(defun qq-state-delete-local-message (session-key row-key)
  "Remove durable ROW-KEY from SESSION-KEY's local visible projection.

The Gateway owns persistence.  This reducer is idempotent and changes only
cached presentation/index state; it never treats a QQ Message ID or Sequence as
a canonical row key."
  (let* ((messages (copy-tree
                    (or (gethash session-key qq-state--messages-by-session) '())))
         (deleted
          (seq-find
           (lambda (message)
             (equal (alist-get 'canonical-row-key message) row-key))
           messages)))
    (when deleted
      (setq messages
            (seq-remove
             (lambda (message)
               (equal (alist-get 'canonical-row-key message) row-key))
             messages))
      (qq-state--unindex-message deleted)
      (puthash session-key messages qq-state--messages-by-session)
      (qq-state--emit 'message
                      :session-key session-key
                      :message (copy-tree deleted)
                      :message-anchor (qq-state-message-anchor deleted)
                      :mutation 'delete
                      :source 'local-deletion)
      deleted)))

(defun qq-state-session-messages (session-key)
  "Return cached messages for SESSION-KEY."
  (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))

(defun qq-state-session-oldest-message-id (session-key)
  "Return oldest server-backed message id for SESSION-KEY."
  (alist-get 'oldest-message-id (gethash session-key qq-state--sessions)))

(defun qq-state-insert-pending-message (session-key segments &optional raw-message)
  "Insert local pending SEGMENTS message into SESSION-KEY.

RAW-MESSAGE overrides the optimistic raw-message field when non-nil.  Return the
local message object."
  (let* ((message (qq-state--pending-message session-key segments raw-message))
         (messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '()))))
    (push message messages)
    (setq messages (qq-state--sort-messages messages))
    (puthash session-key messages qq-state--messages-by-session)
    (qq-state-upsert-session session-key nil nil)
    (qq-state--index-message message)
    (qq-state--sync-session-summary session-key)
    (qq-state--emit 'message
                    :session-key session-key
                    :message (copy-tree message)
                    :message-anchor (qq-state-message-anchor message)
                    :mutation 'create
                    :source 'local)
    message))


(defun qq-state--replace-message (messages existing replacement)
  "Return MESSAGES with EXISTING replaced by REPLACEMENT."
  (mapcar (lambda (it) (if (eq it existing) replacement it)) messages))

(defun qq-state--message-patch-journal-key (session-key message-anchor)
  "Return the journal key for stable MESSAGE-ANCHOR in SESSION-KEY."
  (cons session-key message-anchor))


(defun qq-state--next-message-observation-token ()
  "Allocate the next ID-scoped message observation token."
  (cl-incf qq-state--message-observation-clock))


(defun qq-state--materialization-request-current (owner &optional session-key)
  "Return registered OWNER when it is active and matches SESSION-KEY."
  (when (listp owner)
    (let* ((id (plist-get owner :id))
           (current (and (integerp id)
                         (gethash id
                                  qq-state--materialization-request-owners))))
      (when (and current
                 (equal current owner)
                 (or (null session-key)
                     (equal session-key (plist-get current :session-key))))
        current))))

(defun qq-state--reaction-patch-needed-p (session-key patch)
  "Return non-nil when an active SESSION-KEY owner may need PATCH."
  (let ((token (plist-get patch :observation-token))
        needed)
    (when (integerp token)
      (maphash
       (lambda (_id owner)
         (when (and (equal session-key (plist-get owner :session-key))
                    (< (plist-get owner :start-token) token))
           (setq needed t)))
       qq-state--materialization-request-owners))
    needed))



(defun qq-state--journal-message-patch
    (session-key message-anchor patch)
  "Journal closed PATCH for MESSAGE-ANCHOR in SESSION-KEY when required.

Recall is stored as a tombstone.  A reaction patch is retained only when an
active request started before its observation token."
  (let* ((key (qq-state--message-patch-journal-key
               session-key message-anchor))
         (entry (copy-tree (gethash key qq-state--message-patch-journal))))
    (pcase (plist-get patch :kind)
      ('recall
       (setq entry (plist-put entry :recalled-p t)))
      ('emoji-like
       (when (qq-state--reaction-patch-needed-p session-key patch)
         (setq entry
               (plist-put
                entry :reaction-patches
                (append (plist-get entry :reaction-patches)
                        (list (copy-tree patch)))))))
      (kind (error "qq: unsupported message patch kind %S" kind)))
    (when (or (plist-get entry :recalled-p)
              (plist-get entry :reaction-patches))
      (puthash key entry qq-state--message-patch-journal))))

(defun qq-state--message-reaction-observation-token (message)
  "Return MESSAGE's latest materialized reaction observation token."
  (let ((token (alist-get 'reaction-observation-token message)))
    (and (integerp token) token)))

(defun qq-state--apply-observed-reaction-patch (message patch)
  "Apply reaction PATCH to MESSAGE and record its observation token.

The token is a per-canonical-message watermark.  It lets a later request
response replay only deltas that have not already reached that row."
  (let ((token (plist-get patch :observation-token))
        (updated (qq-state-message-apply-patch message patch)))
    (unless (integerp token)
      (error "qq: reaction patch requires an observation token"))
    (setf (alist-get 'reaction-observation-token updated nil nil #'eq) token)
    updated))

(defun qq-state--materialize-message-patches
    (session-key message &optional owner replay-retained-reactions-p)
  "Apply journaled notice state to normalized MESSAGE in SESSION-KEY.

Recall tombstones always apply.  Reaction patches apply only when OWNER is an
active request for SESSION-KEY and they were observed after OWNER started.
When REPLAY-RETAINED-REACTIONS-P is non-nil without an owner, catch a
canonical row up through every retained delta before applying a newer live
notice.  In both cases the row's reaction observation watermark prevents a
delta from being applied twice."
  (let* ((message-anchor (qq-state-message-anchor message))
         (key (and message-anchor
                   (qq-state--message-patch-journal-key
                    session-key message-anchor)))
         (entry (and key
                     (copy-tree
                      (gethash key qq-state--message-patch-journal)))))
    (if (null entry)
        message
      (let* ((updated (copy-tree message))
             (current-owner
              (qq-state--materialization-request-current owner session-key))
             (start-token (and current-owner
                               (plist-get current-owner :start-token)))
             (replay-floor
              (cond (start-token start-token)
                    (replay-retained-reactions-p -1)))
             (applied-token
              (or (qq-state--message-reaction-observation-token updated) -1)))
        (when replay-floor
          (dolist (patch (plist-get entry :reaction-patches))
            (let ((token (plist-get patch :observation-token)))
              (when (and (integerp token)
                         (< replay-floor token)
                         (< applied-token token))
                (setq updated
                      (qq-state--apply-observed-reaction-patch updated patch))
                (setq applied-token token)))))
        (when (plist-get entry :recalled-p)
          (setq updated (qq-state--as-recalled-message updated)))
        updated))))

(defun qq-state--merge-local-segment-presentation
    (existing incoming merged)
  "Overlay safe client-local media fields from EXISTING onto MERGED.

INCOMING remains authoritative for segment kind and remote identities.  This
only preserves absolute source paths from a correlated optimistic row, keeping
local presentation independent of the authoritative opaque `media_id'."
  (let ((existing-segments (and existing (alist-get 'segments existing)))
        (incoming-segments (alist-get 'segments incoming)))
    (if (not (and (alist-get 'local-id existing)
                  (alist-get 'server-id incoming)
                  (= (length existing-segments) (length incoming-segments))
                  (cl-every
                   (lambda (pair)
                     (let ((old (car pair))
                           (new (cdr pair)))
                       (and (equal (alist-get 'type old)
                                   (alist-get 'type new))
                            (member (alist-get 'type new)
                                    '("image" "file" "video" "record")))))
                   (cl-mapcar #'cons existing-segments incoming-segments))))
        merged
      (let ((preserved (copy-tree merged)))
        (cl-mapc
         (lambda (old new)
           (let ((old-data (alist-get 'data old))
                 (new-data (alist-get 'data new)))
             (dolist (key '(file path))
               (let ((value (alist-get key old-data)))
                 (when (and (stringp value)
                            (file-name-absolute-p value)
                            (null (assq key new-data)))
                   (setf (alist-get key new-data nil nil #'eq) value))))
             (setf (alist-get 'data new nil nil #'eq) new-data)))
         existing-segments (alist-get 'segments preserved))
        preserved))))

(defun qq-state--merge-normalized-message
    (session-key message &optional summary-observation-token source)
  "Merge normalized MESSAGE into SESSION-KEY.

Unread state is deliberately not inferred from message delivery.  Only an
exact authoritative unread materialization may write the unread count and
positions, including updates caused by another logged-in client.

SOURCE may be `history'.  A history row transport-correlated with an existing
live row then enriches that row without replacing its authoritative live
message id.  Live observations perform the inverse promotion and remove a
stale correlated history row.

Return three values via `cl-values':
1. merged local message object
2. mutation symbol `create' or `update'
3. previous timeline anchor when an identity is promoted or a correlated
   duplicate is collapsed, else nil"
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (direct (qq-state--direct-message-match messages message))
         (native-correlation
          (qq-state--native-message-correlation-match
           messages message direct))
         (history-p (eq source 'history))
         (identity-match
          (cond
           ((null direct) native-correlation)
           ((not (and history-p native-correlation)) direct)
           ((alist-get 'local-id direct) direct)
           ((alist-get 'local-id native-correlation) native-correlation)
           (t direct)))
         (redundant
          (and direct native-correlation
               ;; A live observation makes its direct id authoritative.  For
               ;; history, collapse an already duplicated pair only when an
               ;; optimistic local id proves which row came from the live
               ;; send path; otherwise do not guess between two old ids.
               (or (not history-p)
                   (alist-get 'local-id direct)
                   (alist-get 'local-id native-correlation))
               (if (eq identity-match direct)
                   native-correlation
                 direct)))
         (poke-echo (and (null direct)
                         (null native-correlation)
                         (qq-state--poke-echo-match messages message)))
         (existing (or identity-match
                       poke-echo
                       (qq-state--weak-pending-match messages message)))
         ;; If the real notice won the race, the later synthetic callback must
         ;; not replace its snowflake/raw_info with a local anchor.  In the
         ;; opposite order, merge normally so the local row is promoted.
         (merged (cond
                  ((and poke-echo
                        (alist-get 'local-poke-p message)
                        (not (alist-get 'local-poke-p existing)))
                   (copy-tree existing))
                  (existing
                   (qq-state--merge-alists existing message))
                  (t message)))
         (merged
          (if (and history-p native-correlation existing)
              (let ((preserved (copy-tree merged)))
                (dolist (key '(id server-id local-id))
                  (when (assq key existing)
                    (setf (alist-get key preserved nil nil #'eq)
                          (alist-get key existing))))
                preserved)
            merged))
         (merged
          (qq-state--merge-local-segment-presentation
           existing message merged))
         (old-order (and existing (alist-get 'order existing)))
         (previous-anchor
          (or (and redundant (qq-state-message-anchor redundant))
              (and existing
                   (let ((old-anchor (qq-state-message-anchor existing))
                         (new-anchor (qq-state-message-anchor merged)))
                     (and old-anchor new-anchor
                          (not (equal old-anchor new-anchor))
                          old-anchor)))))
         (mutation (if existing 'update 'create)))
    (when poke-echo
      (setf (alist-get 'poke-echo-reconciled-p merged nil nil #'eq) t))
    (when old-order
      (setf (alist-get 'order merged nil nil #'eq) old-order))
    ;; Keep recalled once known unless the incoming payload is itself recalled;
    ;; an older history page may not contain a newer recall observation.
    (when (and existing
               (qq-state-message-recalled-p existing)
               (not (qq-state-message-recalled-p merged)))
      (setq merged (qq-state--as-recalled-message merged)))
    (setq merged (qq-state--materialize-message-patches session-key merged))
    (when redundant
      (qq-state--unindex-message redundant)
      (setq messages (delq redundant messages)))
    (if existing
        (setq messages (qq-state--replace-message messages existing merged))
      (push merged messages))
    (when (and existing previous-anchor
               (equal previous-anchor (qq-state-message-anchor existing)))
      (qq-state--unindex-message existing))
    (setq messages (qq-state--sort-messages messages))
    (puthash session-key messages qq-state--messages-by-session)
    (qq-state-upsert-session
     session-key
     (delq nil
           (list (and (alist-get 'peer-name merged)
                      (cons 'peer-name (alist-get 'peer-name merged)))
                 (and (alist-get 'peer-uid merged)
                      (cons 'peer-uid (alist-get 'peer-uid merged)))
                 (and (alist-get 'peer-uin merged)
                      (cons 'peer-uin (alist-get 'peer-uin merged)))))
     nil)
    (qq-state--index-message merged)
    (qq-state--sync-session-summary session-key summary-observation-token)
    (cl-values merged mutation previous-anchor)))









(defun qq-state-mark-pending-message-sent
    (session-key local-id message-id &optional request-owner)
  "Mark local pending message LOCAL-ID as sent with MESSAGE-ID in SESSION-KEY.

MESSAGE-ID is the permanent decimal timeline identity (`message_id' in the
protocol hard-cut): an NT snowflake for ordinary chats or a sender-local ID for
DataLine. It is stored as `server-id' and becomes the chat timeline anchor.
REQUEST-OWNER is the owner captured before dispatching the send request."
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (existing (qq-state--find-message
                    messages
                    (lambda (it)
                      (equal (alist-get 'local-id it) local-id))))
         (normalized-id
          (qq-protocol-optional-message-id message-id "send_msg response")))
    (when (and existing normalized-id)
      (let ((updated (qq-state--merge-alists
                      existing
                      `((id . ,normalized-id)
                        (server-id . ,normalized-id)
                        (status . sent)
                        (error . nil)))))
        (setq updated
              (qq-state--materialize-message-patches
               session-key updated request-owner))
        (setq messages (qq-state--replace-message messages existing updated))
        (setq messages (qq-state--sort-messages messages))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--index-message updated)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor (qq-state-message-anchor updated)
                        :previous-anchor local-id
                        :mutation 'update
                        :source 'response)
        updated))))

(defun qq-state-mark-pending-message-failed (session-key local-id reason)
  "Mark a still-pending LOCAL-ID as failed with REASON in SESSION-KEY.

Return the updated message only when a transition happened.  A server-backed
or otherwise settled row is authoritative and must not be downgraded by a late
transport timeout."
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (existing (qq-state--find-message
                    messages
                    (lambda (it)
                      (equal (alist-get 'local-id it) local-id)))))
    (when (and existing
               (eq (alist-get 'status existing) 'pending)
               (null (alist-get 'server-id existing)))
      (let ((updated (qq-state--merge-alists
                      existing
                      `((status . failed)
                        (error . ,reason)))))
        (setq messages (qq-state--replace-message messages existing updated))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor (qq-state-message-anchor updated)
                        :mutation 'update
                        :source 'response)
        updated))))

(defun qq-state-set-session-message-unread (session-key count)
  "Set SESSION-KEY ordinary Message Count and emit `:mutation' `read'.

COUNT is clamped to a non-negative integer.  Views treat this mutation as a
read-state change (header-line + optional unread divider), not a full
timeline rebuild."
  (let ((n (max 0 (if (integerp count) count (truncate (or count 0))))))
    (qq-state-upsert-session
     session-key
     `((unread-message-count . ,n)
       ,@(when (zerop n)
           '((unread-at-me-message-id . nil)
             (unread-at-me-message-seq . nil)
             (unread-at-all-message-id . nil)
             (unread-at-all-message-seq . nil))))
     nil)
    (qq-state--emit 'session
                    :session-key session-key
                    :session (qq-state-session session-key)
                    :mutation 'read)
    n))


(defconst qq-state--session-read-projection-keys
  '(unread-message-count unread-badge-count
    first-unread-message-id first-unread-message-seq
    unread-at-me-message-id unread-at-me-message-seq
    unread-at-all-message-id unread-at-all-message-seq
    read-position-available read-latest-message-id)
  "Session fields written by an authoritative unread materialization.")

(defun qq-state--session-read-projection (session)
  "Return the authoritative read projection of SESSION."
  (mapcar (lambda (key) (cons key (alist-get key session)))
          qq-state--session-read-projection-keys))

(defun qq-state--validate-session-read-projection (projection)
  "Return an isolated normalized read PROJECTION, or signal an error."
  (let ((keys (mapcar #'car projection))
        (message-count (alist-get 'unread-message-count projection))
        (badge-count (alist-get 'unread-badge-count projection)))
    (unless (and (proper-list-p projection)
                 (= (length projection)
                    (length qq-state--session-read-projection-keys))
                 (null (seq-difference
                        keys qq-state--session-read-projection-keys))
                 (= (length keys) (length (delete-dups (copy-sequence keys)))))
      (error "qq: invalid internal session read projection"))
    (dolist (count (list message-count badge-count))
      (unless (or (null count) (and (integerp count) (>= count 0)))
        (error "qq: internal unread count must be nil or non-negative")))
    (when (and message-count badge-count (< badge-count message-count))
      (error "qq: exact badge count cannot be below exact message count"))
    (dolist (key '(first-unread-message-id
                   unread-at-me-message-id unread-at-all-message-id
                   read-latest-message-id))
      (qq-protocol-optional-message-id
       (alist-get key projection) "internal read projection"))
    (dolist (key '(first-unread-message-seq
                   unread-at-me-message-seq unread-at-all-message-seq))
      (let ((sequence (alist-get key projection)))
        (unless (or (null sequence)
                    (qq-protocol-message-sequence-p sequence))
          (error "qq: internal read projection has invalid sequence"))))
    (unless (memq (alist-get 'read-position-available projection) '(nil t))
      (error "qq: internal read-position availability must be boolean"))
    (let* ((first-id (alist-get 'first-unread-message-id projection))
           (first-seq (alist-get 'first-unread-message-seq projection))
           (at-me-id (alist-get 'unread-at-me-message-id projection))
           (at-me-seq (alist-get 'unread-at-me-message-seq projection))
           (at-all-id (alist-get 'unread-at-all-message-id projection))
           (at-all-seq (alist-get 'unread-at-all-message-seq projection))
           (available (alist-get 'read-position-available projection))
           (unread-position-values
            (list first-id first-seq at-me-id at-me-seq at-all-id at-all-seq)))
      (unless (and (or (null at-me-id) at-me-seq)
                   (or (null at-all-id) at-all-seq)
                   (cond
                    ((and message-count (zerop message-count))
                     (and (not available)
                          (seq-every-p #'null unread-position-values)))
                    (t
                     (and (or (null first-id) first-seq)
                          (eq available (and first-id t))))))
        (error "qq: inconsistent internal session read projection"))))
  (copy-tree projection))

(defun qq-state-apply-session-read-projection (session-key projection)
  "Apply normalized authoritative read PROJECTION to SESSION-KEY.

PROJECTION is an internal state value, not a Gateway wire object.
Its keys are exactly `qq-state--session-read-projection-keys'. Nil message or
badge counts independently mean that component has not been materialized."
  (setq projection (qq-state--validate-session-read-projection projection))
  (let ((existing (qq-state-session session-key)))
    (unless (and existing
                 (equal (qq-state--session-read-projection existing)
                        projection))
      (qq-state-upsert-session session-key (copy-tree projection) nil)
      (qq-state--emit 'session
                      :session-key session-key
                      :session (qq-state-session session-key)
                      :mutation 'read))
    (qq-state-session session-key)))

(defun qq-state-validate-message-session (session-key message-id)
  "Reject known SESSION-KEY contradictions for exact MESSAGE-ID.

Return the validated NT snowflake string.  This cache-index check is not an
ownership proof for an unknown id; outbound mutations must carry a closed
locator-qualified message reference."
  (qq-state-session-key-identity session-key)
  (setq message-id
        (or (qq-protocol-optional-message-id
             message-id "message/session validation")
            (error "qq: message/session validation requires message_id")))
  (let ((indexed (gethash message-id qq-state--message-session-index)))
    (when (and indexed (not (equal indexed session-key)))
      (error "qq: message patch session %s contradicts indexed session %s"
             session-key indexed)))
  message-id)

(defun qq-state-apply-recall (session-key message-id)
  "Mark exact MESSAGE-ID in explicit SESSION-KEY as recalled.

Keeps the row so the chat view can hide it (default) or show a stub when
`qq-chat-show-recalled-messages' is non-nil.  The explicit session lets an
ID-only patch also update a materialized snapshot outside canonical history.
Return the updated canonical message, if one was present."
  (let* ((normalized-id
          (qq-state-validate-message-session session-key message-id))
         (messages (and session-key
                        (copy-tree (or (gethash session-key qq-state--messages-by-session) '()))))
         (existing (and messages
                        (qq-state--find-message
                         messages
                         (lambda (it)
                           (equal (alist-get 'server-id it) normalized-id)))))
         (observation-token (qq-state--next-message-observation-token))
         (patch (list :kind 'recall
                      :observation-token observation-token)))
    (cond
     ((and session-key existing)
      (let* ((materialized
              (qq-state--materialize-message-patches
               session-key existing))
             (updated (qq-state-message-apply-patch materialized patch)))
        (qq-state--journal-message-patch
         session-key normalized-id patch)
        (setq messages (qq-state--replace-message messages existing updated))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--index-message updated)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor (qq-state-message-anchor updated)
                        :mutation 'update
                        :source 'notice
                        :observation-token observation-token
                        :message-patch patch)
        updated))
     ((and session-key normalized-id)
      ;; Filter projections deliberately do not register in the canonical
      ;; message index.  Journal the patch for later canonical materialization
      ;; and publish it so the private projection updates immediately.
      (qq-state--journal-message-patch
       session-key normalized-id patch)
      (qq-state--emit 'message
                      :session-key session-key
                      :message-anchor normalized-id
                      :mutation 'update
                      :source 'notice
                      :observation-token observation-token
                      :message-patch patch)
      nil))))

(defun qq-state-apply-group-sequence-recall (session-key sequence)
  "Mark the unique authored group row at SEQUENCE as recalled.

Ordinary group recall is closed on `(group, sequence)'; Service Timeline
Messages sharing that cursor use their dedicated operations and must not be
selected accidentally.  Do not manufacture a message id from legacy `msgUid'
metadata.  If the authored row has an exact id, delegate to
`qq-state-apply-recall' so the durable id-scoped patch journal remains
available to filtered projections.

Return the updated canonical message, or nil when the authored row is absent."
  (let ((identity (qq-state-session-key-identity session-key)))
    (unless (eq (alist-get 'type identity) 'group)
      (error "qq: sequence recall requires an explicit group session"))
    (unless (qq-protocol-message-sequence-p sequence)
      (error "qq: sequence recall requires a canonical nonzero sequence string")))
  (let* ((messages
          (copy-tree
           (or (gethash session-key qq-state--messages-by-session) '())))
         (matches
          (seq-filter
           (lambda (message)
             (and (not (qq-state-service-message-p message))
                  (equal (alist-get 'message-seq message) sequence)))
           messages))
         (existing (car matches)))
    (when (cdr matches)
      (error "qq: Multiple authored group messages share one sequence"))
    (when existing
      (if-let* ((message-id (alist-get 'server-id existing)))
          (qq-state-apply-recall session-key message-id)
        (let* ((observation-token
                (qq-state--next-message-observation-token))
               (patch (list :kind 'recall
                            :observation-token observation-token))
               (materialized
                (qq-state--materialize-message-patches
                 session-key existing))
               (updated
                (qq-state-message-apply-patch materialized patch)))
          (setq messages
                (qq-state--replace-message messages existing updated))
          (puthash session-key messages qq-state--messages-by-session)
          (qq-state--index-message updated)
          (qq-state--sync-session-summary session-key)
          (qq-state--emit
           'message
           :session-key session-key
           :message (copy-tree updated)
           :message-anchor (qq-state-message-anchor updated)
           :mutation 'update
           :source 'notice
           :observation-token observation-token
           :message-patch patch)
          updated)))))

(defun qq-state--reaction-with-notice (reactions like is-add own-operation-p)
  "Return REACTIONS after applying one emoji LIKE notice.

IS-ADD identifies add versus remove.  OWN-OPERATION-P updates the local
`chosen-p' flag; reactions by other users leave that flag unchanged."
  (let* ((emoji-id (qq-state--normalize-id
                    (or (alist-get 'emoji_id like)
                        (alist-get 'emojiId like))))
         (existing (and emoji-id
                        (seq-find
                         (lambda (reaction)
                           (equal (alist-get 'emoji-id reaction) emoji-id))
                         reactions))))
    (if (null emoji-id)
        reactions
      (let* ((has-count (or (assq 'count like) (assq 'likes_cnt like)))
             (old-count (qq-state--normalize-reaction-count
                         (alist-get 'count existing)))
             (already-applied-p
              (and own-operation-p
                   existing
                   (eq (and (alist-get 'chosen-p existing) t)
                       (and is-add t))))
             (next-count
              (if has-count
                  (qq-state--normalize-reaction-count
                   (or (alist-get 'count like) (alist-get 'likes_cnt like)))
                (if already-applied-p
                    old-count
                  (max 0 (+ old-count (if is-add 1 -1))))))
             (emoji-type
              (qq-state--normalize-id
               (or (alist-get 'emoji_type like)
                   (alist-get 'emojiType like)
                   (alist-get 'emoji-type existing)
                   (qq-state--infer-reaction-emoji-type emoji-id))))
             (chosen-p (if own-operation-p
                           is-add
                         (and existing (alist-get 'chosen-p existing))))
             (next-item `((emoji-id . ,emoji-id)
                          (emoji-type . ,emoji-type)
                          (count . ,next-count)
                          (chosen-p . ,(and chosen-p t))))
             (next nil)
             (replaced nil))
        (dolist (reaction reactions)
          (if (equal (alist-get 'emoji-id reaction) emoji-id)
              (progn
                (setq replaced t)
                (when (> next-count 0)
                  (push next-item next)))
            (push reaction next)))
        (when (and (not replaced) (> next-count 0))
          (push next-item next))
        (nreverse next)))))

(defun qq-state--message-with-emoji-like-notice (message notice)
  "Return a copy of MESSAGE after applying emoji-like NOTICE."
  (let* ((is-add (qq-protocol-json-true-p (alist-get 'is_add notice)))
         (operator-id (qq-state--normalize-id (alist-get 'user_id notice)))
         (own-operation-p
          (and operator-id
               (equal operator-id (qq-state-self-user-id))))
         (reactions (copy-tree (qq-state-message-reactions message))))
    (dolist (like (or (alist-get 'likes notice) '()))
      (setq reactions
            (qq-state--reaction-with-notice
             reactions like is-add own-operation-p)))
    (let ((updated (copy-tree message)))
      (setf (alist-get 'reactions updated nil nil #'eq) reactions)
      updated)))

(defun qq-state-message-apply-patch (message patch)
  "Return a copy of normalized MESSAGE after applying closed PATCH.

This pure boundary is shared by canonical storage and buffer-owned filtered
snapshots.  PATCH is a plist whose `:kind' is `recall' or `emoji-like'."
  (unless (listp message)
    (error "qq: message patch requires a normalized message"))
  (pcase (plist-get patch :kind)
    ('recall (qq-state--as-recalled-message message))
    ('emoji-like
     (let ((notice (plist-get patch :notice)))
       (unless (listp notice)
         (error "qq: emoji-like message patch requires a notice"))
       (qq-state--message-with-emoji-like-notice message notice)))
    (kind (error "qq: unsupported message patch kind %S" kind))))

(defun qq-state-apply-emoji-like-notice
    (session-key notice &optional target-anchor)
  "Apply group emoji-like NOTICE in explicit SESSION-KEY.

The notice `count' is treated as the authoritative aggregate when present;
notices without an aggregate count are applied as a one-step delta.  The
explicit session scopes filter-owned snapshots when the message is absent from
canonical history.

TARGET-ANCHOR may identify an already matched canonical group row whose native
history snapshot legitimately omitted `message_id'.  Without it, NOTICE must
carry an exact message ID."
  (let* ((message-id
          (qq-protocol-optional-message-id
           (alist-get 'message_id notice)
           "group_msg_emoji_like notice"))
         (message-anchor
          (cond
           ((and message-id target-anchor
                 (not (equal message-id target-anchor)))
            (error "qq: emoji-like notice contradicts its target message"))
           (message-id
            (qq-state-validate-message-session session-key message-id))
           ((and (stringp target-anchor)
                 (not (string-empty-p target-anchor)))
            target-anchor)
           (t
            (error "qq: emoji-like notice requires an exact message target"))))
         (group-id (qq-state--normalize-id (alist-get 'group_id notice)))
         (identity (qq-state-session-key-identity session-key))
         (_group-session
          (unless (eq (alist-get 'type identity) 'group)
            (error "qq: emoji-like patch requires a group session")))
         (_group-id
          (unless (and group-id
                       (equal group-id (alist-get 'target-id identity)))
            (error "qq: emoji-like notice requires the explicit session group")))
         (messages (and session-key
                        (copy-tree
                         (or (gethash session-key qq-state--messages-by-session)
                             '()))))
         (existing
          (and messages
               (qq-state--find-message
                messages
                (lambda (message)
                  (equal (qq-state-message-anchor message)
                         message-anchor)))))
         (_matched-target
          (when (and (null message-id) (null existing))
            (error "qq: emoji-like notice target is not materialized")))
         (observation-token (qq-state--next-message-observation-token))
         (patch (list :kind 'emoji-like
                      :observation-token observation-token
                      :notice (copy-tree notice))))
    (cond
     ((and session-key existing)
      (let* ((materialized
              (qq-state--materialize-message-patches
               ;; Fold any older retained deltas first.  This keeps the
               ;; per-message watermark contiguous when a first notice arrived
               ;; before the live message and this newer notice arrived after.
               session-key existing nil t))
             (updated
              (qq-state--apply-observed-reaction-patch materialized patch)))
        (qq-state--journal-message-patch
         session-key message-anchor patch)
        (setq messages (qq-state--replace-message messages existing updated))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--index-message updated)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor message-anchor
                        :mutation 'update
                        :source 'notice
                        :observation-token observation-token
                        :message-patch patch)
        updated))
     ((and session-key message-id)
      (qq-state--journal-message-patch
       session-key message-anchor patch)
      (qq-state--emit 'message
                      :session-key session-key
                      :message-anchor message-anchor
                      :mutation 'update
                      :source 'notice
                      :observation-token observation-token
                      :message-patch patch)
      nil))))

(defun qq-state--closed-plist-p (value required optional)
  "Return non-nil when plist VALUE has unique REQUIRED and OPTIONAL keys."
  (and (proper-list-p value)
       (zerop (% (length value) 2))
       (let ((keys (cl-loop for (key _item) on value by #'cddr
                            collect key)))
         (and (cl-every #'keywordp keys)
              (= (length keys) (length (delete-dups (copy-sequence keys))))
              (cl-every (lambda (key) (memq key keys)) required)
              (cl-every (lambda (key) (memq key (append required optional)))
                        keys)))))

(defun qq-state--prepare-native-recent-conversation (entry)
  "Validate and isolate one normalized native recent conversation ENTRY."
  (unless (qq-state--closed-plist-p
           entry
           '(:session-key :message :activity-revision :pinned-known-p :pinned)
           nil)
    (error "qq: native recent conversation has invalid domain fields"))
  (let* ((session-key (plist-get entry :session-key))
         (identity (qq-state-session-key-identity session-key))
         (session-type (alist-get 'type identity))
         (message (copy-tree (plist-get entry :message)))
         (server-id (alist-get 'server-id message))
         (canonical-row-key (alist-get 'canonical-row-key message))
         (presentation-id (alist-get 'id message))
         (revision (plist-get entry :activity-revision))
         (pinned-known-p (plist-get entry :pinned-known-p))
         (pinned (plist-get entry :pinned)))
    (unless (memq session-type '(private group dataline))
      (error "qq: native recent conversation has unsupported session %S"
             session-key))
    (let ((message-sequence (alist-get 'message-seq message)))
      (unless (and (listp message)
                   (equal (alist-get 'session-key message) session-key)
                   (or (qq-protocol-message-id-p server-id)
                       (and (null server-id)
                            (qq-protocol-uint64-decimal-p canonical-row-key)
                            (stringp presentation-id)
                            (not (string-empty-p presentation-id))))
                   (integerp (alist-get 'time message))
                   (>= (alist-get 'time message) 0)
                   (if (eq session-type 'dataline)
                       (or (null message-sequence)
                           (qq-protocol-uint64-decimal-p message-sequence t))
                     (qq-protocol-uint64-decimal-p message-sequence t))
                   (stringp (alist-get 'gateway-account-id message))
                   (not (string-empty-p
                         (alist-get 'gateway-account-id message))))
        (error "qq: native recent conversation has malformed normalized message")))
    (unless (qq-protocol-uint64-decimal-p revision)
      (error "qq: native recent conversation revision is malformed"))
    (unless (memq pinned-known-p '(nil t))
      (error "qq: native recent conversation pin ownership is malformed"))
    (when (and pinned-known-p (not (memq pinned '(t :false))))
      (error "qq: native recent conversation pin state is malformed"))
    (when (and (eq session-type 'dataline) pinned-known-p)
      (error "qq: DataLine recent conversation cannot invent pin state"))
    (let ((peer-name (qq-state--present-string
                      (alist-get 'peer-name message))))
      (list
       :session-key session-key
       :message message
       :metadata
       `(,@(when peer-name `((title . ,peer-name)
                             (peer-name . ,peer-name)))
         ,@(when-let* ((peer-uid (alist-get 'peer-uid message)))
             `((peer-uid . ,peer-uid)))
         ,@(when-let* ((peer-uin (alist-get 'peer-uin message)))
             `((peer-uin . ,peer-uin)))
         (recent-activity-revision . ,revision)
         ,@(when pinned-known-p `((pinned . ,pinned))))))))

(defun qq-state-apply-recent-conversations
    (entries &optional summary-observation-token)
  "Atomically apply normalized native recent conversation ENTRIES.

ENTRIES contain root-summary messages prepared by the native facade.  This
operation never inserts those messages into the canonical timeline, replays a
message patch journal, or infers unread state.

The page order authoritatively replaces `qq-state-recent-session-keys'.
SUMMARY-OBSERVATION-TOKEN is captured before asynchronous dispatch, so a live
message observed later keeps ownership of a newer root summary."
  (unless (proper-list-p entries)
    (error "qq: native recent conversations must be a proper list"))
  (let* ((token (or summary-observation-token
                    (qq-state-session-summary-observation-start)))
         (prepared
          (mapcar #'qq-state--prepare-native-recent-conversation entries))
         (keys (mapcar (lambda (entry) (plist-get entry :session-key))
                       prepared))
         (unique (delete-dups (copy-sequence keys))))
    (unless (= (length keys) (length unique))
      (error "qq: native recent conversations duplicate a session"))
    ;; Stage against a shallow table copy.  Every touched session is copied by
    ;; `qq-state-upsert-session', so failure cannot mutate a shared value.
    (let ((staged-sessions (copy-hash-table qq-state--sessions)))
      (let ((qq-state--sessions staged-sessions))
        (dolist (entry prepared)
          (let ((session-key (plist-get entry :session-key))
                (message (plist-get entry :message)))
            (qq-state-upsert-session
             session-key (plist-get entry :metadata) nil)
            (qq-state--apply-session-summary
             session-key (qq-state--message-summary-fields message) token)))
        (setq staged-sessions qq-state--sessions))
      ;; Commit the staged session table and authoritative key projection as
      ;; one mutation boundary, then publish one coarse refresh notification.
      (let ((key-set (make-hash-table :test #'equal)))
        (dolist (session-key keys)
          (puthash session-key t key-set))
        (setq qq-state--sessions staged-sessions
              qq-state--recent-session-keys (copy-sequence keys)
              qq-state--recent-session-key-set key-set)))
    (qq-state--emit 'sessions-refreshed :count (length prepared)
                    :source 'response)
    (qq-state-sessions)))

(defun qq-state--bump-recent-session (session-key)
  "Record live activity for SESSION-KEY in the recent projection.

The activity source may introduce a session before its first native snapshot.
Pinned members retain their existing order; an unknown pinned member joins the
end of the pinned region.  An untruncated native snapshot later reconciles
membership and order."
  (let* ((keys qq-state--recent-session-keys)
         (session (qq-state-session session-key))
         (known-p (member session-key keys))
         (pinned-p (eq (alist-get 'pinned session) t))
         (without (cl-remove session-key (copy-sequence keys) :test #'equal))
         (insert-index
          (or (cl-position-if
               (lambda (key)
                 (not (eq (alist-get 'pinned (qq-state-session key)) t)))
               without)
              (length without)))
         (ordered
          (cond
           ((and known-p pinned-p) keys)
           (t
            (append (seq-take without insert-index)
                    (list session-key)
                    (seq-drop without insert-index))))))
    (unless (equal ordered keys)
      (let ((key-set (make-hash-table :test #'equal)))
        (dolist (key ordered)
          (puthash key t key-set))
        (setq qq-state--recent-session-keys ordered
              qq-state--recent-session-key-set key-set))
      (qq-state--emit 'recent-order :count (length ordered)
                      :source 'activity))))






(defun qq-state-recent-session-keys ()
  "Return keys from the latest authoritative recent-contact snapshot."
  (copy-sequence qq-state--recent-session-keys))

(defun qq-state-session-recent-p (session-key)
  "Return non-nil when SESSION-KEY belongs to the latest recent snapshot."
  (and (gethash session-key qq-state--recent-session-key-set) t))

(defun qq-state--refresh-session-titles ()
  "Refresh hydrated session titles from current contact caches."
  (maphash
   (lambda (session-key session)
     (let ((updated (qq-state--hydrate-session (copy-tree session))))
       (puthash session-key updated qq-state--sessions)))
   qq-state--sessions))

(defun qq-state-apply-friend-categories (categories)
  "Replace cached friends with ordered authoritative CATEGORIES.

Each category contains a `friends' list.  Both category order and friend
order are retained exactly as supplied by the native snapshot."
  (setq qq-state--friend-categories (copy-tree (or categories '())))
  (setq qq-state--friend-categories-loaded-p t)
  (setq qq-state--friend-order nil)
  (clrhash qq-state--friends-by-id)
  (dolist (category qq-state--friend-categories)
    (dolist (friend (alist-get 'friends category))
      (let ((user-id (alist-get 'user_id friend)))
        (push user-id qq-state--friend-order)
        (puthash user-id (copy-tree friend) qq-state--friends-by-id))))
  (setq qq-state--friend-order (nreverse qq-state--friend-order))
  (qq-state--refresh-session-titles)
  (qq-state--emit 'friends-refreshed
                  :count (length qq-state--friend-order)
                  :category-count (length qq-state--friend-categories))
  (qq-state-friend-categories))

(defun qq-state-apply-groups (groups)
  "Replace cached group list with GROUPS.

Known notification modes also update existing group sessions.  An omitted mode
is independently unknown and preserves the last confirmed session value; the
directory alone never creates a conversation session."
  (dolist (group (or groups '()))
    (when-let* ((entry (assq 'message-notify-mode group)))
      (unless (memq (cdr entry) '(notify assistant shield receive))
        (error "qq: group directory has an invalid notification mode"))))
  (setq qq-state--groups-loaded-p t)
  (setq qq-state--group-order nil)
  (clrhash qq-state--groups-by-id)
  (dolist (group (or groups '()))
    (let* ((group-id (qq-state--normalize-id (alist-get 'group_id group)))
           (notify-mode-entry (assq 'message-notify-mode group))
           (session-key (qq-state-session-key 'group group-id)))
      (push group-id qq-state--group-order)
      (puthash group-id (copy-tree group) qq-state--groups-by-id)
      (when-let* ((session (and notify-mode-entry
                                (gethash session-key qq-state--sessions))))
        (let* ((notify-mode (cdr notify-mode-entry))
               (muted-p (and (not (eq notify-mode 'notify)) t)))
          (unless (and (eq (alist-get 'message-notify-mode session) notify-mode)
                       (eq (alist-get 'muted-p session) muted-p))
            (qq-state-upsert-session
             session-key
             `((message-notify-mode . ,notify-mode)
               (muted-p . ,muted-p))
             t))))))
  (setq qq-state--group-order (nreverse qq-state--group-order))
  (qq-state--refresh-session-titles)
  (qq-state--emit 'groups-refreshed :count (length groups))
  groups)

(defun qq-state-friend (user-id)
  "Return cached friend object for USER-ID."
  (copy-tree (gethash (qq-state--normalize-id user-id) qq-state--friends-by-id)))

(defun qq-state-friends ()
  "Return all cached friend objects."
  (mapcar (lambda (user-id)
            (copy-tree (gethash user-id qq-state--friends-by-id)))
          qq-state--friend-order))

(defun qq-state-friend-categories ()
  "Return ordered authoritative friend categories."
  (copy-tree qq-state--friend-categories))

(defun qq-state-friend-categories-loaded-p ()
  "Return non-nil after the native friend-category snapshot has loaded."
  qq-state--friend-categories-loaded-p)

(defun qq-state-friend-count ()
  "Return the number of friends in the authoritative snapshot."
  (length qq-state--friend-order))

(defun qq-state-group (group-id)
  "Return cached group object for GROUP-ID."
  (copy-tree (gethash (qq-state--normalize-id group-id) qq-state--groups-by-id)))

(defun qq-state-groups ()
  "Return all cached group objects."
  (mapcar (lambda (group-id)
            (copy-tree (gethash group-id qq-state--groups-by-id)))
          qq-state--group-order))

(defun qq-state-groups-loaded-p ()
  "Return non-nil after the native joined-group snapshot has loaded."
  qq-state--groups-loaded-p)

(defun qq-state-group-count ()
  "Return the number of joined groups in the authoritative snapshot."
  (length qq-state--group-order))









(defun qq-state--action-live-p (action &optional now)
  "Return non-nil when ACTION has not expired relative to NOW."
  (and (listp action)
       (let ((expires-at (alist-get 'expires-at action)))
         (or (null expires-at)
             (> expires-at (or now (float-time)))))))

(defun qq-state--actions-without-timers (actions)
  "Return a copy of ACTIONS alist without internal timer objects."
  (mapcar (lambda (cell)
            (cons (car cell)
                  (assq-delete-all 'timer (copy-tree (cdr cell)))))
          actions))

(defun qq-state--prune-session-actions (session-key &optional now)
  "Drop expired actions for SESSION-KEY.

Return non-nil when anything was removed."
  (let* ((now (or now (float-time)))
         (actions (gethash session-key qq-state--actions))
         (kept nil)
         (removed nil))
    (dolist (cell actions)
      (if (qq-state--action-live-p (cdr cell) now)
          (push cell kept)
        (qq-state--cancel-action-timer (cdr cell))
        (setq removed t)))
    (cond
     ((null kept)
      (when actions
        (remhash session-key qq-state--actions)
        t))
     (removed
      (puthash session-key (nreverse kept) qq-state--actions)
      t)
     (t nil))))

(defun qq-state-session-actions (session-key)
  "Return live chat-actions alist for SESSION-KEY (telega-style).

Each element is (SENDER-ID . ACTION-ALIST).  Timers are stripped."
  (qq-state--prune-session-actions session-key)
  (qq-state--actions-without-timers
   (copy-tree (gethash session-key qq-state--actions))))

(defun qq-state-action-text (session-key)
  "Return one-line action display text for SESSION-KEY, or nil.

Mirrors telega `telega-ins--actions' first-action preference: show the
first live action's `text' (kernel status_text for QQ typing)."
  (when-let* ((actions (qq-state-session-actions session-key))
              (first (car actions))
              (text (alist-get 'text (cdr first))))
    (and (stringp text)
         (not (string-empty-p text))
         text)))

(defun qq-state-clear-session-actions (session-key &optional silent)
  "Clear all chat-actions for SESSION-KEY.

When SILENT is non-nil, do not emit a state-change event."
  (when-let* ((actions (gethash session-key qq-state--actions)))
    (qq-state--cancel-session-action-timers actions)
    (remhash session-key qq-state--actions)
    (unless silent
      (qq-state--emit 'action
                      :session-key session-key
                      :mutation 'delete
                      :source 'notice
                      :actions nil))
    t))

(defun qq-state-clear-sender-action (session-key sender-id &optional silent)
  "Clear SENDER-ID's action in SESSION-KEY (telega chatActionCancel)."
  (let* ((sender-id (qq-state--normalize-id sender-id))
         (actions (gethash session-key qq-state--actions))
         (cell (and sender-id (assoc sender-id actions))))
    (when cell
      (qq-state--cancel-action-timer (cdr cell))
      (setq actions (assoc-delete-all sender-id actions))
      (if actions
          (puthash session-key actions qq-state--actions)
        (remhash session-key qq-state--actions))
      (unless silent
        (qq-state--emit 'action
                        :session-key session-key
                        :mutation 'delete
                        :source 'notice
                        :actions (qq-state-session-actions session-key)))
      t)))


(defun qq-state-message-apply-tombstones (session-key message)
  "Apply permanent SESSION-KEY tombstones to normalized MESSAGE.

This public projection boundary applies permanent message patches to an
otherwise buffer-owned snapshot.  It does not store MESSAGE or replay
request-scoped reaction observations; recall is the only permanent patch."
  (qq-state-session-key-identity session-key)
  (unless (listp message)
    (error "qq: message tombstones require a normalized message"))
  (qq-state--materialize-message-patches session-key message))

(provide 'qq-state)

;;; qq-state.el ends here
