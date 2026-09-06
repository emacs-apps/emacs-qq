;;; qq-core.el --- emacs-qq product operation boundary -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Product-facing QQ operations live here.  As in telega's TDLib operation
;; layer, callers do not select an implementation: the WebSocket connection
;; and wire protocol stay behind the `qq-account-*' adapter modules.  A local
;; disconnect only detaches Emacs; account stop and logout remain explicit
;; lifecycle commands.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-account)
(require 'qq-attachment)
(require 'qq-message)
(require 'qq-directory)
(require 'qq-favorite-emoji)
(require 'qq-profile)
(require 'qq-remote-media)
(require 'qq-resource)
(require 'qq-server)
(require 'qq-request)
(require 'qq-protocol)
(require 'qq-runtime)
(require 'qq-state)

(cl-defstruct (qq-core--read-operation
               (:constructor qq-core--read-operation-create))
  "One stable read request with an in-flight leaf and queued successor."
  owner
  request
  message
  callback
  errback
  next-message
  next-callback
  next-errback
  token)

(cl-defstruct (qq-core--bootstrap
               (:constructor qq-core--bootstrap-create))
  "One account's directory bootstrap in a Gateway instance."
  owner
  instance-id
  token
  pending)

(defvar qq-core--bootstraps (make-hash-table :test #'equal)
  "Directory bootstrap state keyed by stable account ID.")

(defvar qq-core--observed-account-phases (make-hash-table :test #'equal)
  "Last observed Native Session phase keyed by stable account ID.")

(defvar qq-core--recent-resync-contexts (make-hash-table :test #'equal)
  "One in-flight recent resync context per stable account ID.")

(defvar qq-core--recent-requests (make-hash-table :test #'equal)
  "Newest recent-session request keyed by stable account ID.")

(defvar qq-core--recent-bootstrap-instances (make-hash-table :test #'equal)
  "Gateway instance last used to bootstrap each online account's recents.")

(defvar qq-core--read-operations (make-hash-table :test #'equal)
  "Newest read intent keyed by stable account ID and session.")

(defun qq-core--read-operation-key (owner session-key)
  "Return read-operation key for OWNER and SESSION-KEY."
  (list owner session-key))

(defun qq-core--revoke-read-operations (&rest _ignored)
  "Revoke every in-flight or coalesced read intent.

Late callbacks become inert because their operation token is no longer owned.
Pending transport requests are forgotten after ownership has been revoked, so
cancellation cannot reenter the old operation."
  (let (requests)
    (maphash
     (lambda (_session-key operation)
       (when-let* ((request (qq-core--read-operation-request operation)))
         (push request requests)))
     qq-core--read-operations)
    (setq qq-core--read-operations (make-hash-table :test #'equal))
    (dolist (request requests)
      (qq-request-cancel request))))

(defun qq-core--revoke-stale-read-operations (&rest _ignored)
  "Revoke read work whose stable account slot no longer exists."
  (let (stale)
    (maphash
     (lambda (_session-key operation)
       (unless (qq-account-get
                (qq-core--read-operation-owner operation))
         (push operation stale)))
     qq-core--read-operations)
    (dolist (operation stale)
      (qq-request-cancel
       (qq-core--read-operation-request operation)))))

(defun qq-core-running-p ()
  "Return non-nil when the native service transport is active."
  (qq-server-running-p))

(defun qq-core-ready-p ()
  "Return non-nil when the native service accepts business requests."
  (qq-server-ready-p))

(defun qq-core-connect ()
  "Connect to the native service without changing account lifecycle."
  (qq-core-activate)
  (qq-server-start))

(defun qq-core-disconnect ()
  "Disconnect Emacs from the native service.

This never stops or logs out a managed QQ account."
  (qq-server-stop))

(defun qq-core--default-error (_body reason)
  "Report a native service failure described by REASON."
  (message "qq: %s" (or reason "native request failed")))

(cl-defun qq-core--start-request
    (starter callback errback
             &key (owner nil owner-supplied-p) lifecycle-owner)
  "Start asynchronous product work through STARTER.

CALLBACK and ERRBACK are product-facing leaf callbacks.  When omitted, OWNER
defaults to the current UI account; an explicitly supplied nil marks global
work.  Omitting OWNER without a selected slot is a user error.  LIFECYCLE-OWNER
optionally attaches the request to an Appkit lifecycle owner.  Return one
uniform `qq-request'."
  (qq-request-start
   starter
   :callback callback
   :errback (or errback #'qq-core--default-error)
   :owner (if owner-supplied-p
              owner
            (or (qq-runtime-current-account-id)
                (user-error "qq: Select a QQ account first")))
   :lifecycle-owner lifecycle-owner))

(defun qq-core--directory-refresh-success
    (_kind _owner callback value)
  "Deliver a completed account-scoped directory VALUE to CALLBACK."
  (when callback
    (funcall callback value)))

(defun qq-core-refresh-friend-categories
    (&optional callback errback refresh)
  "Refresh native friends and call CALLBACK with categories.

ERRBACK receives the service response and reason.  REFRESH forces the service's
contact cache."
  (let ((owner (or (qq-runtime-current-account-id)
                   (user-error "qq: Select a QQ account first"))))
    (qq-core--start-request
     (lambda (success failure)
       (qq-directory-refresh-friends success failure refresh))
     (apply-partially
      #'qq-core--directory-refresh-success 'friends owner callback)
     errback)))

(defun qq-core-refresh-joined-groups
    (&optional callback errback refresh)
  "Refresh native joined groups and call CALLBACK with them.

ERRBACK receives the service response and reason.  REFRESH forces the service's
contact cache."
  (let ((owner (or (qq-runtime-current-account-id)
                   (user-error "qq: Select a QQ account first"))))
    (qq-core--start-request
     (lambda (success failure)
       (qq-directory-refresh-groups success failure refresh))
     (apply-partially
      #'qq-core--directory-refresh-success 'groups owner callback)
     errback)))

(defun qq-core--recent-private-row-projectable-p (row account)
  "Return non-nil when C2C recent ROW can derive a UIN using ACCOUNT."
  (let* ((message (alist-get 'message (alist-get 'latest_message row)))
         (sender (alist-get 'sender message))
         (recipient (alist-get 'recipient message))
         (sender-self (qq-message--endpoint-self-p sender account))
         (recipient-self
          (qq-message--endpoint-self-p recipient account))
         (peer
          (pcase (list (and sender-self t) (and recipient-self t))
            (`(t nil) recipient)
            (`(nil t) sender)
            (`(t t) recipient)
            (_ (error "qq: recent private endpoints do not identify account")))))
    (and (qq-protocol-uint64-decimal-p (alist-get 'uin peer)) t)))

(defun qq-core--recent-row-projectable-p (row account)
  "Return non-nil when recent ROW has a product session key for ACCOUNT."
  (let ((identity (alist-get 'conversation row)))
    (pcase (alist-get 'kind identity)
      ("group" t)
      ;; C2C identities may legitimately be UID-only.  The latest message
      ;; can still provide the peer UIN needed by the private product session.
      ((or "private" "temporary")
       (qq-core--recent-private-row-projectable-p row account))
      ("dataline" t))))

(defun qq-core--recent-row-state-entry (page row account)
  "Return one state-domain entry for PAGE ROW and ACCOUNT."
  (let* ((account-id (alist-get 'account_id page))
         (identity (alist-get 'conversation row))
         (identity-kind (alist-get 'kind identity))
         (head (alist-get 'latest_message row))
         (head-kind (if (equal identity-kind "dataline")
                        "dataline"
                      "native"))
         (message
          (qq-message--history-item head head-kind "recent conversation"))
         (normalized
          (if (equal identity-kind "dataline")
              (car (qq-message--normalize-dataline-message
                    account-id message))
            (qq-message-normalize-snapshot
             message
             account-id
             account
             (alist-get 'canonical-recalled-p message))))
         (session-key (alist-get 'session-key normalized))
         (session-identity (qq-state-session-key-identity session-key)))
    (pcase identity-kind
      ((or "private" "temporary")
       (unless (and (eq (alist-get 'type session-identity) 'private)
                    (or (not (assq 'peer_uin identity))
                        (equal (alist-get 'peer_uin identity)
                               (alist-get 'target-id session-identity)))
                    (or (not (assq 'peer_uid identity))
                        (equal (alist-get 'peer_uid identity)
                               (alist-get 'peer-uid normalized))))
         (error "qq: recent C2C identity contradicts latest message")))
      ("group"
       (unless (and (eq (alist-get 'type session-identity) 'group)
                    (equal (alist-get 'group_uin identity)
                           (alist-get 'target-id session-identity)))
         (error "qq: recent group identity contradicts latest message")))
      ("dataline"
       (unless (and (eq (alist-get 'type session-identity) 'dataline)
                    (equal (alist-get 'peer_uid identity)
                           (alist-get 'peer-uid session-identity))
                    (equal (alist-get 'variant identity)
                           (alist-get 'variant session-identity)))
         (error "qq: recent DataLine identity contradicts latest message"))))
    (list
     :session-key session-key
     :message normalized
     :activity-revision (alist-get 'activity_revision row)
     :pinned-known-p (and (assq 'pinned row) t)
     :pinned (alist-get 'pinned row))))

(defun qq-core--apply-recent-page (page observation-token)
  "Normalize and apply recent PAGE for OBSERVATION-TOKEN."
  (let ((account (qq-account-get (alist-get 'account_id page))))
    (unless (and account
                 (equal (alist-get 'account_id account)
                        (alist-get 'account_id page)))
      (error "qq: recent page no longer belongs to a managed account slot"))
    (qq-state-apply-recent-conversations
     (seq-keep
      (lambda (row)
        (when (qq-core--recent-row-projectable-p row account)
          (qq-core--recent-row-state-entry page row account)))
      (alist-get 'conversations page))
     observation-token)))

(defun qq-core-refresh-recent-conversations
    (&optional callback errback limit account-id)
  "Refresh ACCOUNT-ID's native recent conversations.

This operation is slot-scoped: a restart of the same managed account does not
invalidate its result.  CALLBACK receives the resulting state session list;
ERRBACK receives a service/client error.  LIMIT defaults at the Gateway
adapter boundary.  ACCOUNT-ID defaults to the current UI account."
  (let* ((account-id (or account-id (qq-runtime-current-account-id)
                         (user-error "qq: Select a QQ account first")))
         (observation-token
          (qq-runtime-with-account account-id
            (qq-state-session-summary-observation-start)))
         (error-fn (or errback #'qq-core--default-error))
         (previous (gethash account-id qq-core--recent-requests))
         request)
    (when previous
      (qq-request-cancel previous))
    (remhash account-id qq-core--recent-requests)
    (setq request
          (qq-core--start-request
           (lambda (success failure)
             (qq-message-list-recent
              account-id
              :callback success
              :errback failure
              :limit limit))
           (lambda (page)
             (when (eq request
                       (gethash account-id qq-core--recent-requests))
               (remhash account-id qq-core--recent-requests))
             (qq-runtime-with-account account-id
               (condition-case error-data
                   (qq-rpc-invoke
                    callback
                    (qq-core--apply-recent-page page observation-token))
                 (error
                  (let ((reason (error-message-string error-data)))
                    (qq-rpc-invoke
                     error-fn
                     `((code . "client_projection_failed")
                       (message . ,reason))
                     reason))))))
           (lambda (body reason)
             (when (eq request
                       (gethash account-id qq-core--recent-requests))
               (remhash account-id qq-core--recent-requests))
             (qq-runtime-with-account account-id
               (qq-rpc-invoke error-fn body reason)))
           :owner account-id))
    (when (qq-request-active-p request)
      (puthash (copy-sequence account-id) request
               qq-core--recent-requests))
    request))

(defun qq-core--group-profile-from-state (group-id)
  "Return a group-profile projection for exact GROUP-ID, or nil."
  (when-let* ((group (qq-state-group group-id)))
    `((group_id . ,group-id)
      (name . ,(alist-get 'group_name group))
      (remark . ,(alist-get 'group_remark group))
      (description . ,(alist-get 'description group))
      (announcement . ,(alist-get 'announcement group))
      (owner_id . ,(alist-get 'owner_id group))
      (member_count . ,(alist-get 'member_count group))
      (max_member_count . ,(alist-get 'max_member_count group))
      (active_member_count . ,(alist-get 'active_member_count group))
      (created_at . ,(alist-get 'created_at group))
      (joined_at . ,(alist-get 'joined_at group))
      (pinned . ,(alist-get 'pinned group))
      (mute . ,(copy-tree (alist-get 'mute group)))
      (join . ,(copy-tree (alist-get 'join group)))
      (self_permission . ,(alist-get 'self_permission group))
      (category . ,(copy-tree (alist-get 'category group)))
      (grade . ,(alist-get 'grade group))
      (certification . ,(copy-tree (alist-get 'certification group)))
      (school . ,(copy-tree (alist-get 'school group)))
      (location . ,(copy-tree (alist-get 'location group)))
      (has_custom_avatar . ,(alist-get 'has_custom_avatar group)))))

(defun qq-core-get-user-profile (user-id callback &optional errback)
  "Fetch USER-ID's sparse native profile and call CALLBACK."
  (unless (qq-protocol-uint64-decimal-p user-id)
    (user-error "qq: user profile requires an exact decimal UIN"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-profile-get user-id success failure))
   callback errback))

(defun qq-core-get-profile-like-summary
    (user-id callback &optional errback)
  "Fetch USER-ID's native profile-like summary and call CALLBACK."
  (unless (qq-protocol-uint64-decimal-p user-id)
    (user-error "qq: profile likes require an exact decimal UIN"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-profile-get-like-summary user-id success failure))
   callback errback))

(defun qq-core-send-profile-like
    (user-id callback &optional errback)
  "Give USER-ID one native profile-card like and call CALLBACK."
  (unless (qq-protocol-uint64-decimal-p user-id)
    (user-error "qq: profile likes require an exact decimal UIN"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-profile-send-like user-id success failure))
   callback errback))

(defun qq-core-list-group-requests
    (mailbox callback &optional errback)
  "List native group requests in MAILBOX and call CALLBACK."
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-list-group-requests mailbox success failure))
   callback errback))

(defun qq-core-decide-group-request
    (mailbox request decision refusal-message callback &optional errback)
  "Apply DECISION to group REQUEST in MAILBOX and call CALLBACK."
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-decide-group-request
      mailbox request decision refusal-message success failure))
   callback errback))

(defun qq-core-get-group (group-id callback &optional errback)
  "Fetch native GROUP-ID profile and call CALLBACK.

The projection is derived from its selected-slot joined-group cache; when
absent, one authoritative group refresh is performed first."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: group profile requires an exact group UIN"))
  (if-let* ((profile (qq-core--group-profile-from-state group-id)))
      (progn
        (when callback
          (funcall callback profile))
        nil)
    (qq-core-refresh-joined-groups
     (lambda (_groups)
       (if-let* ((profile
                  (qq-core--group-profile-from-state group-id)))
           (when callback
             (funcall callback profile))
         (funcall (or errback #'qq-core--default-error)
                  nil "group is not present in this account")))
     errback t)))

(defun qq-core-refresh ()
  "Refresh primary native recent and directory data."
  (interactive)
  (let* ((owner (or (qq-runtime-current-account-id)
                    (user-error "qq: select a QQ account first")))
         (online-p (equal (alist-get 'phase (qq-account-get owner))
                          "online"))
         (contacts-p (and online-p (qq-core-supports-p 'contacts))))
    (delq nil
          (list
           (when (qq-core-supports-p 'recent-conversations)
             (qq-core-refresh-recent-conversations))
           (when contacts-p (qq-core-refresh-friend-categories))
           (when contacts-p (qq-core-refresh-joined-groups))))))

(defun qq-core--member-values (member)
  "Return non-empty locally searchable strings from MEMBER."
  (seq-filter
   (lambda (value) (and (stringp value) (not (string-empty-p value))))
   (mapcar (lambda (key) (alist-get key member))
           '(card nickname remark qid user_id))))

(defun qq-core--filter-members (members query limit)
  "Return MEMBERS matching QUERY, truncated to LIMIT."
  (let* ((needle (downcase (string-trim query)))
         (matches
          (if (string-empty-p needle)
              members
            (seq-filter
             (lambda (member)
               (seq-some
                (lambda (value)
                  (string-match-p
                   (regexp-quote needle) (downcase value)))
                (qq-core--member-values member)))
             members))))
    (copy-tree (seq-take matches limit))))

(defun qq-core-search-group-members
    (group-id query callback &optional errback limit)
  "Search native GROUP-ID members for QUERY.

CALLBACK receives at most LIMIT exact member projections.  Native member pages
are complete selected-slot snapshots, so repeated queries use the local page
after its first fetch.  ERRBACK receives the service response and reason."
  (unless (stringp query)
    (user-error "qq: Group member search query must be a string"))
  (setq limit (or limit 200))
  (unless (and (integerp limit) (<= 1 limit 200))
    (user-error "qq: Group member search limit must be between 1 and 200"))
  (unless (qq-protocol-uint64-decimal-p group-id)
    (user-error "qq: Group member search requires an exact group UIN"))
  (if-let* ((page (qq-directory-group-member-page group-id)))
      (progn
        (qq-rpc-invoke
         callback
         (qq-core--filter-members
          (alist-get 'members page) query limit))
        nil)
    (qq-core--start-request
     (lambda (success failure)
       (qq-directory-list-group-members
        group-id
        (lambda (members)
          (funcall success
                   (qq-core--filter-members members query limit)))
        failure))
     callback errback)))

(defun qq-core--apply-group-setting (group-id field value)
  "Apply confirmed group FIELD VALUE for GROUP-ID to shared state."
  (let ((groups (qq-state-groups))
        changed)
    (setq groups
          (mapcar
           (lambda (group)
             (when (equal (alist-get 'group_id group) group-id)
               (setf (alist-get field group nil nil #'eq) value)
               (setq changed t))
             group)
           groups))
    (when changed
      (qq-state-apply-groups groups))
    changed))

(defun qq-core--group-setting-success
    (group-id field value success receipt)
  "Apply one confirmed group setting and forward RECEIPT to SUCCESS."
  (qq-core--apply-group-setting group-id field value)
  (funcall success receipt))

(defun qq-core-set-group-name
    (group-id name &optional callback errback)
  "Set GROUP-ID's public NAME."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group name requires an exact group UIN"))
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "qq: Group name must be a non-empty string"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-set-group-name
      group-id name
      (apply-partially #'qq-core--group-setting-success
                       group-id 'group_name name success)
      failure))
   callback errback))

(defun qq-core-set-friend-pinned
    (user-id pinned &optional callback errback)
  "Set USER-ID's friend conversation PINNED state."
  (unless (qq-protocol-user-uin-p user-id)
    (user-error "qq: Friend pinned state requires an exact user UIN"))
  (setq pinned (and pinned t))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-set-friend-pinned
      user-id pinned success failure))
   callback errback))

(defun qq-core-set-presence (presence &optional callback errback)
  "Set the selected account's PRESENCE through the native service.

CALLBACK receives the exact acknowledgement.  The request is routed to the
locally selected managed account without changing its lifecycle phase."
  (setq presence
        (qq-protocol-validate-account-presence
         presence "account presence" 'user-error))
  (let ((account-id
         (or (qq-runtime-current-account-id)
             (user-error "qq: Select a managed QQ account first"))))
    (qq-core--start-request
     (lambda (success failure)
       (qq-account-set-presence
        account-id presence success failure))
     callback errback)))

(defun qq-core-set-group-remark
    (group-id remark &optional callback errback)
  "Set or clear GROUP-ID's account-local REMARK."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group remark requires an exact group UIN"))
  (unless (stringp remark)
    (user-error "qq: Group remark must be a string"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-set-group-remark
      group-id remark
      (apply-partially
       #'qq-core--group-setting-success
       group-id 'group_remark
       (and (not (string-empty-p remark)) remark)
       success)
      failure))
   callback errback))

(defun qq-core-set-group-whole-mute
    (group-id enabled &optional callback errback)
  "Set GROUP-ID's whole-group mute state."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group whole mute requires an exact group UIN"))
  (setq enabled (and enabled t))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-set-group-whole-mute
      group-id enabled success failure))
   callback errback))

(defun qq-core-set-group-pinned
    (group-id pinned &optional callback errback)
  "Set GROUP-ID's conversation PINNED state."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group pinned state requires an exact group UIN"))
  (setq pinned (and pinned t))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-set-group-pinned
      group-id pinned
      (apply-partially #'qq-core--group-setting-success
                       group-id 'pinned (if pinned t :false) success)
      failure))
   callback errback))

(defun qq-core-clock-in-group
    (group-id &optional callback errback)
  "Clock the selected account into GROUP-ID."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group clock-in requires an exact group UIN"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-clock-in-group group-id success failure))
   callback errback))

(defun qq-core-get-group-at-all-remaining
    (group-id callback &optional errback)
  "Fetch GROUP-ID's live @all availability."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group @all quota requires an exact group UIN"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-get-group-at-all-remaining
      group-id success failure))
   callback errback))

(defun qq-core--group-leave-success (group-id success receipt)
  "Remove confirmed GROUP-ID from state, then forward RECEIPT to SUCCESS."
  (when (qq-state-groups-loaded-p)
    (qq-state-apply-groups
     (seq-remove
      (lambda (group)
        (equal (alist-get 'group_id group) group-id))
      (qq-state-groups))))
  (funcall success receipt))

(defun qq-core-leave-group
    (group-id &optional callback errback)
  "Leave GROUP-ID without dismissing it."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group leave requires an exact group UIN"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-leave-group
      group-id
      (apply-partially #'qq-core--group-leave-success group-id success)
      failure))
   callback errback))

(defun qq-core-set-group-member-card
    (group-id user-id card &optional callback errback)
  "Set or clear USER-ID's CARD in GROUP-ID."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group member card requires an exact group UIN"))
  (unless (qq-protocol-user-uin-p user-id)
    (user-error "qq: Group member card requires an exact user UIN"))
  (unless (stringp card)
    (user-error "qq: Group member card must be a string"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-set-group-member-card
      group-id user-id card success failure))
   callback errback))

(defun qq-core-set-group-member-special-title
    (group-id user-id special-title &optional callback errback)
  "Set or clear USER-ID's SPECIAL-TITLE in GROUP-ID."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Special title requires an exact group UIN"))
  (unless (qq-protocol-user-uin-p user-id)
    (user-error "qq: Special title requires an exact user UIN"))
  (unless (stringp special-title)
    (user-error "qq: Special title must be a string"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-set-group-member-special-title
      group-id user-id special-title success failure))
   callback errback))

(defun qq-core-kick-group-member
    (group-id user-id reject-add-request &optional callback errback)
  "Remove USER-ID from GROUP-ID."
  (unless (qq-protocol-group-uin-p group-id)
    (user-error "qq: Group kick requires an exact group UIN"))
  (unless (qq-protocol-user-uin-p user-id)
    (user-error "qq: Group kick requires an exact user UIN"))
  (setq reject-add-request (and reject-add-request t))
  (qq-core--start-request
   (lambda (success failure)
     (qq-directory-kick-group-member
      group-id user-id reject-add-request success failure))
   callback errback))

(defun qq-core--media-plan (segment index)
  "Return a deferred preparation plan for SEGMENT at INDEX, or nil.

Local image, record, and video bytes are staged when sending.  A favorite
segment carries only its durable QQ identity and is materialized through the
same Resource and Prepared Attachment pipeline at that point."
  (let ((kind (alist-get 'type segment))
        (data (alist-get 'data segment)))
    (cond
     ((equal kind "favorite_emoji")
      (let ((favorite-id (and (listp data)
                              (alist-get 'favorite_emoji_id data))))
        (unless (and (qq-server-wire-exact-object-keys-p
                      data '(favorite_emoji_id))
                     (qq-favorite-emoji-id-p favorite-id))
          (user-error "qq: Favorite segment requires one durable identity"))
        (list :index index :kind kind :favorite-emoji-id favorite-id)))
     ((member kind '("image" "record" "video"))
      (let* ((attachment-id (and (listp data)
                                 (alist-get 'attachment_id data)))
             (file (and (listp data)
                        (or (alist-get 'file data)
                            (alist-get 'path data))))
             (summary (and (listp data) (alist-get 'summary data)))
             (sub-type (and (listp data) (alist-get 'sub_type data))))
        (cond
         (attachment-id nil)
         ((and (stringp file) (not (string-empty-p file)))
          (let ((path (expand-file-name file)))
            (unless (and (file-regular-p path) (file-readable-p path))
              (user-error
               "qq: %s source is not a readable regular file: %s"
               (capitalize kind) path))
            (if (equal kind "image")
                (let ((summary (or summary "[图片]"))
                      (sub-type (or sub-type 0)))
                  (unless (and (qq-protocol-non-empty-string-p summary)
                               (<= (length (string-to-list summary)) 128)
                               (not (string-match-p "[[:cntrl:]]" summary)))
                    (user-error
                     "qq: Image summary must be 1–128 printable characters"))
                  (unless (and (integerp sub-type)
                               (<= 0 sub-type #xffffffff))
                    (user-error "qq: Image sub-type must be an unsigned integer"))
                  (list :index index :kind kind :path path
                        :summary summary :sub-type sub-type))
              (list :index index :kind kind :path path))))
         (t
          (user-error
           "qq: Native %s sending requires a local file or prepared attachment"
           kind))))))))

(defun qq-core--release-send-resource (resource-id)
  "Best-effort release one send-pipeline RESOURCE-ID."
  (when resource-id
    (condition-case error-data
        (qq-resource-release
         resource-id nil
         (lambda (_body reason)
           (message "qq: staged media cleanup failed: %s" reason)))
      ((error quit)
       (message "qq: staged media cleanup failed: %s"
                (error-message-string error-data))))))

(defun qq-core--release-send-attachment (attachment-id)
  "Best-effort release one unused send-pipeline ATTACHMENT-ID."
  (when attachment-id
    (condition-case error-data
        (qq-attachment-release
         attachment-id nil
         (lambda (_body reason)
           (message "qq: prepared media cleanup failed: %s" reason)))
      ((error quit)
       (message "qq: prepared media cleanup failed: %s"
                (error-message-string error-data))))))

(defun qq-core--send-message-with-media
    (session-key segments plans raw-message callback errback)
  "Resolve deferred media PLANS, then send SEGMENTS to SESSION-KEY.

Staging and preparation may run concurrently, but the immutable segment order
is retained.  This composite request owns preparation operations until they
produce ready attachments, then owns those attachments until `message.send'
accepts them.  Cancellation or failure releases everything still owned here."
  (let* ((owner (or (qq-runtime-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (optimistic-segments (copy-tree segments))
         (resolved (vconcat (copy-tree segments)))
         (remaining (length plans))
         (operations nil)
         (attachment-ids nil)
         (active t)
         send-token
         request)
    (cl-labels
        ((cancel-operation
           (operation)
           (condition-case error-data
               (qq-attachment-cancel-operation operation)
             ((error quit)
              (message "qq: media operation cleanup failed: %s"
                       (error-message-string error-data)))))
         (release-attachments
           ()
           (let ((owned-attachment-ids (delete-dups attachment-ids)))
             (setq attachment-ids nil)
             (dolist (attachment-id owned-attachment-ids)
               (qq-core--release-send-attachment attachment-id))))
         (cancel-send
           ()
           (when send-token
             (let ((token send-token))
               (setq send-token nil)
               (condition-case error-data
                   (qq-server-cancel token)
                 ((error quit)
                  (message "qq: media send cancellation failed: %s"
                           (error-message-string error-data)))))))
         (cleanup
           ()
           (let ((owned-operations operations))
             (setq operations nil)
             (cancel-send)
             (dolist (operation owned-operations)
               (cancel-operation operation))
             (release-attachments)))
         (finish
           (success-p body value)
           (when active
             (setq active nil
                   send-token nil)
             (if success-p
                 ;; Successful `message.send' has claimed every attachment.
                 (setq attachment-ids nil
                       operations nil)
               ;; Release is idempotent when the service already moved an
               ;; attachment into its Sending state.
               (cleanup))
             (if success-p
                 (qq-request-finish request)
               (qq-request-fail request))
             (if success-p
                 (qq-request--invoke callback value)
               (qq-request--invoke errback body value))))
         (send-failed
           (body reason)
           (finish nil body reason))
         (send-succeeded
           (receipt)
           (finish t nil receipt))
         (dispatch
           ()
           (if (not (qq-account-get owner))
               (qq-request-cancel request)
             (condition-case error-data
                 (let ((token
                        (qq-message-send
                         session-key (append resolved nil) raw-message
                         #'send-succeeded #'send-failed
                         optimistic-segments)))
                   ;; Nil is paired with a synchronous preflight failure.
                   ;; Accepted requests complete later on the event loop.
                   (when (and active token)
                     (setq send-token token)
                     (setf (qq-request-token request) token)))
               (error
                (send-failed nil (error-message-string error-data))))))
         (media-ready
           (plan attachment)
           (let ((attachment-id (alist-get 'attachment_id attachment))
                 (resource-id (alist-get 'resource_id attachment))
                 (thumbnail-resource-id
                  (and (equal (plist-get plan :kind) "video")
                       (alist-get
                        'thumbnail_resource_id
                        (alist-get 'use attachment)))))
             (if (not active)
                 (progn
                   (qq-core--release-send-attachment attachment-id)
                   (qq-core--release-send-resource resource-id)
                   (when thumbnail-resource-id
                     (qq-core--release-send-resource thumbnail-resource-id)))
               (push attachment-id attachment-ids)
               ;; The prepared attachment keeps the bytes alive, so the
               ;; composite never needs to retain the extra resource lease.
               (qq-core--release-send-resource resource-id)
               (when thumbnail-resource-id
                 (qq-core--release-send-resource thumbnail-resource-id))
               (unless (qq-account-get owner)
                 (qq-request-cancel request))
               (when active
                 (aset resolved (plist-get plan :index)
                       `((type . ,(if (equal (plist-get plan :kind)
                                             "favorite_emoji")
                                      "image"
                                    (plist-get plan :kind)))
                         (data . ((attachment_id . ,attachment-id)))))
                 (setq remaining (1- remaining))
                 (when (= remaining 0)
                   (dispatch))))))
         (cancel
           ()
           (when active
             (setq active nil)
             (cleanup)))
         (abort-startup
           ()
           (when active
             (setq active nil)
             (cleanup))
           (when request
             (qq-request-fail request))))
      (condition-case error-data
          (progn
            (setq request (qq-request-create owner #'cancel))
            (dolist (plan plans)
              (when active
                (let* ((ready (apply-partially #'media-ready plan))
                       (operation
                        (pcase (plist-get plan :kind)
                          ("image"
                           (qq-attachment-stage-and-prepare-image
                            session-key (plist-get plan :path)
                            (plist-get plan :summary)
                            (plist-get plan :sub-type)
                            ready #'send-failed))
                          ("record"
                           (qq-attachment-stage-and-prepare-record
                            session-key (plist-get plan :path)
                            ready #'send-failed))
                          ("video"
                           (qq-attachment-stage-and-prepare-video
                            session-key (plist-get plan :path)
                            ready #'send-failed))
                          ("favorite_emoji"
                           (qq-attachment-materialize-and-prepare-favorite
                            session-key
                            (plist-get plan :favorite-emoji-id)
                            ready #'send-failed))
                          (_ (error "qq: Unknown media plan")))))
                  (when (qq-attachment-operation-active-p operation)
                    (if active
                        (push operation operations)
                      (cancel-operation operation))))))
            request)
        ((error quit)
         (abort-startup)
         (signal (car error-data) (cdr error-data)))))))

(defun qq-core--file-send-plan (session-key segments)
  "Return a standalone local-file plan, or nil when SEGMENTS has no file.

QQ private and group files are standalone publications, not ordinary rich-text
segments.  Mixed text, reply, media, and multiple-file drafts are therefore
rejected instead of being split into several messages with ambiguous
partial-success rules."
  (let ((files
         (seq-filter
          (lambda (segment)
            (equal (alist-get 'type segment) "file"))
          segments)))
    (when files
      (unless (and (= (length files) 1)
                   (= (length segments) 1))
        (user-error
         "qq: Send one file at a time without text, reply, or other media"))
      (unless (memq (qq-state-session-key-type session-key) '(private group))
        (user-error "qq: Native file upload requires a private or group chat"))
      (let* ((data (alist-get 'data (car files)))
             (file (and (listp data)
                        (or (alist-get 'file data)
                            (alist-get 'path data))))
             (path (and (stringp file) (expand-file-name file)))
             (name (and (listp data) (alist-get 'name data))))
        (unless (and path (file-regular-p path) (file-readable-p path))
          (user-error "qq: File source is not a readable regular file: %s"
                      (or path file)))
        (list :path path
              :name (or name (file-name-nondirectory path))
              :segment (car files))))))

(defun qq-core--dataline-file-send-plan (session-key segments)
  "Return one DataLine File Transfer plan for local SEGMENTS, or nil.

Pictures remain FILE/FTN payloads on the wire; their `image' segment type is
retained only for the sender's optimistic local presentation."
  (when (eq (qq-state-session-key-type session-key) 'dataline)
    (let ((files
           (seq-filter
            (lambda (segment)
              (member (alist-get 'type segment) '("file" "image")))
            segments)))
      (when files
        (unless (and (= (length files) 1)
                     (= (length segments) 1))
          (user-error
           "qq: Send one DataLine file or image at a time without text or other media"))
        (let* ((segment (car files))
               (data (alist-get 'data segment))
               (file (and (listp data)
                          (or (alist-get 'file data)
                              (alist-get 'path data))))
               (path (and (stringp file) (expand-file-name file)))
               (name (and (listp data) (alist-get 'name data))))
          (unless (and path (file-regular-p path) (file-readable-p path))
            (user-error "qq: File source is not a readable regular file: %s"
                        (or path file)))
          (list :path path
                :name (or name (file-name-nondirectory path))
                :segment segment))))))

(defun qq-core--send-file
    (session-key plan callback errback)
  "Stage and publish local file PLAN through SESSION-KEY.

Cancellation detaches the caller.  An already-started native upload is allowed
to settle so its resource lease and staged resource can be released safely."
  (let* ((owner (or (qq-runtime-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (observing t)
         (publication-active-p nil)
         resource-id
         ready-watch
         request)
    (cl-labels
        ((detach-ready-watch
           ()
           (when ready-watch
             (qq-request-watch-cancel ready-watch)
             (setq ready-watch nil)))
         (release-resource
           ()
           (when resource-id
             (let ((owned resource-id))
               (setq resource-id nil)
               (qq-core--release-send-resource owned))))
         (finish
           (success-p body value)
           (setq publication-active-p nil)
           (detach-ready-watch)
           (release-resource)
           (when (qq-request-active-p request)
             (if success-p
                 (qq-request-finish request)
               (qq-request-fail request))
             (if success-p
                 (qq-request--invoke callback value)
               (qq-request--invoke errback body value))))
         (send-ready
           (_resource)
           (detach-ready-watch)
           (if (not observing)
               (release-resource)
             (condition-case error-data
                 (progn
                   ;; Caller cancellation detaches presentation, but native
                   ;; publication must retain the staged bytes until its own
                   ;; callback settles.
                   (setq publication-active-p t)
                   (if (eq (qq-state-session-key-type session-key) 'dataline)
                       (qq-message-send-file
                        session-key resource-id
                        (lambda (receipt) (finish t nil receipt))
                        (lambda (body reason) (finish nil body reason))
                        (plist-get plan :segment))
                     (qq-message-send-file
                      session-key resource-id
                      (lambda (receipt) (finish t nil receipt))
                      (lambda (body reason) (finish nil body reason)))))
               ((error quit)
                (finish nil nil (error-message-string error-data))))))
         (stage-observed
           (resource)
           (setq resource-id (alist-get 'resource_id resource))
           (if (not observing)
               (release-resource)
             (pcase (alist-get 'phase resource)
               ("ready" (send-ready resource))
               ("staging"
                (setq ready-watch
                      (qq-resource-await-ready
                       resource-id #'send-ready #'stage-failed)))
               ((or "failed" "released")
                (let ((problem (alist-get 'error resource)))
                  (stage-failed
                   problem
                   (or (alist-get 'message problem)
                       "Staged resource did not become ready"))))
               (_
                (stage-failed
                 nil "Gateway returned an invalid staged-resource phase")))))
         (stage-failed
           (body reason)
           (finish nil body reason))
         (cancel
           ()
           (setq observing nil)
           (detach-ready-watch)
           (unless publication-active-p
             (release-resource))))
      (setq request (qq-request-create owner #'cancel))
      (condition-case error-data
          (qq-resource-stage-local
           (plist-get plan :path)
           (plist-get plan :name)
           nil
           #'stage-observed
           #'stage-failed)
        ((error quit)
         (qq-request-fail request)
         (detach-ready-watch)
         (release-resource)
         (signal (car error-data) (cdr error-data))))
      request)))

(defun qq-core-send-message
    (session-key segments &optional raw-message callback errback)
  "Send SEGMENTS to SESSION-KEY through the native service.

Local image and video paths are copied into the service Resource Store; videos
also get a locally generated immutable JPEG thumbnail.  Local PCM WAV records
are first derived into message-ready Tencent Silk.  Media is prepared for the
selected account and conversation, then replaced by opaque attachment IDs
before the wire request is sent.  RAW-MESSAGE is an
optional optimistic rendering override.  The pending row is promoted only by
the later authoritative self event."
  (let* ((dataline-p (eq (qq-state-session-key-type session-key) 'dataline))
         (dataline-file-plan
          (and dataline-p
               (qq-core--dataline-file-send-plan session-key segments)))
         (file-plan (and (not dataline-p)
                         (qq-core--file-send-plan session-key segments)))
         (plans
          (and (not dataline-file-plan)
               (cl-loop for segment in segments
                        for index from 0
                        for plan = (qq-core--media-plan segment index)
                        when plan collect plan)))
         (error-fn (or errback #'qq-core--default-error)))
    (cond
     (dataline-file-plan
      (qq-core--send-file
       session-key dataline-file-plan callback error-fn))
     (dataline-p
      (qq-core--start-request
       (lambda (success failure)
         (qq-message-send
          session-key segments raw-message success failure))
       callback error-fn))
     (file-plan
      (qq-core--send-file
       session-key file-plan callback error-fn))
     (plans
      (qq-core--send-message-with-media
       session-key segments plans raw-message callback error-fn))
     (t
      (qq-core--start-request
       (lambda (success failure)
         (qq-message-send
          session-key segments raw-message success failure))
       callback error-fn)))))

(defun qq-core-send-poke
    (session-key target-id &optional callback errback)
  "Poke TARGET-ID in SESSION-KEY through the native service."
  (qq-core--start-request
   (lambda (success failure)
     (qq-message-send-poke
      session-key target-id success failure))
   callback errback))

(defun qq-core-set-message-reaction
    (message reaction set &optional callback errback)
  "Add or remove normalized REACTION on native group MESSAGE.

SET non-nil adds the reaction.  CALLBACK receives the successful service
receipt; ERRBACK receives failure details."
  (unless (listp message)
    (user-error "qq: Reaction requires a normalized message"))
  (unless (listp reaction)
    (user-error "qq: Reaction requires a normalized emoji identity"))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (and session-key (stringp message-id))
      (user-error "qq: Reaction requires exact session and message identity"))
    (qq-core--start-request
     (lambda (success failure)
       (qq-message-set-reaction
        message reaction set success failure))
     callback errback)))

(defun qq-core-set-message-essence
    (message set &optional callback errback)
  "Set or remove normalized native group MESSAGE as essence.

SET non-nil marks the message as essence.  CALLBACK receives the successful
service receipt; ERRBACK receives failure details."
  (unless (listp message)
    (user-error "qq: Essence action requires a normalized message"))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-protocol-message-id-p message-id))
      (user-error "qq: Essence action requires exact group message identity"))
    (qq-core--start-request
     (lambda (success failure)
       (qq-message-set-essence message set success failure))
     callback errback)))

(defun qq-core-set-message-todo
    (message operation &optional callback errback)
  "Apply todo OPERATION to normalized native group MESSAGE.

OPERATION must be one of `set', `complete', or `cancel'.  CALLBACK receives
the successful receipt; ERRBACK receives failure details."
  (unless (listp message)
    (user-error "qq: Todo action requires a normalized message"))
  (unless (memq operation '(set complete cancel))
    (user-error "qq: Unknown message todo operation %S" operation))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-protocol-message-id-p message-id))
      (user-error "qq: Todo action requires exact group message identity"))
    (qq-core--start-request
     (lambda (success failure)
       (qq-message-set-todo message operation success failure))
     callback errback)))

(defun qq-core-recall-poke (message &optional callback errback)
  "Recall normalized poke MESSAGE through its native capability.

CALLBACK receives the successful response; ERRBACK receives failure details."
  (unless (qq-message-poke-recall-capable-p message)
    (user-error "qq: Poke recall requires an authored group Poke GrayTip"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-message-recall-poke message success failure))
   callback errback))

(defun qq-core-delete-message-local (message &optional callback errback)
  "Persistently hide normalized MESSAGE from local history only.

This operation never asks QQ to recall the message.  CALLBACK receives the
checked durable receipt; ERRBACK receives failure details."
  (unless (qq-message-delete-local-capable-p message)
    (user-error "qq: Local deletion requires a canonical timeline row"))
  (qq-core--start-request
   (lambda (success failure)
     (qq-message-delete-local message success failure))
   callback errback))

(defun qq-core-recall-message (message &optional callback errback)
  "Recall normalized MESSAGE through the native service.

CALLBACK receives the successful response; ERRBACK receives the failure
response and reason."
  (let ((session-key (alist-get 'session-key message)))
    (unless session-key
      (user-error "qq: Recall requires a normalized message"))
    (qq-core--start-request
     (lambda (success failure)
       (qq-message-recall
        session-key message success failure))
     callback errback)))

(defun qq-core--read-operation-current-p (session-key operation)
  "Return non-nil when OPERATION still owns SESSION-KEY."
  (eq operation
      (gethash
       (qq-core--read-operation-key
        (qq-core--read-operation-owner operation) session-key)
       qq-core--read-operations)))

(defun qq-core--read-message-after-p (candidate reference)
  "Return non-nil when CANDIDATE follows REFERENCE in their session timeline.

Read coalescing is a client projection concern.  It compares canonical row
identities in the cached timeline instead of depending on native sequence or
Message ID metadata."
  (let ((candidate-session (alist-get 'session-key candidate))
        (reference-session (alist-get 'session-key reference))
        (candidate-row (alist-get 'canonical-row-key candidate))
        (reference-row (alist-get 'canonical-row-key reference)))
    (when (and candidate-session
               (equal candidate-session reference-session)
               (qq-protocol-uint64-decimal-p candidate-row)
               (qq-protocol-uint64-decimal-p reference-row))
      (let* ((messages (qq-state-session-messages candidate-session))
             (row-key (lambda (message)
                        (alist-get 'canonical-row-key message)))
             (candidate-position
              (cl-position candidate-row messages
                           :key row-key :test #'equal))
             (reference-position
              (cl-position reference-row messages
                           :key row-key :test #'equal)))
        (and candidate-position
             reference-position
             (> candidate-position reference-position))))))

(defun qq-core--cancel-read-operation (session-key operation)
  "Revoke OPERATION, its queued intent, and its transport token."
  (when (qq-core--read-operation-current-p session-key operation)
    (remhash
     (qq-core--read-operation-key
      (qq-core--read-operation-owner operation) session-key)
     qq-core--read-operations))
  (let ((token (qq-core--read-operation-token operation)))
    (setf (qq-core--read-operation-token operation) nil
          (qq-core--read-operation-next-message operation) nil
          (qq-core--read-operation-next-callback operation) nil
          (qq-core--read-operation-next-errback operation) nil)
    (when token
      (qq-server-cancel token))))

(defun qq-core--advance-read-operation
    (session-key operation success-p)
  "Advance OPERATION after one leaf settles with SUCCESS-P.

The stable composite request remains active while a queued read intent exists
and becomes terminal only after the actual queue is empty."
  (if-let* ((message (qq-core--read-operation-next-message operation)))
      (progn
        (setf (qq-core--read-operation-message operation) message
              (qq-core--read-operation-callback operation)
              (qq-core--read-operation-next-callback operation)
              (qq-core--read-operation-errback operation)
              (qq-core--read-operation-next-errback operation)
              (qq-core--read-operation-next-message operation) nil
              (qq-core--read-operation-next-callback operation) nil
              (qq-core--read-operation-next-errback operation) nil)
        ;; Publish the successor before leaf delivery so callback reentry sees
        ;; the stable composite request still in flight.
        (qq-core--dispatch-read-operation session-key operation))
    (remhash
     (qq-core--read-operation-key
      (qq-core--read-operation-owner operation) session-key)
     qq-core--read-operations)
    (if success-p
        (qq-request-finish
         (qq-core--read-operation-request operation))
      (qq-request-fail
       (qq-core--read-operation-request operation)))))

(defun qq-core--settle-read-leaf
    (session-key operation success-p body value)
  "Settle OPERATION's current read report with BODY and VALUE."
  (when (qq-core--read-operation-current-p session-key operation)
    (let* ((request (qq-core--read-operation-request operation))
           (callback
            (if success-p
                (qq-core--read-operation-callback operation)
              (or (qq-core--read-operation-errback operation)
                  #'qq-core--default-error))))
      (setf (qq-core--read-operation-token operation) nil)
      (if (not (qq-request--owner-current-p request))
          (qq-request-cancel request)
        (qq-core--advance-read-operation session-key operation success-p)
        (if success-p
            (qq-request--invoke-owned
             (qq-core--read-operation-owner operation) callback value)
          (qq-request--invoke-owned
           (qq-core--read-operation-owner operation)
           callback body value))))))

(defun qq-core--dispatch-read-operation (session-key operation)
  "Dispatch OPERATION's current leaf for SESSION-KEY."
  (let* ((request (qq-core--read-operation-request operation))
         (token
          (qq-message-mark-read
           (qq-core--read-operation-message operation)
           (lambda (receipt)
             (qq-core--settle-read-leaf
              session-key operation t nil receipt))
           (lambda (body reason)
             (qq-core--settle-read-leaf
              session-key operation nil body reason)))))
    ;; A nil token is paired with a synchronous failure callback.  Every
    ;; non-nil token completes later through the transport event loop.
    (when (and token
               (qq-core--read-operation-current-p session-key operation)
               (qq-request-active-p request))
      (setf (qq-core--read-operation-token operation) token))
    request))

(defun qq-core--start-mark-message-read (message callback errback)
  "Start one native read report for canonical MESSAGE."
  (let* ((session-key (alist-get 'session-key message))
         (owner (qq-runtime-current-account-id))
         request
         operation)
    (condition-case error-data
        (progn
          (setq operation
                (qq-core--read-operation-create
                 :owner (copy-sequence owner)
                 :message message
                 :callback callback
                 :errback errback))
          (setq request
                (qq-request-create
                 owner
                 (lambda ()
                   (qq-core--cancel-read-operation session-key operation))))
          (setf (qq-core--read-operation-request operation) request)
          (puthash (qq-core--read-operation-key owner session-key)
                   operation qq-core--read-operations)
          (qq-core--dispatch-read-operation session-key operation)
          request)
      ((error quit)
       (when request
         (qq-request-cancel request))
       (signal (car error-data) (cdr error-data))))))

(defun qq-core-mark-message-read (message &optional callback errback)
  "Advance native read state through normalized MESSAGE.

Only one report per session is in flight and at most one newest message intent
is queued.  CALLBACK belongs only to an intent that is actually dispatched;
duplicate, older, and superseded intents do not accumulate waiters.  ERRBACK
receives failure details."
  (unless (qq-core-message-read-capable-p message)
    (user-error
     "qq: Native read report requires a current canonical timeline row"))
  (let* ((session-key (alist-get 'session-key message))
         (owner (qq-runtime-current-account-id))
         (operation
          (gethash (qq-core--read-operation-key owner session-key)
                   qq-core--read-operations)))
    (cond
     ((null operation)
      (qq-core--start-mark-message-read message callback errback))
     ((not (qq-core--read-message-after-p
            message (qq-core--read-operation-message operation)))
      (qq-core--read-operation-request operation))
     (t
      (let ((next (qq-core--read-operation-next-message operation)))
        (when (or (null next)
                  (qq-core--read-message-after-p message next))
          (setf (qq-core--read-operation-next-message operation) message
                (qq-core--read-operation-next-callback operation) callback
                (qq-core--read-operation-next-errback operation) errback))
        (qq-core--read-operation-request operation))))))

(defun qq-core--authored-message-at-sequence (session-key sequence)
  "Return SESSION-KEY's authored row carrying SEQUENCE, or nil."
  (seq-find
   (lambda (message)
     (and (not (qq-state-service-message-p message))
          (equal (alist-get 'message-seq message) sequence)))
   (qq-state-session-messages session-key)))

(defun qq-core-history-frontier (session-key)
  "Return the native service's exact known history frontier.

SESSION-KEY identifies the private or group conversation.  The result carries
`:sequence' and may name one cached authored row at that cursor.  A sequence is
not a message identity: Service Timeline Messages can share it and therefore
do not supply `:message-id'.  Group `latest_sequence' is authoritative and is
advanced by a newer live event.  Private history deliberately exposes only a
live sequence observed by this client projection because the C2C body-history
methods provide no latest cursor.  `:empty-p' or `:unavailable-reason'
explains a result without a sequence."
  (let* ((identity (qq-state-session-key-identity session-key))
         (kind (alist-get 'type identity))
         (target-id (alist-get 'target-id identity))
         (live (qq-message-live-frontier session-key))
         (live-sequence (alist-get 'sequence live)))
    (unless (memq kind '(private group))
      (user-error "qq: Native history supports private and group chats"))
    (pcase kind
      ('private
       (if live-sequence
           (let ((message
                  (qq-core--authored-message-at-sequence
                   session-key live-sequence)))
             (list :sequence live-sequence
                   :message-id (alist-get 'server-id message)
                   :source 'live-event))
         (list :unavailable-reason 'private-latest-sequence)))
      ('group
       (let* ((group (qq-state-group target-id))
              (directory-sequence (alist-get 'latest_sequence group))
              sequence source)
         (when (and directory-sequence
                    (not (qq-protocol-uint64-decimal-p
                          directory-sequence t)))
           (error "qq: Native group latest_sequence is not exact"))
         (cond
          ((and live-sequence
                (or (null directory-sequence)
                    (qq-protocol-decimal-less-p
                     directory-sequence live-sequence)))
           (setq sequence live-sequence source 'live-event))
          (directory-sequence
           (setq sequence directory-sequence source 'group-directory)))
         (cond
          (sequence
           (let ((message
                  (qq-core--authored-message-at-sequence
                   session-key sequence)))
             (list :sequence sequence
                   :message-id (alist-get 'server-id message)
                   :source source
                   :authoritative-p t)))
          (group
           (list :empty-p t :source 'group-directory :authoritative-p t))
          (t
           (list :unavailable-reason 'group-directory))))))))

(defconst qq-core-history-port-version qq-message-history-port-version
  "Version of the conversation-neutral Gateway history contract.")

(defun qq-core-fetch-history-page
    (session-key cursor direction callback
                 &optional errback count lifecycle-owner)
  "Fetch one conversation-neutral history page for SESSION-KEY.

CURSOR is nil for the authoritative latest page or an opaque cursor returned
in earlier metadata.  DIRECTION is `older' or `newer'.  CALLBACK always
receives versioned metadata with opaque older/newer cursors and explicit edge
flags; callers must not inspect the selected storage/native driver.  ERRBACK
receives failure details and COUNT limits rows.  LIFECYCLE-OWNER optionally
owns cancellation of the returned request."
  (unless (memq direction '(older newer))
    (user-error "qq: History direction must be older or newer"))
  (setq count (min 100 (max 1 (or count qq-history-fetch-count))))
  (qq-core--start-request
   (lambda (success failure)
     (qq-message--request-history-page
      session-key cursor direction success failure count))
   callback errback
   :lifecycle-owner lifecycle-owner))

(defun qq-core-fetch-history-around
    (session-key message-id callback
                 &optional errback count sequence lifecycle-owner)
  "Fetch SESSION-KEY history around one exact locator.

MESSAGE-ID takes precedence.  When it is absent, SEQUENCE names an authored
native row; ambiguous sequence resolution fails in the Message Store.  DataLine
requires MESSAGE-ID.  No cached frontier or adapter hint participates in the
request.  CALLBACK and ERRBACK receive settlement, COUNT limits rows, and
LIFECYCLE-OWNER optionally owns cancellation of the returned request."
  (setq count (min 100 (max 1 (or count qq-history-fetch-count))))
  (let ((center
         (cond
          (message-id
           `((kind . "message") (message_id . ,message-id)))
          ((and sequence
                (memq (qq-state-session-key-type session-key)
                      '(private group)))
           `((kind . "sequence") (sequence . ,sequence)))
          (t
           (user-error "qq: History around requires an exact message locator")))))
    (qq-core--start-request
     (lambda (success failure)
       (qq-message--request-history-around
        session-key center success failure count))
     callback errback
     :lifecycle-owner lifecycle-owner)))

(defun qq-core-get-forward
    (resource-id scene callback &optional errback)
  "Fetch merged-forward entries behind opaque RESOURCE-ID in SCENE.
CALLBACK receives a page plist with messages and the unsupported-entry count."
  (qq-core--start-request
   (lambda (success failure)
     (qq-message-get-forward resource-id scene success failure))
   callback errback))

(defun qq-core-history-exhausted-error-p (response reason)
  "Return non-nil when native RESPONSE and REASON mean history EOF."
  (ignore response reason)
  nil)

(defconst qq-core--capability-methods
  '((recent-conversations "conversation.list_recent")
    (contacts "contact.list_friends" "contact.list_groups")
    (user-profile "profile.get")
    (profile-like-summary "profile.get_like_summary")
    (profile-like "profile.send_like")
    (avatar "contact.get_user_avatar" "contact.get_group_avatar")
    (group-members "contact.list_group_members")
    (group-requests "group_request.list" "group_request.decide")
    (group-settings "group.set_name" "group.set_remark"
                    "group.set_whole_mute" "group.set_pinned")
    (group-member-settings "group.set_member_card"
                           "group.set_member_special_title")
    (group-moderation "group.kick_member")
    (group-clock-in "group.clock_in")
    (group-at-all-quota "group.get_at_all_remaining")
    (group-lifecycle "group.leave")
    (presence "account.set_presence")
    (send-text "message.send")
    (dataline-text "dataline.send_text")
    (dataline-file "dataline.send_file" "resource.stage_local")
    (send-message "message.send")
    (merged-forward "message.send_merged_forward")
    (send-file "file.send" "resource.stage_local")
    (face "message.send")
    (reply "message.send")
    (mention "message.send")
    (poke "message.poke")
    (recall "message.recall")
    (history "message.get_history_page"
             "message.get_history_around")
    (read-receipt "message.mark_read"))
  "Product capabilities and every required negotiated Gateway method.")

(defun qq-core-implemented-p (capability)
  "Return non-nil when this client implements product CAPABILITY."
  (and (assq capability qq-core--capability-methods) t))

(defun qq-core-supports-p (capability)
  "Return non-nil when negotiated methods support product CAPABILITY."
  (when-let* ((spec (assq capability qq-core--capability-methods)))
    (let ((methods (qq-server-capabilities)))
      (cl-every (lambda (method) (member method methods)) (cdr spec)))))

(defun qq-core-message-read-capable-p (message)
  "Return non-nil when MESSAGE is a current, stable read reference."
  (let ((account (qq-account-get (qq-runtime-current-account-id))))
    (and (qq-core-ready-p)
         (qq-core-supports-p 'read-receipt)
         (equal (alist-get 'phase account) "online")
         (qq-message-read-capable-p message))))

(defun qq-core-presence-capable-p ()
  "Return non-nil when the current account can accept presence changes."
  (and (qq-core-ready-p)
       (qq-core-supports-p 'presence)
       (let ((account
              (qq-account-get (qq-runtime-current-account-id))))
         (and account
              (equal (alist-get 'phase account) "online")))))

(defun qq-core--bootstrap-complete (owner token failed-p)
  "Complete one OWNER directory bootstrap part identified by TOKEN."
  (when-let* ((bootstrap (gethash owner qq-core--bootstraps)))
    (when (eq token (qq-core--bootstrap-token bootstrap))
      (if failed-p
          (remhash owner qq-core--bootstraps)
        (setf (qq-core--bootstrap-pending bootstrap)
              (max 0 (1- (qq-core--bootstrap-pending bootstrap)))))
      t)))

(defun qq-core--bootstrap-success (owner token _value)
  "Record one successful OWNER bootstrap part identified by TOKEN."
  (when (qq-core--bootstrap-complete owner token nil)
    (when-let* ((bootstrap (gethash owner qq-core--bootstraps)))
      (when (zerop (qq-core--bootstrap-pending bootstrap))
        (qq-core--bootstrap-managed-recents)))))

(defun qq-core--bootstrap-failure (owner token _body reason)
  "Record failed OWNER bootstrap TOKEN and report REASON."
  (when (qq-core--bootstrap-complete owner token t)
    (qq-core--default-error nil reason)
    ;; Recent Conversation is independent of the directory which failed. The
    ;; current token check above prevents a superseded Native Session from
    ;; releasing this account's recent bootstrap.
    (qq-core--bootstrap-managed-recents)))

(defun qq-core--maybe-bootstrap-account (owner)
  "Load live friend and group directories for online account OWNER."
  (when (and (qq-server-ready-p)
             (qq-core-supports-p 'contacts))
    (let* ((account (qq-account-get owner))
           (instance-id (qq-server-gateway-instance-id))
           (existing (gethash owner qq-core--bootstraps)))
      (when (and account instance-id
                 (equal (alist-get 'phase account) "online")
                 (not (and existing
                           (equal
                            instance-id
                            (qq-core--bootstrap-instance-id existing)))))
        (qq-runtime-with-account owner
          (let* ((token (list 'qq-core-bootstrap owner))
                 (friends-refresh
                  (and (qq-state-friend-categories-loaded-p) t))
                 (groups-refresh (and (qq-state-groups-loaded-p) t))
                 (bootstrap
                  (qq-core--bootstrap-create
                   :owner (copy-sequence owner)
                   :instance-id (copy-sequence instance-id)
                   :token token
                   :pending 2)))
            ;; Publish before dispatch because preflight errors may settle
            ;; synchronously.
            (puthash (copy-sequence owner) bootstrap
                     qq-core--bootstraps)
            (qq-core-refresh-friend-categories
             (apply-partially
              #'qq-core--bootstrap-success owner token)
             (apply-partially
              #'qq-core--bootstrap-failure owner token)
             friends-refresh)
            (qq-core-refresh-joined-groups
             (apply-partially
              #'qq-core--bootstrap-success owner token)
             (apply-partially
              #'qq-core--bootstrap-failure owner token)
             groups-refresh)))))))

(defun qq-core--maybe-bootstrap-all (&rest _arguments)
  "Load account-scoped directories for every online managed account."
  (dolist (account (qq-account-list))
    (qq-core--maybe-bootstrap-account
     (alist-get 'account_id account))))

(defun qq-core--handle-ready (_instance-id)
  "Start fresh account bootstraps after typed Gateway ready."
  ;; Account replacement publishes before this hook.  Discard the prior
  ;; connection's completed context even when the service instance survived,
  ;; because transient conversation events may have arrived while detached.
  (clrhash qq-core--bootstraps)
  (clrhash qq-core--observed-account-phases)
  (clrhash qq-core--recent-resync-contexts)
  (clrhash qq-core--recent-bootstrap-instances)
  (dolist (account (qq-account-list))
    (puthash (alist-get 'account_id account)
             (alist-get 'phase account)
             qq-core--observed-account-phases))
  (qq-core--maybe-bootstrap-all)
  (qq-core--bootstrap-managed-recents))

(defun qq-core--managed-recent-bootstrap-failed
    (account-id _body reason)
  "Release ACCOUNT-ID's bootstrap marker and report REASON."
  (remhash account-id qq-core--recent-bootstrap-instances)
  (qq-core--default-error nil reason))

(defun qq-core--bootstrap-managed-recents ()
  "Bootstrap recent conversations for every online managed account."
  (when (and (qq-server-ready-p)
             (qq-core-supports-p 'recent-conversations))
    (let ((instance-id (qq-server-gateway-instance-id)))
      (dolist (account (qq-account-list))
        (let* ((account-id (alist-get 'account_id account))
               (directory-bootstrap
                (gethash account-id qq-core--bootstraps))
               (directory-pending
                (and directory-bootstrap
                     (equal instance-id
                            (qq-core--bootstrap-instance-id
                             directory-bootstrap))
                     (> (qq-core--bootstrap-pending directory-bootstrap) 0))))
          (if (equal (alist-get 'phase account) "online")
              (unless
                  (or directory-pending
                      (equal instance-id
                             (gethash account-id
                                      qq-core--recent-bootstrap-instances)))
                ;; Publish before dispatch because preflight failure callbacks
                ;; may run synchronously.
                (puthash (copy-sequence account-id)
                         (copy-sequence instance-id)
                         qq-core--recent-bootstrap-instances)
                (qq-core-refresh-recent-conversations
                 nil
                 (apply-partially
                  #'qq-core--managed-recent-bootstrap-failed account-id)
                 nil account-id))
            (remhash account-id qq-core--recent-bootstrap-instances)))))))

(defun qq-core--handle-account-registry-change (reason account-id)
  "Bootstrap after account registry REASON other than typed ready."
  (unless (eq reason 'ready)
    (when account-id
      (let* ((account (qq-account-get account-id))
             (old-phase
              (gethash account-id qq-core--observed-account-phases))
             (new-phase (and account (alist-get 'phase account)))
             (online-boundary-p
              (and old-phase
                   (not (equal old-phase new-phase))
                   (or (equal old-phase "online")
                       (equal new-phase "online")))))
        (when (or (null account) online-boundary-p)
          (remhash account-id qq-core--bootstraps)
          (remhash account-id qq-core--recent-resync-contexts))
        (if account
            (puthash account-id new-phase
                     qq-core--observed-account-phases)
          (remhash account-id qq-core--observed-account-phases))))
    (qq-core--maybe-bootstrap-all)
    (qq-core--bootstrap-managed-recents)))

(defun qq-core--recent-resync-complete
    (account-id context failed-p &rest arguments)
  "Release ACCOUNT-ID lag resync CONTEXT and optionally report failure."
  (when (equal context
               (gethash account-id qq-core--recent-resync-contexts))
    (remhash account-id qq-core--recent-resync-contexts))
  (when failed-p
    (qq-core--default-error nil (cadr arguments))))

(defun qq-core--handle-desync (&rest _arguments)
  "Coalesce a recent-conversation resync after transient event loss."
  (when (and (qq-server-ready-p)
             (qq-core-supports-p 'recent-conversations))
    (when-let* ((instance-id
                 (qq-server-gateway-instance-id)))
      (dolist (account (qq-account-list))
        (when (equal (alist-get 'phase account) "online")
          (let* ((account-id (alist-get 'account_id account))
                 (context (list instance-id account-id)))
            (unless
                (equal
                 context
                 (gethash account-id qq-core--recent-resync-contexts))
              ;; Publish before dispatch because preflight errors can settle
              ;; synchronously.
              (puthash account-id (copy-tree context)
                       qq-core--recent-resync-contexts)
              (qq-core-refresh-recent-conversations
               (apply-partially
                #'qq-core--recent-resync-complete
                account-id context nil)
               (apply-partially
                #'qq-core--recent-resync-complete
                account-id context t)
               nil account-id))))))))

(defun qq-core-activate ()
  "Activate account-scoped native projections for all managed accounts."
  (qq-core--maybe-bootstrap-all)
  t)

(defun qq-core-reset-session-state ()
  "Revoke account projections and request caches."
  (clrhash qq-core--bootstraps)
  (clrhash qq-core--observed-account-phases)
  (clrhash qq-core--recent-resync-contexts)
  (clrhash qq-core--recent-requests)
  (clrhash qq-core--recent-bootstrap-instances)
  (qq-core--revoke-read-operations)
  (qq-request-revoke-all)
  (qq-attachment-reset)
  (qq-remote-media-reset)
  (qq-resource-reset)
  (qq-directory-reset)
  (qq-message-reset-correlations))

(add-hook 'qq-account-registry-changed-hook
          #'qq-core--revoke-stale-read-operations)
(add-hook 'qq-account-registry-changed-hook
          #'qq-request-revoke-stale)
(add-hook 'qq-account-registry-changed-hook
          #'qq-core--handle-account-registry-change t)
(add-hook 'qq-account-registry-ready-hook #'qq-core--handle-ready t)
(add-hook 'qq-account-desync-hook #'qq-core--handle-desync t)

(provide 'qq-core)

;;; qq-core.el ends here
