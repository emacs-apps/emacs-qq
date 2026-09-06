;;; qq-directory.el --- QQ contact projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Account-scoped adapters for the Gateway friend, group, and group-member
;; methods.  Every request, cache entry, and projection is owned by a stable
;; account ID; changing the UI's selected account never revokes another
;; account's directory work.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-message)
(require 'qq-rpc)
(require 'qq-runtime)
(require 'qq-state)

(defvar qq-directory--active-requests
  (make-hash-table :test #'equal)
  "Newest request keyed by stable account ID and projected resource.")
(defvar qq-directory--member-pages
  (make-hash-table :test #'equal)
  "Group-member pages keyed by stable account ID and exact group UIN.")
(defvar qq-directory--account-phases
  (make-hash-table :test #'equal)
  "Last observed Native Session phase keyed by stable account ID.")

(cl-defstruct (qq-directory--request-record
               (:constructor qq-directory--request-record-create))
  "Exactly-once lifecycle for one newest-owned directory resource request."
  resource
  owner
  (state 'active)
  transport-token
  errback)

(defun qq-directory--current-owner ()
  "Return the managed account owning the current UI context."
  (let ((owner (qq-runtime-current-account-id)))
    (unless (and owner (qq-account-get owner))
      (user-error "qq: this operation requires a managed QQ account"))
    owner))

(defun qq-directory--request-key (owner resource)
  "Return the cache key for OWNER's directory RESOURCE."
  (list owner resource))

(defun qq-directory--member-key (owner group-uin)
  "Return the cache key for OWNER's exact GROUP-UIN member page."
  (list owner group-uin))

(defun qq-directory--assert-unique (items key context)
  "Return ITEMS after asserting unique KEY values in CONTEXT."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (item items)
      (let ((value (alist-get key item)))
        (when (gethash value seen)
          (error "qq: Gateway %s duplicates %s %s" context key value))
        (puthash value t seen))))
  items)

(defun qq-directory--check-friend-relationships (result)
  "Return RESULT after checking relations owned by the friend projection."
  (let ((categories (alist-get 'categories result))
        (friends (alist-get 'friends result))
        (category-ids (make-hash-table :test #'equal)))
    (qq-directory--assert-unique
     categories 'id "friend categories")
    (dolist (category categories)
      (puthash (alist-get 'id category) t category-ids))
    (qq-directory--assert-unique friends 'uid "friend list")
    (qq-directory--assert-unique friends 'uin "friend list")
    (dolist (friend friends)
      (unless (gethash (alist-get 'category_id friend) category-ids)
        (error "qq: Gateway friend belongs to an unknown category"))
      (unless (and (qq-protocol-non-empty-string-p
                    (alist-get 'avatar_url friend))
                   (string-prefix-p "https://"
                                    (alist-get 'avatar_url friend)))
        (error "qq: Gateway friend avatar must be an HTTPS URL"))))
  result)

(defun qq-directory--friend-to-state (friend category-name)
  "Project native FRIEND with CATEGORY-NAME to shared directory shape."
  `((user_id . ,(alist-get 'uin friend))
    (uid . ,(alist-get 'uid friend))
    (nickname . ,(alist-get 'nickname friend))
    (remark . ,(alist-get 'remark friend))
    (category_id . ,(alist-get 'category_id friend))
    (category_name . ,category-name)
    (personal_sign . ,(alist-get 'personal_sign friend))
    (qid . ,(alist-get 'qid friend))
    (age . ,(alist-get 'age friend))
    (gender . ,(alist-get 'gender friend))
    (avatar_url . ,(alist-get 'avatar_url friend))))

(defun qq-directory--friends-to-state (result)
  "Convert friend-list RESULT to ordered shared categories."
  (let ((friends (alist-get 'friends result)))
    (mapcar
     (lambda (category)
       (let* ((category-id (alist-get 'id category))
              (category-name (alist-get 'name category))
              (members
               (delq
                nil
                (mapcar
                 (lambda (friend)
                   (when (= (alist-get 'category_id friend) category-id)
                     (qq-directory--friend-to-state
                      friend category-name)))
                 friends))))
         `((category_id . ,category-id)
           (sort_id . ,(alist-get 'sort_id category))
           (name . ,category-name)
           (member_count . ,(alist-get 'member_count category))
           (friends . ,members))))
     (alist-get 'categories result))))

(defun qq-directory--preflight-peer-identities (owner identities)
  "Preflight UID/UIN IDENTITIES for projected account OWNER."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (identity identities)
      (let ((uid (car identity))
            (uin (cdr identity)))
        (when-let* ((page-uin (gethash uid seen)))
          (unless (equal page-uin uin)
            (error "qq: Gateway directory maps one UID to multiple UINs")))
        (puthash uid uin seen)
        (qq-message--validate-peer-identity owner uid uin))))
  identities)

(defun qq-directory--project-friends (result owner)
  "Project friend-list RESULT for OWNER into shared state."
  (qq-directory--check-friend-relationships result)
  (let* ((friends (alist-get 'friends result))
         (identities
          (mapcar (lambda (friend)
                    (cons (alist-get 'uid friend) (alist-get 'uin friend)))
                  friends))
         (categories (qq-directory--friends-to-state result)))
    (qq-directory--preflight-peer-identities owner identities)
    (qq-state-apply-friend-categories categories)
    (dolist (identity identities)
      (qq-message--remember-peer-identity
       owner (car identity) (cdr identity)))
    categories))

(defun qq-directory--group-notification-mode (group)
  "Return GROUP's known notification-mode symbol, or nil when omitted."
  (when-let* ((entry (assq 'message_notify_mode group)))
    (let ((mode (cdr entry)))
      (unless (member mode '("notify" "assistant" "shield" "receive"))
        (error "qq: Gateway group has an invalid notification mode"))
      (intern mode))))

(defun qq-directory--group-to-state (group account)
  "Project native GROUP using its owning ACCOUNT identity."
  (let ((notify-mode (qq-directory--group-notification-mode group)))
    `((group_id . ,(alist-get 'group_uin group))
      (group_name . ,(alist-get 'name group))
      (group_remark . ,(alist-get 'remark group))
      (owner_uid . ,(alist-get 'owner_uid group))
      (member_count . ,(alist-get 'member_count group))
      (max_member_count . ,(alist-get 'member_max group))
      (created_at . ,(alist-get 'created_time group))
      (description . ,(alist-get 'description group))
      (question . ,(alist-get 'question group))
      (announcement . ,(alist-get 'announcement group))
      (last_speak_time . ,(alist-get 'last_speak_time group))
      (latest_sequence . ,(and (alist-get 'latest_sequence group)
                               (format "%s" (alist-get 'latest_sequence group))))
      ,@(when notify-mode
          `((message-notify-mode . ,notify-mode)
            (muted-p . ,(and (not (eq notify-mode 'notify)) t))))
      (self_permission
       . ,(and (alist-get 'owner_uid group)
               (equal (alist-get 'owner_uid group) (alist-get 'uid account))
               "owner")))))

(defun qq-directory--project-groups (result owner)
  "Project joined-group RESULT into shared state."
  (let ((account (qq-account-get owner))
        groups)
    (qq-directory--assert-unique
     (alist-get 'groups result) 'group_uin "group list")
    (setq groups
          (mapcar (lambda (group)
                    (qq-directory--group-to-state group account))
                  (alist-get 'groups result)))
    (qq-state-apply-groups groups)
    groups))

(defun qq-directory--permission-role (permission)
  "Return UI role for exact native PERMISSION, or nil when unknown."
  (pcase (alist-get 'kind permission)
    ((or "member" "owner" "admin") (alist-get 'kind permission))))

(defun qq-directory--member-to-state (member group-uin group-name)
  "Project native MEMBER for GROUP-UIN and GROUP-NAME."
  `((user_id . ,(alist-get 'uin member))
    (uid . ,(alist-get 'uid member))
    (nickname . ,(alist-get 'nickname member))
    (card . ,(alist-get 'member_card member))
    (remark)
    (qid)
    (title . ,(alist-get 'special_title member))
    (role . ,(qq-directory--permission-role
              (alist-get 'permission member)))
    (robot)
    (group_id . ,group-uin)
    (group_name . ,group-name)
    (level . ,(alist-get 'level member))
    (join_timestamp . ,(alist-get 'join_timestamp member))
    (last_message_timestamp . ,(alist-get 'last_message_timestamp member))
    (shut_up_timestamp . ,(alist-get 'shut_up_timestamp member))
    (native_permission
     . ,(qq-server-value-copy (alist-get 'permission member)))
    (is_friend . ,(and (qq-state-friend-categories-loaded-p)
                       (qq-state-friend (alist-get 'uin member))
                       t))))

(defun qq-directory--enrich-group-from-members
    (owner group-uin raw-members)
  "Enrich OWNER's cached GROUP-UIN with exact ownership from RAW-MEMBERS."
  (when-let* ((group (qq-state-group group-uin)))
    (let* ((self-uin (alist-get 'uin (qq-account-get owner)))
           (owner
            (seq-find
             (lambda (member)
               (equal (alist-get 'kind (alist-get 'permission member))
                      "owner"))
             raw-members))
           (self
            (seq-find (lambda (member)
                        (equal (alist-get 'uin member) self-uin))
                      raw-members))
           (self-role
            (and self
                 (qq-directory--permission-role
                  (alist-get 'permission self)))))
      (when owner
        (setf (alist-get 'owner_id group nil nil #'eq)
              (alist-get 'uin owner))
        (setf (alist-get 'owner_uid group nil nil #'eq)
              (alist-get 'uid owner)))
      (when self-role
        (setf (alist-get 'self_permission group nil nil #'eq) self-role))
      (qq-state-apply-groups
       (mapcar (lambda (candidate)
                 (if (equal (alist-get 'group_id candidate) group-uin)
                     group
                   candidate))
               (qq-state-groups))))))

(defun qq-directory--project-members (result owner group-uin)
  "Project GROUP-UIN member RESULT for OWNER and return mapped members."
  (let* ((group (qq-state-group group-uin))
         (group-name (and group (alist-get 'group_name group)))
         (raw-members (alist-get 'members result))
         (identities
          (mapcar (lambda (member)
                    (cons (alist-get 'uid member) (alist-get 'uin member)))
                  raw-members))
         (members
          (mapcar (lambda (member)
                    (qq-directory--member-to-state
                     member group-uin group-name))
                  raw-members))
         (page
          `((group_id . ,group-uin)
            (members . ,members)
            (member_count . ,(alist-get 'member_count result))
            (member_list_change_sequence
             . ,(alist-get 'member_list_change_sequence result))
            (member_card_sequence
             . ,(alist-get 'member_card_sequence result)))))
    (qq-directory--assert-unique
     raw-members 'uid "group-member list")
    (qq-directory--assert-unique
     raw-members 'uin "group-member list")
    (qq-directory--preflight-peer-identities owner identities)
    (dolist (identity identities)
      (qq-message--remember-peer-identity
       owner (car identity) (cdr identity)))
    (puthash (qq-directory--member-key owner group-uin)
             (qq-server-value-copy page)
             qq-directory--member-pages)
    (qq-directory--enrich-group-from-members
     owner group-uin raw-members)
    members))

(defun qq-directory--request-active-p (request)
  "Return non-nil when directory REQUEST still owns asynchronous work."
  (and (qq-directory--request-record-p request)
       (eq (qq-directory--request-record-state request) 'active)))

(defun qq-directory--request-current-p (request)
  "Return non-nil when REQUEST still owns its resource and account slot."
  (let ((owner (qq-directory--request-record-owner request))
        (resource (qq-directory--request-record-resource request)))
    (and (qq-directory--request-active-p request)
         (eq request
             (gethash
              (qq-directory--request-key owner resource)
              qq-directory--active-requests))
         (qq-account-get owner))))

(defun qq-directory--finish-request (request state)
  "Move active directory REQUEST to terminal STATE exactly once."
  (when (qq-directory--request-active-p request)
    (setf (qq-directory--request-record-state request) state
          (qq-directory--request-record-transport-token request) nil)
    (let* ((owner (qq-directory--request-record-owner request))
           (resource
            (qq-directory--request-record-resource request))
           (key (qq-directory--request-key owner resource)))
      (when (eq request
                (gethash key qq-directory--active-requests))
        (remhash key qq-directory--active-requests)))
    t))

(defun qq-directory--cancel-transport (token)
  "Best-effort cancel directory transport TOKEN."
  (when token
    (condition-case error-data
        (qq-server-cancel token)
      (error
       (message "qq: directory request cancellation failed: %s"
                (error-message-string error-data))))))

(defun qq-directory--cancel-request
    (request &optional code message)
  "Cancel active REQUEST and optionally report client CODE and MESSAGE."
  (when (qq-directory--request-active-p request)
    (let ((token
           (qq-directory--request-record-transport-token request))
          (errback (qq-directory--request-record-errback request)))
      ;; Revoke before transport or user code: both may synchronously reenter.
      (qq-directory--finish-request request 'cancelled)
      (qq-directory--cancel-transport token)
      (when code
        (qq-rpc-client-error
         errback code "%s" (or message "Gateway directory request cancelled")))
      t)))

(defun qq-directory--cancel-resource
    (owner resource &optional code message)
  "Cancel OWNER's current request for RESOURCE."
  (let ((request
          (gethash (qq-directory--request-key owner resource)
                   qq-directory--active-requests)))
    (when (qq-directory--request-record-p request)
      (qq-directory--cancel-request request code message))))

(defun qq-directory--cancel-all-requests
    (&optional code message)
  "Cancel all active directory requests, optionally reporting CODE and MESSAGE."
  (let (requests)
    (maphash (lambda (_resource request) (push request requests))
             qq-directory--active-requests)
    ;; Publish revocation before cancellation callbacks can start replacements.
    (clrhash qq-directory--active-requests)
    (dolist (request requests)
      (when (qq-directory--request-record-p request)
        (qq-directory--cancel-request request code message)))))

(defun qq-directory--cancel-owner-requests
    (owner &optional code message)
  "Cancel every active directory request owned by OWNER."
  (let (requests)
    (maphash
     (lambda (_key request)
       (when (and (qq-directory--request-record-p request)
                  (equal
                   owner
                   (qq-directory--request-record-owner request)))
         (push request requests)))
     qq-directory--active-requests)
    (dolist (request requests)
      (qq-directory--cancel-request request code message))))

(defun qq-directory--drop-owner-member-pages (owner)
  "Drop all cached group-member pages owned by OWNER."
  (let (keys)
    (maphash
     (lambda (key _page)
       (when (equal owner (car key))
         (push key keys)))
     qq-directory--member-pages)
    (dolist (key keys)
      (remhash key qq-directory--member-pages))))

(defun qq-directory-reset ()
  "Revoke native directory request ownership and member caches."
  (qq-directory--cancel-all-requests
   "gateway_reset" "Gateway directory state was reset")
  (clrhash qq-directory--member-pages)
  (clrhash qq-directory--account-phases)
  nil)

(defun qq-directory--handle-account-registry-change
    (reason account-id)
  "Revoke directory work made stale by registry REASON for ACCOUNT-ID."
  (cond
   ((eq reason 'ready)
    (qq-directory-reset)
    (dolist (account (qq-account-list))
      (puthash (alist-get 'account_id account)
               (alist-get 'phase account)
               qq-directory--account-phases)))
   (account-id
    (let* ((account (qq-account-get account-id))
           (old-phase
            (gethash account-id qq-directory--account-phases))
           (new-phase (and account (alist-get 'phase account)))
           (online-boundary-p
            (and old-phase
                 (not (equal old-phase new-phase))
                 (or (equal old-phase "online")
                     (equal new-phase "online")))))
      (when (or (null account) online-boundary-p)
        (qq-directory--cancel-owner-requests
         account-id
         (if account "native_session_changed" "account_removed")
         (if account
             "QQ Native Session changed during directory request"
           "QQ account was removed during directory request"))
        (qq-directory--drop-owner-member-pages account-id))
      (if account
          (puthash account-id new-phase
                   qq-directory--account-phases)
        (remhash account-id qq-directory--account-phases))))))

(defun qq-directory--begin-request (resource owner errback)
  "Publish and return the newest request record for RESOURCE and OWNER."
  (let* ((key (qq-directory--request-key owner resource))
         (previous
          (gethash key qq-directory--active-requests))
         (request
           (qq-directory--request-record-create
            :resource resource :owner (copy-sequence owner)
            :errback errback)))
    ;; Publish first so a predecessor's errback sees its replacement.
    (puthash key request qq-directory--active-requests)
    (when (qq-directory--request-record-p previous)
      (qq-directory--cancel-request
       previous "superseded_request"
       "Gateway directory request was superseded"))
    request))

(defun qq-directory--request
    (resource method params projector callback errback)
  "Run one newest-owned directory RESOURCE request using native METHOD.

PARAMS are sent as-is.  PROJECTOR receives the owned domain result and exact
account owner.  CALLBACK receives the projected value; ERRBACK follows
Gateway error conventions."
  (let* ((owner (qq-directory--current-owner))
         (request
           (qq-directory--begin-request resource owner errback)))
    (condition-case error-data
        (when (qq-directory--request-current-p request)
          (let ((token
                 (qq-rpc-call
                  method (append `((account_id . ,owner)) params)
                  :current-p
                  (lambda ()
                    (qq-directory--request-current-p request))
                  :stale-code "superseded_request"
                  :stale-message
                  "Gateway directory request was superseded or changed owner"
                  :projector
                  (lambda (result)
                    (qq-runtime-with-account owner
                      (funcall projector result owner)))
                  :callback
                  (lambda (value)
                    (when (qq-directory--finish-request
                           request 'settled)
                      (qq-runtime-with-account owner
                        (qq-rpc-invoke callback value))))
                  :errback
                  (lambda (body reason)
                    (when (qq-directory--finish-request
                           request 'failed)
                      (qq-runtime-with-account owner
                        (qq-rpc-invoke errback body reason)))))))
            (when (and token
                       (qq-directory--request-current-p request))
              (setf
               (qq-directory--request-record-transport-token request)
               token))
            token))
      ((error quit)
       (qq-directory--finish-request request 'failed)
       (signal (car error-data) (cdr error-data))))))

(defun qq-directory-refresh-friends
    (&optional callback errback refresh)
  "Refresh native friends and call CALLBACK with ordered categories.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Native Session to replace its contact cache."
  (interactive
   (list (lambda (categories)
           (message "qq: loaded %d Gateway friend categories"
                    (length categories)))
         (lambda (_body reason)
           (message "qq: Gateway friend refresh failed: %s" reason))
         (and current-prefix-arg t)))
  (qq-directory--request
   'friends "contact.list_friends"
   `((refresh . ,(if refresh t :false)))
   #'qq-directory--project-friends
   callback errback))

(defun qq-directory-refresh-groups
    (&optional callback errback refresh)
  "Refresh native joined groups and call CALLBACK with mapped groups.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Native Session to replace its contact cache."
  (interactive
   (list (lambda (groups)
           (message "qq: loaded %d Gateway groups" (length groups)))
         (lambda (_body reason)
           (message "qq: Gateway group refresh failed: %s" reason))
         (and current-prefix-arg t)))
  (qq-directory--request
   'groups "contact.list_groups"
   `((refresh . ,(if refresh t :false)))
   #'qq-directory--project-groups
   callback errback))

(defun qq-directory-set-friend-pinned
    (friend-uin pinned &optional callback errback)
  "Set FRIEND-UIN's conversation PINNED state through the native service."
  (unless (qq-protocol-uint64-decimal-p friend-uin)
    (user-error "qq: Friend pinned state requires an exact decimal UIN"))
  (setq pinned (if pinned t :false))
  (let ((owner (qq-directory--current-owner)))
    (qq-rpc-call
     "friend.set_pinned"
     `((account_id . ,owner)
       (friend_uin . ,friend-uin)
       (pinned . ,pinned))
     :current-p (lambda () (qq-account-get owner))
     :stale-code "invalid_gateway_result"
     :stale-message "QQ account was removed during friend setting"
     :callback callback
     :errback errback)))

(defun qq-directory--set-group-setting
    (method group-uin field value callback errback)
  "Send group setting METHOD with FIELD and VALUE for GROUP-UIN."
  (unless (qq-protocol-uint64-decimal-p group-uin)
    (user-error "qq: Group setting requires an exact decimal group UIN"))
  (let ((owner (qq-directory--current-owner)))
    (qq-rpc-call
     method
     `((account_id . ,owner)
       (group_uin . ,group-uin)
       (,field . ,value))
     :current-p (lambda () (qq-account-get owner))
     :stale-code "invalid_gateway_result"
     :stale-message "QQ account was removed during group setting"
     :callback callback
     :errback errback)))

(defun qq-directory-set-group-name
    (group-uin name &optional callback errback)
  "Set GROUP-UIN's public NAME through the native service."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "qq: Group name must be a non-empty string"))
  (qq-directory--set-group-setting
   "group.set_name" group-uin 'name name callback errback))

(defun qq-directory-set-group-remark
    (group-uin remark &optional callback errback)
  "Set or clear GROUP-UIN's account-local REMARK through the Gateway."
  (unless (stringp remark)
    (user-error "qq: Group remark must be a string"))
  (qq-directory--set-group-setting
   "group.set_remark" group-uin 'remark remark callback errback))

(defun qq-directory-set-group-whole-mute
    (group-uin enabled &optional callback errback)
  "Set GROUP-UIN's whole-group mute state through the native service."
  (setq enabled (and enabled t))
  (qq-directory--set-group-setting
   "group.set_whole_mute" group-uin 'enabled
   (if enabled t :false) callback errback))

(defun qq-directory-set-group-pinned
    (group-uin pinned &optional callback errback)
  "Set GROUP-UIN's conversation PINNED state through the Gateway."
  (setq pinned (and pinned t))
  (qq-directory--set-group-setting
   "group.set_pinned" group-uin 'pinned
   (if pinned t :false) callback errback))

(defun qq-directory-clock-in-group
    (group-uin &optional callback errback)
  "Clock the current buffer's QQ account into exact GROUP-UIN."
  (unless (qq-protocol-uint64-decimal-p group-uin)
    (user-error "qq: Group clock-in requires an exact group UIN"))
  (let ((owner (qq-directory--current-owner)))
    (qq-rpc-call
     "group.clock_in"
     `((account_id . ,owner)
       (group_uin . ,group-uin))
     :current-p (lambda () (qq-account-get owner))
     :stale-code "invalid_gateway_result"
     :stale-message "QQ account was removed during group clock-in"
     :callback callback
     :errback errback)))

(defun qq-directory-get-group-at-all-remaining
    (group-uin &optional callback errback)
  "Fetch live @all availability and quotas for exact GROUP-UIN."
  (unless (qq-protocol-uint64-decimal-p group-uin)
    (user-error "qq: Group @all quota requires an exact group UIN"))
  (let ((owner (qq-directory--current-owner)))
    (qq-rpc-call
     "group.get_at_all_remaining"
     `((account_id . ,owner)
       (group_uin . ,group-uin))
     :current-p (lambda () (qq-account-get owner))
     :stale-code "invalid_gateway_result"
     :stale-message "QQ account was removed during group @all query"
     :callback callback
     :errback errback)))

(defun qq-directory-leave-group
    (group-uin &optional callback errback)
  "Leave exact GROUP-UIN through the current buffer's QQ account.

This method cannot dismiss a group.  A successful receipt revokes cached
member data and any older group or member request that could reintroduce the
departed group."
  (unless (qq-protocol-uint64-decimal-p group-uin)
    (user-error "qq: Group leave requires an exact group UIN"))
  (let ((owner (qq-directory--current-owner)))
    (qq-rpc-call
     "group.leave"
     `((account_id . ,owner)
       (group_uin . ,group-uin))
     :current-p
     (lambda () (qq-account-get owner))
     :stale-code "invalid_gateway_result"
     :stale-message "QQ account was removed during group leave"
     :projector
     (lambda (receipt)
       (qq-runtime-with-account owner
         (qq-directory--cancel-resource
          owner 'groups "superseded_request"
          "Gateway group list was invalidated by leaving a group")
         (qq-directory--cancel-resource
          owner (cons 'group-members group-uin) "superseded_request"
          "Gateway group-member list was invalidated by leaving the group")
         (remhash (qq-directory--member-key owner group-uin)
                  qq-directory--member-pages)
         receipt))
     :callback callback
     :errback errback)))

(defun qq-directory--apply-group-member-setting
    (group-uin target-uin field value)
  "Apply confirmed FIELD VALUE to cached TARGET-UIN in GROUP-UIN.

The directory never creates an incomplete page or member from a mutation
receipt."
  (let ((owner (qq-directory--current-owner)))
    (when-let* ((page (gethash
                       (qq-directory--member-key owner group-uin)
                       qq-directory--member-pages))
                (member
                 (seq-find
                  (lambda (candidate)
                    (equal (alist-get 'user_id candidate) target-uin))
                  (alist-get 'members page))))
      (setf (alist-get field member nil nil #'eq)
            (and (not (string-empty-p value)) value)))))

(defun qq-directory--set-group-member-setting
    (method group-uin target-uin field cache-field value callback errback)
  "Send group-member setting METHOD and update its projected CACHE-FIELD."
  (unless (qq-protocol-uint64-decimal-p group-uin)
    (user-error "qq: Group-member setting requires an exact group UIN"))
  (unless (qq-protocol-uint64-decimal-p target-uin)
    (user-error "qq: Group-member setting requires an exact target UIN"))
  (unless (stringp value)
    (user-error "qq: Group-member setting value must be a string"))
  (let ((owner (qq-directory--current-owner)))
    (qq-rpc-call
     method
     `((account_id . ,owner)
       (group_uin . ,group-uin)
       (target_uin . ,target-uin)
       (,field . ,value))
     :current-p (lambda () (qq-account-get owner))
     :stale-code "invalid_gateway_result"
     :stale-message
     "QQ account was removed during group-member setting"
     :projector
     (lambda (receipt)
       (qq-runtime-with-account owner
         (qq-directory--apply-group-member-setting
          group-uin target-uin cache-field value)
         receipt))
     :callback callback
     :errback errback)))

(defun qq-directory-set-group-member-card
    (group-uin target-uin card &optional callback errback)
  "Set or clear TARGET-UIN's CARD in GROUP-UIN through the Gateway."
  (qq-directory--set-group-member-setting
   "group.set_member_card" group-uin target-uin 'card 'card card
   callback errback))

(defun qq-directory-set-group-member-special-title
    (group-uin target-uin special-title &optional callback errback)
  "Set or clear TARGET-UIN's SPECIAL-TITLE in GROUP-UIN through Gateway."
  (qq-directory--set-group-member-setting
   "group.set_member_special_title" group-uin target-uin
   'special_title 'title special-title callback errback))

(defun qq-directory--remove-group-member (group-uin target-uin)
  "Remove cached TARGET-UIN from GROUP-UIN exactly once.

Return non-nil only when an existing member was removed.  An absent page or
member is left untouched, and no incomplete directory state is invented."
  (let ((owner (qq-directory--current-owner)))
    (when-let* ((page (gethash
                       (qq-directory--member-key owner group-uin)
                       qq-directory--member-pages))
                (members (alist-get 'members page))
                (member
                 (seq-find
                  (lambda (candidate)
                    (equal (alist-get 'user_id candidate) target-uin))
                  members)))
      (setf (alist-get 'members page nil nil #'eq) (delq member members))
      (setf (alist-get 'member_count page nil nil #'eq)
            (max 0 (1- (alist-get 'member_count page))))
      t)))

(defun qq-directory-kick-group-member
    (group-uin target-uin reject-add-request &optional callback errback)
  "Remove TARGET-UIN from GROUP-UIN through the native service."
  (unless (qq-protocol-uint64-decimal-p group-uin)
    (user-error "qq: Group kick requires an exact group UIN"))
  (unless (qq-protocol-uint64-decimal-p target-uin)
    (user-error "qq: Group kick requires an exact target UIN"))
  (let* ((owner (qq-directory--current-owner))
         (wire-reject (if reject-add-request t :false)))
    (qq-rpc-call
     "group.kick_member"
     `((account_id . ,owner)
       (group_uin . ,group-uin)
       (target_uin . ,target-uin)
       (reject_add_request . ,wire-reject))
     :current-p (lambda () (qq-account-get owner))
     :stale-code "invalid_gateway_result"
     :stale-message "QQ account was removed during group-member kick"
     :projector
     (lambda (receipt)
       (qq-runtime-with-account owner
         (qq-directory--remove-group-member group-uin target-uin)
         receipt))
     :callback callback
     :errback errback)))

(defun qq-directory--group-request-tag-p (value kinds)
  "Return non-nil when VALUE is a closed tagged union in KINDS."
  (and (listp value)
       (let ((kind (alist-get 'kind value)))
         (if (member kind kinds)
             (qq-server-wire-exact-object-keys-p value '(kind))
           (and (equal kind "unknown")
                (qq-server-wire-exact-object-keys-p value '(kind code))
                (qq-protocol-uint32-p (alist-get 'code value)))))))

(defun qq-directory--group-request-user-p (value)
  "Return non-nil when VALUE is a closed group-request user."
  (or (null value)
      (and (qq-server-wire-exact-object-keys-p value '(uid name))
           (qq-protocol-non-empty-string-p (alist-get 'uid value))
           (stringp (alist-get 'name value)))))

(defun qq-directory--group-request-p (request)
  "Return non-nil when REQUEST is one closed native group request."
  (and
   (qq-server-wire-exact-object-keys-p
    request
    '(sequence event state group_uin group_name target inviter operator
      comment actionable))
   (qq-protocol-uint64-decimal-p (alist-get 'sequence request))
   (qq-directory--group-request-tag-p
    (alist-get 'event request)
    '("user_join" "account_invited" "member_invited"))
   (qq-directory--group-request-tag-p
    (alist-get 'state request)
    '("no_action" "pending" "processed"))
   (qq-protocol-uint64-decimal-p (alist-get 'group_uin request))
   (stringp (alist-get 'group_name request))
   (qq-directory--group-request-user-p (alist-get 'target request))
   (qq-directory--group-request-user-p (alist-get 'inviter request))
   (qq-directory--group-request-user-p (alist-get 'operator request))
   (stringp (alist-get 'comment request))
   (memq (alist-get 'actionable request) '(t :false))))

(defun qq-directory--project-group-requests (result owner mailbox)
  "Project owned group-request RESULT for OWNER and MAILBOX."
  (let ((wire-mailbox (symbol-name mailbox))
        (requests (alist-get 'requests result))
        (cursor (alist-get 'new_latest_sequence result)))
    (unless
        (and
         (qq-server-wire-exact-object-keys-p
          result '(account_id mailbox new_latest_sequence requests))
         (equal (alist-get 'account_id result) owner)
         (equal (alist-get 'mailbox result) wire-mailbox)
         (or (null cursor) (qq-protocol-uint64-decimal-p cursor))
         (listp requests)
         (cl-every #'qq-directory--group-request-p requests))
      (error "qq: Gateway returned an invalid %s group-request page" mailbox))
    (copy-tree result)))

(defun qq-directory-list-group-requests
    (mailbox callback &optional errback)
  "List native group requests in MAILBOX and call CALLBACK.

MAILBOX is `main' or `filtered'."
  (unless (memq mailbox '(main filtered))
    (user-error "qq: unsupported group-request mailbox %S" mailbox))
  (qq-directory--request
   (cons 'group-requests mailbox) "group_request.list"
   `((mailbox . ,(symbol-name mailbox)))
   (lambda (result owner)
     (qq-directory--project-group-requests result owner mailbox))
   callback errback))

(defun qq-directory-decide-group-request
    (mailbox request decision refusal-message callback &optional errback)
  "Apply DECISION to native group REQUEST in MAILBOX and call CALLBACK.

MAILBOX is `main' or `filtered'.  DECISION is `accept' or `reject'.
REFUSAL-MESSAGE is used only for rejection."
  (unless (memq mailbox '(main filtered))
    (user-error "qq: unsupported group-request mailbox %S" mailbox))
  (unless (and (qq-directory--group-request-p request)
               (eq (alist-get 'actionable request) t))
    (user-error "qq: group request is not actionable"))
  (unless (memq decision '(accept reject))
    (user-error "qq: unsupported group-request decision %S" decision))
  (when (and (eq decision 'accept)
             (not (string-empty-p (or refusal-message ""))))
    (user-error "qq: accepted group request cannot carry a refusal message"))
  (let* ((owner (qq-directory--current-owner))
         (mailbox (symbol-name mailbox))
         (sequence (alist-get 'sequence request))
         (event (alist-get 'event request))
         (group-uin (alist-get 'group_uin request))
         (wire-decision (symbol-name decision))
         (refusal-message (or refusal-message "")))
    (qq-rpc-call
     "group_request.decide"
     `((account_id . ,owner)
       (mailbox . ,mailbox)
       (sequence . ,sequence)
       (event . ,(copy-tree event))
       (group_uin . ,group-uin)
       (decision . ,wire-decision)
       (refusal_message . ,refusal-message))
     :current-p (lambda () (qq-account-get owner))
     :stale-code "invalid_gateway_result"
     :stale-message "QQ account was removed during group-request decision"
     :projector
     (lambda (receipt)
       (unless
           (and
            (qq-server-wire-exact-object-keys-p
             receipt '(account_id mailbox sequence event group_uin decision))
            (equal (alist-get 'account_id receipt) owner)
            (equal (alist-get 'mailbox receipt) mailbox)
            (equal (alist-get 'sequence receipt) sequence)
            (equal (alist-get 'event receipt) event)
            (equal (alist-get 'group_uin receipt) group-uin)
            (equal (alist-get 'decision receipt) wire-decision))
         (error "qq: Gateway returned an invalid group-request receipt"))
       (copy-tree receipt))
     :callback callback
     :errback errback)))

(defun qq-directory-list-group-members
    (group-uin callback &optional errback refresh)
  "List exact GROUP-UIN members and call CALLBACK with mapped members.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Native Session to replace its member cache."
  (unless (qq-protocol-uint64-decimal-p group-uin)
    (user-error "qq: Group UIN must be an exact decimal string"))
  (qq-directory--request
   (cons 'group-members group-uin) "contact.list_group_members"
   `((group_uin . ,group-uin)
     (refresh . ,(if refresh t :false)))
   (lambda (result owner)
     (qq-directory--project-members result owner group-uin))
   callback errback))

(defun qq-directory-group-member-page (group-uin)
  "Return the current account's cached member page for exact GROUP-UIN."
  (let ((owner (qq-directory--current-owner)))
    (qq-server-value-copy
     (gethash (qq-directory--member-key owner group-uin)
              qq-directory--member-pages))))

(add-hook 'qq-account-registry-changed-hook
          #'qq-directory--handle-account-registry-change)

(provide 'qq-directory)

;;; qq-directory.el ends here
