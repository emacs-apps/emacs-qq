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




(defvar qq-directory--title-expiry-timer nil
  "Next finite title expiration across the account-scoped member pages.")



(defun qq-directory--current-owner ()
  "Return the managed account owning the current UI context."
  (let ((owner (qq-runtime-current-account-id)))
    (unless (and owner (qq-account-get owner))
      (user-error "qq: this operation requires a managed QQ account"))
    owner))





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
  "Validate and commit OWNER's joined-group RESULT inside its App."
  (qq-directory--assert-unique (alist-get 'groups result) 'group_uin "group list")
  (qq-state-apply-groups
   (mapcar (lambda (group) (qq-directory--group-to-state group (qq-account-get owner)))
           (alist-get 'groups result)))
  t)

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
    (title-expire-time . ,(alist-get 'special_title_expire_time member))
    (title-id . ,(alist-get 'title_id member))
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
    (qq-directory--enrich-group-from-members
     owner group-uin raw-members)
    (qq-directory--members-changed owner group-uin)
    page))

(defun qq-directory--members-changed (owner group-uin)
  "Notify OWNER's views that current GROUP-UIN membership changed."
  (qq-directory--schedule-title-expiry)
  (qq-runtime-with-account owner
    (qq-state--emit
     'group-members-refreshed
     :session-key (qq-state-session-key 'group group-uin)
     :group-id group-uin)))

(defun qq-directory-member-active-title (member)
  "Return MEMBER's nonempty, currently active special title.
Absent, zero and uint32 MAX expiration values are displayed without a
bounded lifetime; the native raw expiration remains unchanged in the cache."
  (let ((title (alist-get 'title member))
        (expiry (alist-get 'title-expire-time member)))
    (and (stringp title)
         (not (string-empty-p title))
         (or (null expiry) (eq expiry 0) (eq expiry #xffffffff)
             (and (numberp expiry) (> expiry (float-time))))
         title)))

(defun qq-directory--schedule-title-expiry ()
  "Schedule a redraw for the earliest shared member-title expiration."
  (when (timerp qq-directory--title-expiry-timer)
    (cancel-timer qq-directory--title-expiry-timer))
  (setq qq-directory--title-expiry-timer nil)
  (let ((now (float-time)) next keys)
    (dolist (runtime (qq-runtime-accounts))
      (maphash
       (lambda (resource entry)
         (when (eq (car resource) 'group-members)
           (let ((key (list (qq-runtime-account-id runtime) (cadr resource))))
             (dolist (member (alist-get 'members (plist-get entry :value)))
               (let ((expiry (alist-get 'title-expire-time member)))
                 (when (and (qq-directory-member-active-title member)
                            (numberp expiry) (> expiry now) (< expiry #xffffffff))
                   (cond
                    ((or (null next) (< expiry next))
                     (setq next expiry keys (list key)))
                    ((= expiry next) (cl-pushnew key keys :test #'equal)))))))))
       (qq-state-partition-operations (appkit-app-model (qq-runtime-account-app runtime)))))
    (when next
      (setq qq-directory--title-expiry-timer
            (run-at-time (max 0.01 (- next now)) nil
                         #'qq-directory--expire-member-titles keys)))))

(defun qq-directory--expire-member-titles (keys)
  "Redraw member pages identified by KEYS after their title deadline."
  (setq qq-directory--title-expiry-timer nil)
  (dolist (key keys)
    (when (and (qq-account-get (car key))
               (plist-get (qq-runtime-operation (list 'group-members (cadr key)) (car key)) :value))
      (qq-directory--members-changed (car key) (cadr key))))
  (qq-directory--schedule-title-expiry))



















(defun qq-directory-reset ()
  "Revoke directory reads and cached member pages in every live account App."
  (dolist (runtime (qq-runtime-accounts))
    (dolist (kind '(friends groups group-members group-requests))
      (qq-runtime-forget (list kind) (qq-runtime-account-id runtime))))
  (when (timerp qq-directory--title-expiry-timer)
    (cancel-timer qq-directory--title-expiry-timer))
  (setq qq-directory--title-expiry-timer nil))





























(defun qq-directory--apply-group-member-setting
    (group-uin target-uin field value)
  "Apply confirmed FIELD VALUE to cached TARGET-UIN in GROUP-UIN.

The directory never creates an incomplete page or member from a mutation
receipt."
  (let ((owner (qq-directory--current-owner)))
    (when-let* ((page (plist-get (qq-runtime-operation (list 'group-members group-uin) owner) :value))
                (member
                 (seq-find
                  (lambda (candidate)
                    (equal (alist-get 'user_id candidate) target-uin))
                  (alist-get 'members page))))
      (setf (alist-get field member nil nil #'eq)
            (and (not (string-empty-p value)) value))
      ;; Native title-only updates preserve expiration and the independent
      ;; group-level title-id, even when clearing the special title.
      (qq-directory--members-changed owner group-uin))))







(defun qq-directory--remove-group-member (group-uin target-uin)
  "Remove cached TARGET-UIN from GROUP-UIN exactly once.

Return non-nil only when an existing member was removed.  An absent page or
member is left untouched, and no incomplete directory state is invented."
  (let ((owner (qq-directory--current-owner)))
    (when-let* ((page (plist-get (qq-runtime-operation (list 'group-members group-uin) owner) :value))
                (members (alist-get 'members page))
                (member
                 (seq-find
                  (lambda (candidate)
                    (equal (alist-get 'user_id candidate) target-uin))
                  members)))
      (setf (alist-get 'members page nil nil #'eq) (delq member members))
      (setf (alist-get 'member_count page nil nil #'eq)
            (max 0 (1- (alist-get 'member_count page))))
      (qq-directory--members-changed owner group-uin)
      t)))



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









(defun qq-directory-ensure-group-members (group-uin)
  "Warm GROUP-UIN's shared page without duplicating ready or pending work."
  (unless (and (qq-state-groups-loaded-p) (not (qq-state-group group-uin)))
    (qq-runtime-request (list 'group-members group-uin))))

(defun qq-directory-group-member (group-uin user-id &optional uid)
  "Return the current account's cached GROUP-UIN member by USER-ID or UID.
Never fetch during rendering or copy the entire member page."
  (let* ((owner (qq-runtime-current-account-id))
         (page (plist-get (qq-runtime-operation (list 'group-members group-uin) owner) :value)))
    (qq-server-value-copy
     (seq-find
      (lambda (member)
        (or (and user-id (equal user-id (alist-get 'user_id member)))
            (and uid (equal uid (alist-get 'uid member)))))
      (alist-get 'members page)))))



(defun qq-directory--decision-params (mailbox request decision refusal-message)
  "Validate and describe a native group-request decision."
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
  `((mailbox . ,(symbol-name mailbox))
    (sequence . ,(alist-get 'sequence request))
    (event . ,(copy-tree (alist-get 'event request)))
    (group_uin . ,(alist-get 'group_uin request))
    (decision . ,(symbol-name decision))
    (refusal_message . ,(or refusal-message ""))))

(defun qq-directory--check-decision (result owner mailbox request decision)
  "Validate a decision receipt against its exact account and request."
  (unless (and (qq-server-wire-exact-object-keys-p
                result '(account_id mailbox sequence event group_uin decision))
               (equal (alist-get 'account_id result) owner)
               (equal (alist-get 'mailbox result) (symbol-name mailbox))
               (equal (alist-get 'decision result) (symbol-name decision))
               (cl-every (lambda (key) (equal (alist-get key result) (alist-get key request)))
                         '(sequence event group_uin)))
    (error "qq: Gateway returned an invalid group-request receipt"))
  result)

(defun qq-directory--departed-member-keys ()
  "Return App member resources whose groups are no longer joined."
  (let ((entries (qq-state-partition-operations (appkit-app-model (qq-runtime-app)))) keys)
    (maphash (lambda (key _entry)
               (when (and (eq (car key) 'group-members) (not (qq-state-group (cadr key))))
                 (push key keys))) entries)
    keys))

(qq-runtime-define-operation friends ()
  :method "contact.list_friends" :params `((refresh . ,(if refresh t :false)))
  :commit (progn (qq-directory--project-friends result owner) t)
  :next (qq-core--directory-next))

(qq-runtime-define-operation groups ()
  :method "contact.list_groups" :params `((refresh . ,(if refresh t :false)))
  :commit (qq-directory--project-groups result owner)
  :invalidate (qq-directory--departed-member-keys)
  :next (qq-core--directory-next))

(qq-runtime-define-operation group-members (group-id)
  :method "contact.list_group_members"
  :params `((group_uin . ,group-id) (refresh . ,(if refresh t :false)))
  :commit (qq-directory--project-members result owner group-id)
  :next (progn (qq-directory--schedule-title-expiry) nil))

(qq-runtime-define-operation group-requests (mailbox)
  :method "group_request.list" :params `((mailbox . ,(symbol-name mailbox)))
  :commit (qq-directory--project-group-requests result owner mailbox))

(qq-runtime-define-operation group-decision (mailbox request decision refusal-message)
  :key '(group-decision) :write t :method "group_request.decide"
  :params (qq-directory--decision-params mailbox request decision refusal-message)
  :commit (qq-directory--check-decision result owner mailbox request decision)
  :cancel '((group-requests))
  :next (unless reason '((group-requests main) (group-requests filtered))))

(qq-runtime-define-operation group-setting (group-id field new-value)
  :key (list 'group-setting group-id) :write t
  :method (pcase field ('name "group.set_name") ('remark "group.set_remark")
                 ('whole-mute "group.set_whole_mute") ('pinned "group.set_pinned")
                 (_ (user-error "qq: unsupported group field %S" field)))
  :params `((group_uin . ,group-id)
            (,(if (eq field 'whole-mute) 'enabled field)
             . ,(if (memq field '(pinned whole-mute)) (if new-value t :false) new-value)))
  :commit (progn
            (qq-core--apply-group-setting
             group-id (pcase field ('name 'group_name) ('remark 'group_remark)
                             ('whole-mute 'mute) (_ field))
             (pcase field
               ('whole-mute (let ((mute (copy-tree (alist-get 'mute (qq-state-group group-id)))))
                              (setf (alist-get 'all_until mute) (and new-value #xffffffff)) mute))
               ('pinned (if new-value t :false))
               ('remark (and (not (string-empty-p new-value)) new-value))
               (_ new-value)))
            result)
  :cancel '((groups)))

(qq-runtime-define-operation friend-pinned (user-id pinned)
  :key (list 'friend-pinned user-id) :write t :method "friend.set_pinned"
  :params `((friend_uin . ,user-id) (pinned . ,(if pinned t :false))))

(qq-runtime-define-operation group-clock-in (group-id)
  :write t :method "group.clock_in" :params `((group_uin . ,group-id)))

(qq-runtime-define-operation group-at-all (group-id)
  :method "group.get_at_all_remaining" :params `((group_uin . ,group-id)))

(qq-runtime-define-operation group-leave (group-id)
  :write t :method "group.leave" :params `((group_uin . ,group-id))
  :commit (progn
            (when (qq-state-groups-loaded-p)
              (qq-state-apply-groups
               (seq-remove (lambda (group) (equal (alist-get 'group_id group) group-id))
                           (qq-state-groups))))
            result)
  :cancel '((groups)) :invalidate (list (list 'group-members group-id)))

(qq-runtime-define-operation member-setting (group-id user-id field new-value)
  :key (list 'member-write group-id user-id) :write t
  :method (pcase field ('card "group.set_member_card") ('title "group.set_member_special_title")
                 (_ (user-error "qq: unsupported member field %S" field)))
  :params `((group_uin . ,group-id) (target_uin . ,user-id)
            (,(if (eq field 'title) 'special_title field) . ,new-value))
  :commit (progn (qq-directory--apply-group-member-setting group-id user-id field new-value) result)
  :cancel (list (list 'group-members group-id)))

(qq-runtime-define-operation member-kick (group-id user-id reject-add-request)
  :key (list 'member-write group-id user-id) :write t :method "group.kick_member"
  :params `((group_uin . ,group-id) (target_uin . ,user-id)
            (reject_add_request . ,(if reject-add-request t :false)))
  :commit (progn (qq-directory--remove-group-member group-id user-id) result)
  :cancel (list (list 'group-members group-id)))


(provide 'qq-directory)

;;; qq-directory.el ends here
