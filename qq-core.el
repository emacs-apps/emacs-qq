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

(defvar qq-core--observed-account-phases (make-hash-table :test #'equal)
  "Last observed Native Session phase keyed by stable account ID.")

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

(defun qq-core--recent-row-state-entry (state page row account)
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
              (car (qq-message--normalize-dataline-message state
                                                           account-id message))
            (qq-message-normalize-snapshot state
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

(defun qq-core--apply-recent-page (state page observation-token)
  "Normalize and apply recent PAGE for OBSERVATION-TOKEN."
  (let ((value (progn
                 (let ((account (qq-account-get (alist-get 'account_id page))))
                   (unless (and account
                                (equal (alist-get 'account_id account)
                                       (alist-get 'account_id page)))
                     (error "qq: recent page no longer belongs to a managed account slot"))
                   (qq-state--step state (qq-state-apply-recent-conversations state
                                                                              (seq-keep
                                                                               (lambda (row)
                                                                                 (when (qq-core--recent-row-projectable-p row account)
                                                                                   (qq-core--recent-row-state-entry state page row account)))
                                                                               (alist-get 'conversations page))
                                                                              observation-token))))))
    (cons state value)))

(defun qq-core--group-profile-from-state (group-id)
  "Return a group-profile projection for exact GROUP-ID, or nil."
  (when-let* ((group (qq-state-group (qq-runtime-state) group-id)))
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
      (pinned . ,(alist-get 'pinned
                            (qq-state-session (qq-runtime-state) (qq-state-session-key 'group group-id))))
      (mute . ,(copy-tree (alist-get 'mute group)))
      (join . ,(copy-tree (alist-get 'join group)))
      (self_permission . ,(alist-get 'self_permission group))
      (category . ,(copy-tree (alist-get 'category group)))
      (grade . ,(alist-get 'grade group))
      (certification . ,(copy-tree (alist-get 'certification group)))
      (school . ,(copy-tree (alist-get 'school group)))
      (location . ,(copy-tree (alist-get 'location group)))
      (has_custom_avatar . ,(alist-get 'has_custom_avatar group)))))

(defun qq-core-refresh ()
  "Refresh the current account's directory and recent conversations."
  (interactive)
  (qq-runtime-require-account-id "refreshing QQ")
  (if (and (equal (alist-get 'phase (qq-account-get (qq-runtime-current-account-id))) "online")
           (qq-core-supports-p 'contacts))
      (dolist (name '(friends groups))
        (qq-runtime-request (qq-runtime-request-create name) t))
    (when (qq-core-supports-p 'recent-conversations)
      (qq-runtime-request (qq-runtime-request-create 'recent) t))))

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

(defun qq-core--apply-group-setting (state group-id field value)
  "Apply confirmed group FIELD VALUE for GROUP-ID to shared state."
  (let ((value (progn
                 (let ((groups (qq-state-groups state))
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
                     (qq-state--step state (qq-state-apply-groups state groups)))
                   changed))))
    (cons state value)))

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
      (let* ((messages (qq-state-session-messages (qq-runtime-state) candidate-session))
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

(defun qq-core--authored-message-at-sequence (session-key sequence)
  "Return SESSION-KEY's authored row carrying SEQUENCE, or nil."
  (seq-find
   (lambda (message)
     (and (not (qq-state-service-message-p message))
          (equal (alist-get 'message-seq message) sequence)))
   (qq-state-session-messages (qq-runtime-state) session-key)))

(defconst qq-core-history-port-version qq-message-history-port-version
  "Version of the conversation-neutral Gateway history contract.")

(defun qq-core-fetch-history-page
    (session-key cursor direction callback
                 &optional errback count lifecycle-owner)
  "Fetch raw history data for SESSION-KEY's account App.
CALLBACK receives an uncommitted result for `qq-message--commit-history-result'.
CURSOR is nil for the latest page or an opaque earlier cursor; DIRECTION is
`older' or `newer'.  ERRBACK receives failure details and COUNT limits rows.
LIFECYCLE-OWNER optionally owns cancellation of the returned request."
  (unless (memq direction '(older newer))
    (user-error "qq: History direction must be older or newer"))
  (setq count (min 100 (max 1 (or count qq-history-fetch-count))))
  (qq-core--start-request
   (lambda (success failure)
     (qq-message--request-history-page
      session-key cursor direction success failure count))
   callback errback
   :lifecycle-owner lifecycle-owner))

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
    (read-receipt "message.mark_read")
    (read-all "conversation.mark_read"))
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

(defun qq-core--maybe-bootstrap-account (owner)
  "Ensure OWNER's shared directory and recent data are loaded."
  (when (and (qq-server-ready-p) (equal (alist-get 'phase (qq-account-get owner)) "online"))
    (qq-runtime-with-account owner
                             (if (qq-core-supports-p 'contacts)
                                 (dolist (name '(friends groups))
                                   (unless (qq-runtime-operation (list name))
                                     (qq-runtime-request (qq-runtime-request-create name) t)))
                               (when (qq-core-supports-p 'recent-conversations)
                                 (qq-runtime-request (qq-runtime-request-create 'recent)))))))

(defun qq-core--maybe-bootstrap-all (&rest _arguments)
  "Load account-scoped directories for every online managed account."
  (dolist (account (qq-account-list))
    (qq-core--maybe-bootstrap-account
     (alist-get 'account_id account))))

(defun qq-core--handle-ready (_instance-id)
  "Refresh all account data after a Gateway connection becomes ready."
  (clrhash qq-core--observed-account-phases)
  (dolist (runtime (qq-runtime-accounts))
    (qq-runtime-forget nil (qq-runtime-account-id runtime)))
  (dolist (account (qq-account-list))
    (puthash (alist-get 'account_id account) (alist-get 'phase account)
             qq-core--observed-account-phases))
  (qq-core--maybe-bootstrap-all))

(defun qq-core--handle-account-registry-change (reason account-id)
  "Invalidate online directory reads across ACCOUNT-ID's phase changes."
  (unless (eq reason 'ready)
    (when account-id
      (let* ((account (qq-account-get account-id))
             (phase (alist-get 'phase account))
             (previous (gethash account-id qq-core--observed-account-phases)))
        (when (and previous (not (equal previous phase))
                   (or (equal previous "online") (equal phase "online")))
          (dolist (kind '(friends groups group-members group-requests profile profile-likes))
            (qq-runtime-forget (list kind) account-id)))
        (if account (puthash account-id phase qq-core--observed-account-phases)
          (remhash account-id qq-core--observed-account-phases))))
    (qq-core--maybe-bootstrap-all)))

(defun qq-core--handle-desync (&rest _arguments)
  "Coalesce recent-conversation reads after transient event loss."
  (when (and (qq-server-ready-p) (qq-core-supports-p 'recent-conversations))
    (dolist (account (qq-account-list))
      (when (equal (alist-get 'phase account) "online")
        (qq-runtime-with-account (alist-get 'account_id account)
                                 (qq-runtime-request (qq-runtime-request-create 'recent) 'join))))))

(defun qq-core-activate ()
  "Activate account-scoped native projections for all managed accounts."
  (qq-core--maybe-bootstrap-all)
  t)

(defun qq-core-reset-session-state ()
  "Revoke account projections and request caches."
  (clrhash qq-core--observed-account-phases)
  (dolist (runtime (qq-runtime-accounts))
    (qq-runtime-forget nil (qq-runtime-account-id runtime)))
  (qq-request-revoke-all)
  (qq-attachment-reset)
  (qq-remote-media-reset)
  (qq-resource-reset)
  (qq-directory-reset)
  (qq-message-reset-correlations))


(add-hook 'qq-account-registry-changed-hook
          #'qq-request-revoke-stale)
(add-hook 'qq-account-registry-changed-hook
          #'qq-core--handle-account-registry-change t)
(add-hook 'qq-account-registry-ready-hook #'qq-core--handle-ready t)
(add-hook 'qq-account-desync-hook #'qq-core--handle-desync t)

(defun qq-core--directory-next (model)
  "Load recents once both directory reads in MODEL have settled."
  (when (and (qq-core-supports-p 'recent-conversations)
             (cl-every (lambda (key)
                         (let ((entry (appkit-data-map-get (qq-state-partition-operations model) key)))
                           (and entry (not (plist-get entry :pending)))))
                       '((friends) (groups))))
    (list (qq-runtime-request-create 'recent))))

(qq-runtime-define-operation recent ()
                             :method "conversation.list_recent"
                             :params `((limit . ,(qq-message--recent-normalize-limit nil)))
                             :prepare (qq-state--step state (qq-state-session-summary-observation-start state))
                             :commit (progn (qq-state--step state (qq-core--apply-recent-page state (qq-message--recent-check-page result) observation)) t))

(provide 'qq-core)

;;; qq-core.el ends here
