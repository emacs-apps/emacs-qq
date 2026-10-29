;;; qq-favorite-emoji.el --- Native QQ favorite emoji operations -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Account-scoped adapter for the native favorite-emoji catalog and its
;; Resource Store materialization.  Durable QQ favorite identities stay
;; opaque here; nt-core is the sole owner of their wire grammar.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-account)
(require 'qq-request)
(require 'qq-resource)
(require 'qq-rpc)
(require 'qq-runtime)

(defvar qq-favorite-emoji-changed-hook nil
  "Hook run with (OWNER REASON RECEIPT) after a valid favorite mutation.
REASON is `add', `delete', `set-description', or `move'.  A delete receipt
retains individual statuses and explicitly unreported identities; the hook
does not imply that every requested deletion succeeded.")

(defun qq-favorite-emoji-id-p (value)
  "Return non-nil when VALUE is an opaque favorite-emoji identity.

The native service owns the exact wire grammar.  This client checks only the
transport-safe properties it needs before returning an identity to Gateway."
  (and (qq-protocol-non-empty-string-p value)
       (not (string-match-p "[/[:space:][:cntrl:]]" value))))

(defun qq-favorite-emoji--entry-p (entry)
  "Return non-nil when ENTRY has the exact native catalog shape."
  (and (qq-server-wire-exact-object-keys-p
        entry (if (assq 'description entry)
                  '(favorite_emoji_id md5 url description)
                '(favorite_emoji_id md5 url)))
       (or (not (assq 'description entry))
           (stringp (alist-get 'description entry)))
       (qq-favorite-emoji-id-p (alist-get 'favorite_emoji_id entry))
       (let ((md5 (alist-get 'md5 entry)))
         (and (stringp md5)
              (string-match-p "\\`[0-9a-f]\\{32\\}\\'" md5)))
       (let ((url (alist-get 'url entry)))
         (and (qq-protocol-non-empty-string-p url)
              (string-prefix-p "https://" url)))))

(defun qq-favorite-emoji--entries-p (entries max-entries)
  "Return non-nil when ENTRIES form a unique catalog within MAX-ENTRIES."
  (and (proper-list-p entries)
       (<= (length entries) max-entries)
       (cl-every #'qq-favorite-emoji--entry-p entries)
       (let ((seen (make-hash-table :test #'equal))
             duplicate)
         (dolist (entry entries)
           (let ((id (alist-get 'favorite_emoji_id entry)))
             (if (gethash id seen)
                 (setq duplicate t)
               (puthash id t seen))))
         (not duplicate))))

(defun qq-favorite-emoji--current-owner ()
  "Return the managed account owning the current favorite operation."
  (let ((owner (qq-runtime-current-account-id)))
    (unless (and owner (qq-account-get owner))
      (user-error "qq: favorite emojis require a managed QQ account"))
    owner))

(defun qq-favorite-emoji--request
    (method params projector callback errback &optional reason)
  "Run account-scoped favorite METHOD with PARAMS.

PROJECTOR validates the response for the captured account.  CALLBACK and
ERRBACK follow the native RPC adapter convention.  Non-nil REASON notifies
`qq-favorite-emoji-changed-hook' after a valid mutation response."
  (let ((owner (qq-favorite-emoji--current-owner)))
    (qq-rpc-call
     method (append `((account_id . ,owner)) params)
     :current-p (lambda () (and (qq-account-get owner) t))
     :stale-code "account_removed"
     :stale-message "QQ account was removed during favorite-emoji operation"
     :projector
     (lambda (result)
       (qq-runtime-with-account owner
                                (funcall projector result owner)))
     :callback
     (lambda (value)
       (qq-runtime-with-account owner
         (when reason
           (qq-runtime-forget '(favorite-emojis) owner)
           (qq-runtime-request '(favorite-emojis) t)
           (qq-rpc-run-hook
            'qq-favorite-emoji-changed-hook owner reason value))
         (qq-rpc-invoke callback value)))
     :errback
     (lambda (body reason)
       (if (qq-account-get owner)
           (qq-runtime-with-account owner
             (qq-rpc-invoke errback body reason))
         ;; A late failure still identifies its owner without recreating the
         ;; removed account's canonical runtime and state partition.
         (let ((qq-runtime--context-account-id owner))
           (qq-rpc-invoke errback body reason)))))))

(defun qq-favorite-emoji--project-catalog (result owner)
  "Return exact favorite catalog RESULT belonging to OWNER."
  (let* ((account (qq-account-get owner))
         (max-entries (alist-get 'max_entries result))
         (entries (alist-get 'entries result)))
    (unless
        (and
         (qq-server-wire-exact-object-keys-p
          result
          '(account_id owner max_entries entries source refreshed_at))
         (equal (alist-get 'account_id result) owner)
         (equal (alist-get 'owner result) (alist-get 'uin account))
         (qq-protocol-uint32-p max-entries)
         (qq-favorite-emoji--entries-p entries max-entries)
         (member (alist-get 'source result) '("network" "cache"))
         (integerp (alist-get 'refreshed_at result)))
      (error "qq: Gateway returned an invalid favorite-emoji catalog"))
    (copy-tree result)))

(defun qq-favorite-emoji--project-materialized
    (result owner expected-id)
  "Project materialized RESULT for OWNER and EXPECTED-ID into Resource Store."
  (let* ((entry (alist-get 'entry result))
         (resource (alist-get 'resource result))
         (digests (and (listp resource) (alist-get 'digests resource))))
    (unless
        (and
         (qq-server-wire-exact-object-keys-p result '(account_id entry resource))
         (equal (alist-get 'account_id result) owner)
         (qq-favorite-emoji--entry-p entry)
         (equal (alist-get 'favorite_emoji_id entry) expected-id)
         (listp resource)
         (qq-resource-id-p (alist-get 'resource_id resource))
         (equal (alist-get 'phase resource) "ready")
         (listp digests)
         (equal (alist-get 'md5 digests) (alist-get 'md5 entry))
         (let ((media-type (alist-get 'media_type resource)))
           (and (stringp media-type)
                (string-prefix-p "image/" media-type))))
      (error "qq: Gateway returned a contradictory favorite-emoji resource"))
    (let ((projected (qq-resource--upsert
                      resource 'favorite-emoji-materialized)))
      `((account_id . ,owner)
        (entry . ,(copy-tree entry))
        (resource . ,projected)))))

(defun qq-favorite-emoji-materialize
    (favorite-emoji-id callback &optional errback)
  "Materialize FAVORITE-EMOJI-ID as one verified ready Resource.

CALLBACK receives the validated materialization snapshot.  The returned
resource is also merged into the existing Resource Store projection."
  (unless (qq-favorite-emoji-id-p favorite-emoji-id)
    (user-error "qq: favorite emoji identity is malformed"))
  (qq-favorite-emoji--request
   "favorite_emoji.materialize"
   `((favorite_emoji_id . ,favorite-emoji-id))
   (lambda (result owner)
     (qq-favorite-emoji--project-materialized
      result owner favorite-emoji-id))
   callback errback))

(defun qq-favorite-emoji-add (resource-id callback &optional errback)
  "Add ready image RESOURCE-ID to the selected account's favorites.

CALLBACK receives the native account and entry receipt.  RESOURCE-ID is
borrowed: this operation never releases it."
  (unless (qq-resource-id-p resource-id)
    (user-error "qq: favorite source must be an opaque resource identity"))
  (let ((md5 (alist-get 'md5 (alist-get 'digests (qq-resource resource-id)))))
    (qq-favorite-emoji--request
     "favorite_emoji.add" `((resource_id . ,resource-id))
     (lambda (result owner)
       (unless (and (qq-server-wire-exact-object-keys-p
                     result '(account_id entry))
                    (equal (alist-get 'account_id result) owner)
                    (qq-favorite-emoji--entry-p (alist-get 'entry result))
                    (or (null md5)
                        (equal md5 (alist-get 'md5 (alist-get 'entry result)))))
         (error "qq: Gateway returned an invalid favorite-emoji add receipt"))
       result)
     callback errback 'add)))

(defun qq-favorite-emoji--unique-ids-p (ids)
  "Return non-nil when IDS is a proper list of unique opaque identities."
  (and (proper-list-p ids)
       (cl-every #'qq-favorite-emoji-id-p ids)
       (let ((seen (make-hash-table :test #'equal)))
         (cl-every (lambda (id)
                     (unless (gethash id seen)
                       (puthash id t seen)))
                   ids))))

(defun qq-favorite-emoji--project-deleted (result owner expected-ids)
  "Validate RESULT for OWNER as a complete partition of EXPECTED-IDS.
Unreported IDs remain unreported, never synthesized as successful entries."
  (let ((entries (alist-get 'entries result))
        (unreported (alist-get 'unreported_favorite_emoji_ids result)))
    (unless
        (and
         (qq-server-wire-exact-object-keys-p
          result '(account_id entries unreported_favorite_emoji_ids))
         (equal (alist-get 'account_id result) owner)
         (proper-list-p entries)
         (cl-every
          (lambda (entry)
            (and (qq-server-wire-exact-object-keys-p
                  entry '(favorite_emoji_id status))
                 (qq-favorite-emoji-id-p (alist-get 'favorite_emoji_id entry))
                 (qq-protocol-uint32-p (alist-get 'status entry))))
          entries)
         (proper-list-p unreported)
         (let ((ids (append (mapcar (lambda (entry)
                                     (alist-get 'favorite_emoji_id entry))
                                   entries)
                            unreported)))
           (and (qq-favorite-emoji--unique-ids-p ids)
                (= (length ids) (length expected-ids))
                (cl-every (lambda (id) (member id expected-ids)) ids))))
      (error "qq: Gateway returned an invalid favorite-emoji delete receipt"))
    result))

(defun qq-favorite-emoji-delete (favorite-emoji-ids callback &optional errback)
  "Delete FAVORITE-EMOJI-IDS for the selected account.

CALLBACK receives the exact native receipt: only entries with status zero
confirm deletion.  Nonzero statuses and `unreported_favorite_emoji_ids' are
preserved, not promoted to success."
  (unless (and favorite-emoji-ids
               (qq-favorite-emoji--unique-ids-p favorite-emoji-ids))
    (user-error "qq: favorite deletion requires unique opaque identities"))
  (let ((ids (copy-tree favorite-emoji-ids)))
    (qq-favorite-emoji--request
     "favorite_emoji.delete" `((favorite_emoji_ids . ,(vconcat ids)))
     (lambda (result owner)
       (qq-favorite-emoji--project-deleted result owner ids))
     callback errback 'delete)))

(defun qq-favorite-emoji-set-description
    (favorite-emoji-id description callback &optional errback)
  "Set FAVORITE-EMOJI-ID's cloud DESCRIPTION for the selected account.

An empty string clears the annotation.  CALLBACK receives the native
receipt confirming the requested identity and description."
  (unless (qq-favorite-emoji-id-p favorite-emoji-id)
    (user-error "qq: favorite emoji identity is malformed"))
  (unless (stringp description)
    (user-error "qq: favorite emoji description must be a string"))
  (qq-favorite-emoji--request
   "favorite_emoji.set_description"
   `((favorite_emoji_id . ,favorite-emoji-id) (description . ,description))
   (lambda (result owner)
     (unless (and (qq-server-wire-exact-object-keys-p
                   result '(account_id favorite_emoji_id description))
                  (equal (alist-get 'account_id result) owner)
                  (equal (alist-get 'favorite_emoji_id result) favorite-emoji-id)
                  (equal (alist-get 'description result) description))
       (error "qq: Gateway returned an invalid favorite-emoji description receipt"))
     result)
   callback errback 'set-description))

(defun qq-favorite-emoji-move
    (favorite-emoji-id position callback &optional errback)
  "Move FAVORITE-EMOJI-ID to one-based POSITION for the selected account.

CALLBACK receives the authoritative reordered catalog."
  (unless (qq-favorite-emoji-id-p favorite-emoji-id)
    (user-error "qq: favorite emoji identity is malformed"))
  (unless (and (qq-protocol-uint32-p position) (> position 0))
    (user-error "qq: favorite emoji position must be a positive uint32"))
  (qq-favorite-emoji--request
   "favorite_emoji.move"
   `((favorite_emoji_id . ,favorite-emoji-id) (position . ,position))
   (lambda (result owner)
     (let* ((catalog (qq-favorite-emoji--project-catalog result owner))
            (entries (alist-get 'entries catalog)))
       (unless (and (<= position (length entries))
                    (equal (alist-get 'favorite_emoji_id
                                      (nth (1- position) entries))
                           favorite-emoji-id))
         (error "qq: Gateway favorite-emoji order contradicts the requested move"))
       catalog))
   callback errback 'move))

(defun qq-favorite-emoji--release-resource (resource-id)
  "Release the favorite operation's owned RESOURCE-ID."
  (condition-case error-data
      (qq-resource-release
       resource-id nil
       (lambda (_body reason)
         (message "qq: favorite image cleanup failed: %s" reason)))
    ((error quit)
     (message "qq: favorite image cleanup failed: %s"
              (error-message-string error-data)))))

(defun qq-favorite-emoji-add-file (path callback &optional errback)
  "Stage local image PATH and add it to the captured account's favorites.

Return a `qq-request' handle.  Cancellation or account removal detaches the
caller; an already-dispatched add settles before releasing its source.
Every staged resource owned here is released on success or failure, including
a stage response arriving after cancellation.  CALLBACK receives the native
add receipt; ERRBACK follows the Gateway convention."
  (let ((owner (qq-favorite-emoji--current-owner))
        resource-id ready-watch publishing request account-observer)
    (cl-labels
        ((detach
           ()
           (when ready-watch
             (qq-request-watch-cancel ready-watch)
             (setq ready-watch nil))
           (remove-hook 'qq-account-registry-changed-hook account-observer))
         (release
           ()
           (when resource-id
             (let ((owned resource-id))
               (setq resource-id nil)
               (qq-favorite-emoji--release-resource owned))))
         (finish
           (success body value)
           (setq publishing nil)
           (detach)
           (release)
           (when (qq-request-active-p request)
             (if success
                 (qq-request-finish request)
               (qq-request-fail request))
             (qq-runtime-with-account owner
               (if success
                   (qq-rpc-invoke callback value)
                 (qq-rpc-invoke errback body value)))))
         (failed
           (body reason)
           (finish nil body reason))
         (ready
           (_resource)
           (when ready-watch
             (qq-request-watch-cancel ready-watch)
             (setq ready-watch nil))
           (unless (qq-account-get owner)
             (qq-request-cancel request))
           (if (not (qq-request-active-p request))
               (release)
             (condition-case error-data
                 (qq-runtime-with-account owner
                   (setq publishing t)
                   (qq-favorite-emoji-add
                    resource-id
                    (lambda (receipt) (finish t nil receipt))
                    #'failed))
               ((error quit)
                (failed nil (error-message-string error-data))))))
         (staged
           (resource)
           (setq resource-id (alist-get 'resource_id resource))
           (unless (qq-account-get owner)
             (qq-request-cancel request))
           (if (not (qq-request-active-p request))
               (release)
             (condition-case error-data
                 (progn
                   (setq ready-watch
                         (qq-resource-await-ready resource-id #'ready #'failed))
                   ;; Await-ready can settle synchronously before assignment.
                   (when (or publishing (not (qq-request-active-p request)))
                     (when ready-watch
                       (qq-request-watch-cancel ready-watch)
                       (setq ready-watch nil))))
               ((error quit)
                (failed nil (error-message-string error-data))))))
         (cancel
           ()
           (detach)
           (unless publishing (release))))
      (setq request (qq-request-create owner #'cancel)
            account-observer
            (lambda (&rest _ignored)
              (unless (qq-account-get owner)
                (qq-request-cancel request))))
      (add-hook 'qq-account-registry-changed-hook account-observer)
      (condition-case error-data
          (qq-resource-stage-local path nil nil #'staged #'failed)
        ((error quit)
         (qq-request-fail request)
         (detach)
         (release)
         (signal (car error-data) (cdr error-data))))
      request)))

(qq-runtime-define-operation favorite-emojis ()
  :method "favorite_emoji.list" :params (when refresh '((refresh . t)))
  :commit (qq-favorite-emoji--project-catalog result owner))

(provide 'qq-favorite-emoji)

;;; qq-favorite-emoji.el ends here
