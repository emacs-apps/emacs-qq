;;; qq-favorite-emoji.el --- Native QQ favorite emoji operations -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Account-scoped adapter for the native favorite-emoji catalog and its
;; Resource Store materialization.  Durable QQ favorite identities stay
;; opaque here; nt-core is the sole owner of their wire grammar.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-account)
(require 'qq-resource)
(require 'qq-rpc)
(require 'qq-runtime)

(defun qq-favorite-emoji-id-p (value)
  "Return non-nil when VALUE is an opaque favorite-emoji identity.

The native service owns the exact wire grammar.  This client checks only the
transport-safe properties it needs before returning an identity to Gateway."
  (and (qq-protocol-non-empty-string-p value)
       (not (string-match-p "[/[:space:][:cntrl:]]" value))))

(defun qq-favorite-emoji--entry-p (entry)
  "Return non-nil when ENTRY has the exact native catalog shape."
  (and (qq-server-wire-exact-object-keys-p
        entry '(favorite_emoji_id md5 url))
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
    (method params projector callback errback)
  "Run account-scoped favorite METHOD with PARAMS.

PROJECTOR validates the response for the captured account.  CALLBACK and
ERRBACK follow the native RPC adapter convention."
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
         (qq-rpc-invoke callback value)))
     :errback
     (lambda (body reason)
       (qq-runtime-with-account owner
         (qq-rpc-invoke errback body reason))))))

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

(defun qq-favorite-emoji-list
    (&optional refresh callback errback)
  "Fetch the selected account's native favorite-emoji catalog.

With REFRESH non-nil, bypass Gateway's replaceable catalog cache.  CALLBACK
receives the validated catalog; ERRBACK follows the Gateway convention."
  (qq-favorite-emoji--request
   "favorite_emoji.list"
   (when refresh '((refresh . t)))
   #'qq-favorite-emoji--project-catalog
   callback errback))

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

(provide 'qq-favorite-emoji)

;;; qq-favorite-emoji.el ends here
