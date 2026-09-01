;;; qq-profile.el --- QQ profile-card operations -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Account-scoped profile-card reads and interactions.  Contact directory
;; caches deliberately do not own these operations.

;;; Code:

(require 'qq-account)
(require 'qq-rpc)
(require 'qq-runtime)

(defun qq-profile--current-owner ()
  "Return the managed account owning the current profile operation."
  (let ((owner (qq-runtime-current-account-id)))
    (unless (and owner (qq-account-get owner))
      (user-error "qq: this operation requires a managed QQ account"))
    owner))

(defun qq-profile--request
    (method params projector callback errback)
  "Run account-scoped profile METHOD with PARAMS.

PROJECTOR validates the result for the exact account owner.  CALLBACK and
ERRBACK follow the native RPC adapter convention."
  (let ((owner (qq-profile--current-owner)))
    (qq-rpc-call
     method (append `((account_id . ,owner)) params)
     :current-p (lambda () (and (qq-account-get owner) t))
     :stale-code "account_removed"
     :stale-message "QQ account was removed during profile request"
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

(defun qq-profile--project (result owner user-uin)
  "Return USER-UIN's profile from owned Gateway RESULT."
  (let ((profile (alist-get 'profile result)))
    (unless
        (and
         (qq-server-wire-exact-object-keys-p result '(account_id profile))
         (equal (alist-get 'account_id result) owner)
         (listp profile)
         (equal (alist-get 'user_uin profile) user-uin))
      (error "qq: Gateway returned a profile for another user"))
    (copy-tree profile)))

(defun qq-profile-get (user-uin callback &optional errback)
  "Fetch USER-UIN's sparse native profile and call CALLBACK."
  (unless (qq-protocol-uint64-decimal-p user-uin)
    (user-error "qq: User profile requires an exact decimal UIN"))
  (qq-profile--request
   "profile.get"
   `((user_uin . ,user-uin))
   (lambda (result owner)
     (qq-profile--project result owner user-uin))
   callback errback))

(defun qq-profile--project-like-summary (result owner user-uin)
  "Return USER-UIN's verified profile-like summary from owned RESULT."
  (let ((summary (alist-get 'summary result)))
    (unless
        (and
         (qq-server-wire-exact-object-keys-p result '(account_id summary))
         (equal (alist-get 'account_id result) owner)
         (qq-server-wire-exact-object-keys-p summary '(user_uin total_count))
         (equal (alist-get 'user_uin summary) user-uin)
         (natnump (alist-get 'total_count summary)))
      (error "qq: Gateway returned an invalid profile-like summary"))
    (copy-tree summary)))

(defun qq-profile-get-like-summary
    (user-uin callback &optional errback)
  "Fetch USER-UIN's native profile-like summary and call CALLBACK."
  (unless (qq-protocol-uint64-decimal-p user-uin)
    (user-error "qq: Profile likes require an exact decimal UIN"))
  (qq-profile--request
   "profile.get_like_summary"
   `((user_uin . ,user-uin))
   (lambda (result owner)
     (qq-profile--project-like-summary result owner user-uin))
   callback errback))

(defun qq-profile--project-like-outcome (result owner user-uin)
  "Return USER-UIN's verified profile-like mutation from owned RESULT."
  (let ((outcome (alist-get 'outcome result)))
    (unless
        (and
         (qq-server-wire-exact-object-keys-p
          result '(account_id user_uin outcome))
         (equal (alist-get 'account_id result) owner)
         (equal (alist-get 'user_uin result) user-uin)
         (listp outcome))
      (error "qq: Gateway returned an invalid profile-like outcome"))
    (pcase (alist-get 'kind outcome)
      ("liked"
       (unless
           (and
            (qq-server-wire-exact-object-keys-p outcome '(kind added_count))
            (equal (alist-get 'added_count outcome) 1))
         (error "qq: Gateway returned an invalid liked outcome")))
      ("daily_limit"
       (unless (qq-server-wire-exact-object-keys-p outcome '(kind))
         (error "qq: Gateway returned an invalid daily-limit outcome")))
      (_
       (error "qq: Gateway returned an unknown profile-like outcome")))
    (copy-tree outcome)))

(defun qq-profile-send-like
    (user-uin callback &optional errback)
  "Give USER-UIN one native profile-card like and call CALLBACK."
  (unless (qq-protocol-uint64-decimal-p user-uin)
    (user-error "qq: Profile likes require an exact decimal UIN"))
  (qq-profile--request
   "profile.send_like"
   `((user_uin . ,user-uin))
   (lambda (result owner)
     (qq-profile--project-like-outcome result owner user-uin))
   callback errback))

(provide 'qq-profile)

;;; qq-profile.el ends here
