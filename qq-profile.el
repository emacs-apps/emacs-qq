;;; qq-profile.el --- QQ profile-card operations -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Account-scoped profile-card reads and interactions.  Contact directory
;; caches deliberately do not own these operations.

;;; Code:

(require 'qq-account)
(require 'qq-rpc)
(require 'qq-runtime)

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

(qq-runtime-define-operation profile (user-id)
  :method "profile.get"
  :params `((user_uin . ,user-id))
  :commit (qq-profile--project result owner user-id))

(qq-runtime-define-operation profile-likes (user-id)
  :method "profile.get_like_summary"
  :params `((user_uin . ,user-id))
  :commit (qq-profile--project-like-summary result owner user-id))

(qq-runtime-define-operation profile-like (user-id)
  :method "profile.send_like" :write t
  :params `((user_uin . ,user-id))
  :commit (qq-profile--project-like-outcome result owner user-id)
  :cancel (list (list 'profile-likes user-id))
  :next (when (equal (alist-get 'kind value) "liked")
          (list (qq-runtime-request-create 'profile-likes :user-id user-id))))

(provide 'qq-profile)

;;; qq-profile.el ends here
