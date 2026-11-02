;;; qq-profile-test.el --- Tests for QQ profile operations -*- lexical-binding: t; -*-

(require 'qq-test-state)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-profile)
(require 'qq-operation-test)

(defconst qq-profile-test-capabilities
  '("profile.get" "profile.get_like_summary" "profile.send_like")
  "Native profile capabilities exercised by these tests.")

(defmacro qq-profile-test-with-account (&rest body)
  "Run BODY with one isolated selected managed account."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--account-id
          (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
         (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create)))
         (qq-account--accounts (make-hash-table :test #'equal)) (qq-account--account-order nil)
         (qq-account--current-account-id nil) (qq-account--gateway-instance-id nil)
         (qq-account-registry-changed-hook nil) (qq-account-selection-changed-hook nil)
         (qq-runtime--app nil) (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-server--state 'ready))
     (unwind-protect
         (progn
           (qq-account--replace-accounts
            '(((account_id . "slot-a")
               (label . "Primary")
               (phase . "online")
               (uin . "10001")
               (uid . "u_self")
               (challenge)
               (problem)))
            'ready "gateway-test")
           (setq qq-runtime--account-id "slot-a")
           ,@body)
       (qq-runtime-stop)
       (qq-test-reset))))

(ert-deftest qq-profile-get-projects-an-owned-profile ()
  (qq-operation-test-with-app
    (qq-runtime-request (qq-runtime-request-create 'profile :user-id "9007199254740999"))
    (qq-operation-test--drain)
    (should (equal (plist-get (car calls) :method) "profile.get"))
    (should (equal (plist-get (car calls) :params) '((account_id . "slot-a") (user_uin . "9007199254740999"))))
    (qq-operation-test--resolve (car calls)
     '((account_id . "slot-a") (profile . ((user_uin . "9007199254740999") (nickname . "Alice")))))
    (should-not (plist-get (qq-runtime-operation '(profile "9007199254740999")) :value))
    (qq-operation-test--drain)
    (should (equal (alist-get 'nickname (plist-get (qq-runtime-operation '(profile "9007199254740999")) :value)) "Alice"))))

(ert-deftest qq-profile-like-summary-and-mutation-are-not-directory-work ()
  (qq-operation-test-with-app
    (qq-runtime-request (qq-runtime-request-create 'profile-likes :user-id "10002"))
    (qq-operation-test--drain)
    (let ((old (car calls)))
      (qq-runtime-request (qq-runtime-request-create 'profile-like :user-id "10002"))
      (qq-operation-test--drain)
      (qq-operation-test--resolve (car calls)
       '((account_id . "slot-a") (user_uin . "10002") (outcome . ((kind . "liked") (added_count . 1)))))
      (qq-operation-test--drain)
      (should (equal (mapcar (lambda (call) (plist-get call :method)) calls)
                     '("profile.get_like_summary" "profile.send_like" "profile.get_like_summary")))
      (qq-operation-test--resolve (car calls)
       '((account_id . "slot-a") (summary . ((user_uin . "10002") (total_count . 42)))))
      (qq-operation-test--resolve old
       '((account_id . "slot-a") (summary . ((user_uin . "10002") (total_count . 10)))))
      (qq-operation-test--drain)
      (should (= (alist-get 'total_count (plist-get (qq-runtime-operation '(profile-likes "10002")) :value)) 42)))))

(ert-deftest qq-profile-rejects-cross-account-and-cross-user-results ()
  (qq-profile-test-with-account
    (dolist
        (result
         '(((account_id . "slot-b")
            (profile . ((user_uin . "10002"))))
           ((account_id . "slot-a")
            (profile . ((user_uin . "10003"))))))
      (should-error
       (qq-profile--project result "slot-a" "10002")))))

(provide 'qq-profile-test)

;;; qq-profile-test.el ends here
