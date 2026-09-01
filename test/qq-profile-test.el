;;; qq-profile-test.el --- Tests for QQ profile operations -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-profile)

(defconst qq-profile-test-capabilities
  '("profile.get" "profile.get_like_summary" "profile.send_like")
  "Native profile capabilities exercised by these tests.")

(defmacro qq-profile-test-with-account (&rest body)
  "Run BODY with one isolated selected managed account."
  (declare (indent 0) (debug t))
  `(let ((qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--gateway-instance-id nil)
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
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
           (qq-state-select-account "slot-a")
           ,@body)
       (qq-runtime-stop)
       (qq-state-reset))))

(ert-deftest qq-profile-get-projects-an-owned-profile ()
  (qq-profile-test-with-account
    (let (sent-method sent-params profile)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-profile-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (profile
                       . ((user_uin . "9007199254740999")
                          (uid . "u_alice")
                          (nickname . "Alice")
                          (relationship . ((kind . "friend")))))))
                   "request-profile")))
        (should
         (equal
          (qq-profile-get
           "9007199254740999"
           (lambda (value) (setq profile value)))
          "request-profile"))
        (should (equal sent-method "profile.get"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (user_uin . "9007199254740999"))))
        (should (equal (alist-get 'nickname profile) "Alice"))))))

(ert-deftest qq-profile-like-summary-and-mutation-are-not-directory-work ()
  (qq-profile-test-with-account
    (let (calls summary outcome)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-profile-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (cons method params) calls)
                   (funcall
                    callback
                    (pcase method
                      ("profile.get_like_summary"
                       '((account_id . "slot-a")
                         (summary
                          . ((user_uin . "10002")
                             (total_count . 42)))))
                      ("profile.send_like"
                       '((account_id . "slot-a")
                         (user_uin . "10002")
                         (outcome
                          . ((kind . "liked")
                             (added_count . 1)))))))
                   method)))
        (qq-profile-get-like-summary
         "10002" (lambda (value) (setq summary value)))
        (qq-profile-send-like
         "10002" (lambda (value) (setq outcome value)))
        (should (equal (alist-get 'total_count summary) 42))
        (should (equal outcome '((kind . "liked") (added_count . 1))))
        (should
         (equal
          (mapcar #'car (nreverse calls))
          '("profile.get_like_summary" "profile.send_like")))))))

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
