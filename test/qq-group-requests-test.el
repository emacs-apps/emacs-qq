;;; qq-group-requests-test.el --- Tests for native group requests -*- lexical-binding: t; -*-

(require 'qq-test-state)

;;; Code:

(require 'ert)
(require 'qq-operation-test)
(require 'cl-lib)
(require 'qq-group-requests)
(require 'qq-directory)

(defun qq-group-requests-test-request (&optional actionable)
  "Return one closed native group request with ACTIONABLE state."
  `((sequence . "9007199254740993")
    (event . ((kind . "user_join")))
    (state . ((kind . "pending")))
    (group_uin . "20001")
    (group_name . "Protocol Lab")
    (target . ((uid . "u_requester") (name . "Applicant")))
    (inviter)
    (operator)
    (comment . "Please approve")
    (actionable . ,(if actionable t :false))))

(defun qq-group-requests-test-page (mailbox request)
  "Return one closed MAILBOX page containing REQUEST."
  `((account_id . "slot-a")
    (mailbox . ,(symbol-name mailbox))
    (new_latest_sequence . "9007199254740994")
    (requests . (,request))))

(ert-deftest qq-directory-group-request-page-keeps-decimal-selector-closed ()
  (let* ((request (qq-group-requests-test-request t))
         (page (qq-group-requests-test-page 'main request))
         (projected
          (qq-directory--project-group-requests page "slot-a" 'main)))
    (should (equal projected page))
    (let ((bad-page (copy-tree page)))
      (setf (alist-get 'sequence (car (alist-get 'requests bad-page)))
            9007199254740993)
      (should-error
       (qq-directory--project-group-requests bad-page "slot-a" 'main)))))

(ert-deftest qq-directory-group-request-decision-replays-exact-native-key ()
  (qq-operation-test-with-app
   (let ((request (qq-group-requests-test-request t)))
     (qq-runtime-request (qq-runtime-request-create 'group-decision :mailbox 'main :request request :decision 'accept :refusal-message ""))
     (qq-operation-test--drain)
     (should (equal (plist-get (car calls) :method) "group_request.decide"))
     (should (equal (alist-get 'sequence (plist-get (car calls) :params)) "9007199254740993"))
     (qq-operation-test--resolve (car calls)
                                 '((account_id . "slot-a") (mailbox . "main") (sequence . "9007199254740993")
                                   (event . ((kind . "user_join"))) (group_uin . "20001") (decision . "accept")))
     (qq-operation-test--drain)
     (should (equal (mapcar (lambda (call) (plist-get call :method)) calls)
                    '("group_request.list" "group_request.list" "group_request.decide"))))))

(ert-deftest qq-group-requests-render-exposes-only-actionable-row-actions ()
  (qq-operation-test-with-app
   (with-temp-buffer
     (qq-group-requests-mode)
     (dolist (mailbox '(main filtered))
       (qq-operation-test--seed
        (list 'group-requests mailbox)
        :value (qq-group-requests-test-page mailbox (qq-group-requests-test-request (eq mailbox 'main)))))
     (qq-group-requests-render)
     (let ((text (buffer-string)))
       (should (string-match-p "Protocol Lab" text))
       (should (string-match-p "Applicant 申请加入" text))
       (should (= (how-many "同意" (point-min) (point-max)) 1))
       (should (= (how-many "拒绝" (point-min) (point-max)) 1)))
     (goto-char (point-min))
     (search-forward "同意")
     (let ((button (button-at (1- (point)))))
       (should button)
       (should (eq (button-get button 'qq-group-request-mailbox) 'main))
       (should (equal
                (alist-get 'sequence (button-get button 'qq-group-request))
                "9007199254740993"))))))

(provide 'qq-group-requests-test)

;;; qq-group-requests-test.el ends here
