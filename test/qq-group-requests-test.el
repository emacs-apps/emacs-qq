;;; qq-group-requests-test.el --- Tests for native group requests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
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

(ert-deftest qq-group-requests-frame-only-skips-render ()
  (let ((invalidations (appkit-invalidations-create)))
    (setf (appkit-invalidations-parts invalidations) '(frame))
    (cl-letf (((symbol-function 'qq-group-requests-render)
               (lambda ()
                 (ert-fail "frame-only sync rendered group requests"))))
      (qq-group-requests--sync-invalidations
       'unused invalidations nil))))

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
  (let ((request (qq-group-requests-test-request t))
        sent-method sent-params delivered)
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-account-get)
               (lambda (account-id)
                 (and (equal account-id "slot-a")
                      '((account_id . "slot-a") (phase . "online")))))
              ((symbol-function 'qq-rpc-call)
               (lambda (method params &rest options)
                 (setq sent-method method
                       sent-params params)
                 (let* ((projector (plist-get options :projector))
                        (callback (plist-get options :callback))
                        (receipt
                         '((account_id . "slot-a")
                           (mailbox . "main")
                           (sequence . "9007199254740993")
                           (event . ((kind . "user_join")))
                           (group_uin . "20001")
                           (decision . "accept"))))
                   (funcall callback (funcall projector receipt)))
                 'request-token)))
      (should
       (eq
        (qq-directory-decide-group-request
         'main request 'accept ""
         (lambda (receipt) (setq delivered receipt)))
        'request-token))
      (should (equal sent-method "group_request.decide"))
      (should (equal (alist-get 'sequence sent-params) "9007199254740993"))
      (should (equal (alist-get 'event sent-params)
                     '((kind . "user_join"))))
      (should (equal delivered
                     '((account_id . "slot-a")
                       (mailbox . "main")
                       (sequence . "9007199254740993")
                       (event . ((kind . "user_join")))
                       (group_uin . "20001")
                       (decision . "accept")))))))

(ert-deftest qq-group-requests-render-exposes-only-actionable-row-actions ()
  (with-temp-buffer
    (qq-group-requests-mode)
    (setq qq-group-requests--pages
          `((main . ,(qq-group-requests-test-page
                      'main (qq-group-requests-test-request t)))
            (filtered . ,(qq-group-requests-test-page
                          'filtered (qq-group-requests-test-request nil)))))
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
               "9007199254740993")))))

(provide 'qq-group-requests-test)

;;; qq-group-requests-test.el ends here
