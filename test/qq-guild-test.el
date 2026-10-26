;;; qq-guild-test.el --- Guild boundary and Surface tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'qq-operation-test)
(require 'qq-guild)

(defconst qq-guild-test-route '((guild_id . "9007199254740993") (channel_id . "9007199254740995")))
(defun qq-guild-test-message (sequence &optional channel)
  `((channel . ,(or channel qq-guild-test-route)) (sequence . ,sequence)
    (count_sequence . "72") (timestamp . 123456)
    (sender_tiny_id . "9007199254740997") (sender_name . "频道成员")
    (text . "你好") (unsupported_elements . 0)))

(defmacro qq-guild-test-with-view (&rest body)
  (declare (indent 0))
  `(qq-operation-test-with-app
     (cl-letf (((symbol-function 'qq-rpc-method-available-p) (lambda (_) t))
               ((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer)))
       (let ((buffer (qq-guild-open-channel qq-guild-test-route "频道测试" "slot-a")))
         (with-current-buffer buffer ,@body)))))

(ert-deftest qq-guild-exact-identities-and-route-checks ()
  (should (qq-guild--message-p (qq-guild-test-message "9007199254740999")))
  (should-not (qq-guild--channel-p '((guild_id . 1) (channel_id . "2"))))
  (with-temp-buffer
    (qq-guild-mode)
    (setq qq-guild--channel qq-guild-test-route)
    (should-error (qq-guild--merge
                   (list (qq-guild-test-message "100" '((guild_id . "3") (channel_id . "4"))))))))

(ert-deftest qq-guild-sparse-history-uses-returned-boundaries ()
  (qq-guild-test-with-view
    (qq-operation-test--resolve
     (car calls)
     `((account_id . "slot-a")
       (history . ((channel . ,qq-guild-test-route) (begin_sequence . "90")
                   (end_sequence . "100")
                   (messages . (,(qq-guild-test-message "99"))) (unsupported_messages . 0)))))
    (should (equal qq-guild--begin "90"))
    (qq-guild-history-older)
    (should (equal (alist-get 'end_sequence (plist-get (car calls) :params)) "89"))))

(ert-deftest qq-guild-push-is-account-scoped-and-deduplicated ()
  (qq-guild-test-with-view
    (let ((msg (qq-guild-test-message "100")))
      (qq-guild--event "guild.observed"
                       `((account_id . "slot-b") (messages . (,msg))))
      (should-not qq-guild--messages)
      (dotimes (_ 2)
        (qq-guild--event "guild.observed"
                         `((account_id . "slot-a") (messages . (,msg)))))
      (should (= (hash-table-count qq-guild--messages) 1))
      (should (string-match-p "频道成员" (buffer-string))))))

(ert-deftest qq-guild-read-waits-for-confirmation-and-never-uses-group-route ()
  (qq-guild-test-with-view
    (qq-guild--merge (list (qq-guild-test-message "100")))
    (qq-guild--render)
    (goto-char (point-min)) (search-forward "你好")
    (qq-guild-mark-read)
    (let ((call (car calls)))
      (should (equal (plist-get call :method) "guild.mark_read"))
      (should (equal (alist-get 'channel (plist-get call :params)) qq-guild-test-route))
      (should-not (assq 'group_uin (plist-get call :params)))
      (should-not qq-guild--read-sequence)
      (qq-operation-test--resolve call
       `((account_id . "slot-a") (receipt . ((channel . ,qq-guild-test-route) (sequence . "100")))))
      (should (equal qq-guild--read-sequence "100")))))

(ert-deftest qq-guild-retired-view-cannot-accept-late-history ()
  (qq-guild-test-with-view
    (let ((call (car calls)))
      (qq-guild--cancel)
      (qq-operation-test--resolve call
       `((account_id . "slot-a") (history . ((channel . ,qq-guild-test-route)
        (begin_sequence . "90") (end_sequence . "100")
        (messages . (,(qq-guild-test-message "99"))) (unsupported_messages . 0)))))
      (should-not qq-guild--messages))))

(ert-deftest qq-guild-send-failure-retains-draft-and-success-keeps-new-edits ()
  (qq-guild-test-with-view
    (let* ((source (current-buffer))
           (surface (qq-runtime-open-account-surface
                     :account-id "slot-a" :id '(guild-test-draft)
                     :mode 'qq-guild-compose-mode :render-function #'ignore))
           (draft (appkit-surface-buffer surface)))
      (with-current-buffer draft
        (setq qq-guild--channel qq-guild-test-route qq-guild--source source)
        (insert "草稿")
        (qq-guild-send)
        (let ((call (car calls)))
          (should-error (qq-guild-send) :type 'user-error)
          (funcall (plist-get (plist-get call :options) :errback) nil "拒绝发送")
          (should (equal (buffer-string) "草稿")))
        (qq-guild-send)
        (let ((call (car calls)))
          (insert "的新内容")
          (qq-operation-test--resolve call
           `((account_id . "slot-a") (receipt . ((channel . ,qq-guild-test-route) (sequence . "101")))))
          (should (equal (buffer-string) "草稿的新内容")))))))

(provide 'qq-guild-test)
