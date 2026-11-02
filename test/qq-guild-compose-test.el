;;; qq-guild-compose-test.el --- Forum write ownership tests -*- lexical-binding: t; -*-

(require 'qq-test-state)
(require 'qq-guild-test)
(require 'qq-guild-compose)

(defmacro qq-guild-compose-test-with-app (&rest body)
  (declare (indent 0))
  (list 'qq-guild-test-with-app
        (list 'unwind-protect (cons 'progn body)
              '(dolist (buffer (buffer-list))
                 (with-current-buffer buffer
                   (when (derived-mode-p 'qq-guild-compose-mode)
                     (set-buffer-modified-p nil)
                     (kill-buffer buffer)))))))

(defun qq-guild-compose-test--forum (call-function)
  (let ((buffer (qq-guild-forum-open qq-guild-test--channel "Forum" "slot-a")))
    (qq-operation-test--drain)
    (qq-operation-test--resolve
     (funcall call-function) (qq-guild-test--forum-page "" nil
                                                        (qq-guild-test--post "p1") (qq-guild-test--post "p2")))
    (qq-operation-test--drain)
    buffer))

(defun qq-guild-compose-test--draft (source)
  (with-current-buffer source (qq-guild-forum-new-post)))

(defun qq-guild-compose-test--receipt (target status &optional account)
  (list (cons 'account_id (or account "slot-a"))
        (cons 'receipt
              (list (cons 'target (copy-tree target))
                    (cons 'outcome
                          (if (equal status "accepted") '((status . "accepted") (id . "confirmed"))
                            '((status . "rejected") (code . 20005) (message . "暂无权限"))))))))

(ert-deftest qq-guild-compose-captures-post-comment-and-nested-reply-identities ()
  (qq-guild-compose-test-with-app
   (let* ((forum (qq-guild-compose-test--forum (lambda () (car calls))))
          (post (qq-guild-test--post "p1"))
          (identity (alist-get 'identity post))
          details captured child)
     (with-current-buffer forum
       (goto-char (point-min)) (search-forward "论坛正文 p1")
       (setq captured (qq-guild-compose--scope))
       (search-forward "论坛正文 p2")
       (should (equal "p1" (alist-get 'post_id
                                      (alist-get 'post (plist-get (qq-guild-compose--prepare 'reply captured) :target)))))
       (setq details (qq-guild-forum--open 'comments identity "详情" "slot-a")))
     (qq-operation-test--drain)
     (setq child (copy-tree (qq-guild-test--comment "child")))
     (setf (alist-get 'author child) '((tiny_id . "9007199254741001") (name . "Carol")))
     (qq-operation-test--resolve
      (car calls)
      (list '(account_id . "slot-a")
            (cons 'page (list (cons 'post identity) '(cursor . "") '(next_cursor)
                              '(total_count . 2)
                              (cons 'comments (list (qq-guild-test--comment "parent" (list child) 1)))))))
     (qq-operation-test--drain)
     (with-current-buffer details
       (goto-char (point-min)) (search-forward "Comment child")
       (let* ((scope (qq-guild-compose--scope))
              (target (plist-get (qq-guild-compose--prepare 'reply scope) :target))
              (comment (plist-get (qq-guild-compose--prepare 'comment scope) :target)))
         (should (equal "parent" (alist-get 'comment_id target)))
         (should (equal "child" (alist-get 'reply_id target)))
         (should (equal "9007199254740999" (alist-get 'comment_author_tiny_id target)))
         (should (equal "9007199254741001" (alist-get 'target_tiny_id target)))
         (should (qq-guild--same-post-p identity (alist-get 'post target)))
         (should (equal "comment" (alist-get 'kind comment)))
         (should-not (alist-get 'reply_id comment))
         (let ((draft (qq-guild-forum-reply scope)))
           (with-current-buffer draft (insert "只提交新正文\n第二行"))
           (should (eq draft (qq-guild-forum-reply scope)))
           (with-current-buffer draft
             (should (equal "只提交新正文\n第二行" (alist-get 'text (qq-guild-compose--snapshot))))
             (should-not (string-match-p "Carol\\|Comment" (buffer-string))))))
       (kill-buffer details))
     (kill-buffer forum)
     (should-error (qq-guild-compose--prepare 'reply captured) :type 'user-error))))

(ert-deftest qq-guild-compose-rejection-preserves-draft-and-success-refreshes-source ()
  (qq-guild-compose-test-with-app
   (let* ((source (qq-guild-compose-test--forum (lambda () (car calls))))
          (draft (qq-guild-compose-test--draft source))
          target count)
     (with-current-buffer draft
       (insert "new post\n<&>")
       (setq target (copy-tree qq-guild-compose--target))
       (qq-runtime-with-account "slot-b" (qq-guild-compose-submit))
       (setq count (length calls))
       (should (equal "slot-a" (alist-get 'account_id (plist-get (car calls) :params))))
       (should (equal "new post\n<&>" (alist-get 'text (alist-get 'request (plist-get (car calls) :params)))))
       (should-error (qq-guild-compose-submit) :type 'user-error)
       (should (= count (length calls)))
       (should buffer-read-only))
     (qq-operation-test--resolve (car calls) (qq-guild-compose-test--receipt target "rejected"))
     (with-current-buffer draft
       (should-not buffer-read-only)
       (should-not qq-guild-compose--outcome)
       (should (equal "new post\n<&>" (buffer-string)))
       (goto-char (point-max)) (insert "\n改好")
       (qq-guild-compose-submit))
     (qq-operation-test--resolve (car calls) (qq-guild-compose-test--receipt target "accepted"))
     (qq-operation-test--drain)
     (should-not (buffer-live-p draft))
     (should (equal "guild.forum" (plist-get (car calls) :method)))
     (should (buffer-live-p source)))))

(ert-deftest qq-guild-compose-cancel-and-foreign-receipts-cannot-clear-or-resend-draft ()
  (qq-guild-compose-test-with-app
   (let* ((source (qq-guild-compose-test--forum (lambda () (car calls))))
          (draft (qq-guild-compose-test--draft source))
          target first count)
     (with-current-buffer draft
       (insert "keep me")
       (setq target (copy-tree qq-guild-compose--target))
       (qq-guild-compose-submit)
       (setq first (car calls))
       (qq-guild-compose-cancel)
       (should (eq qq-guild-compose--outcome 'unknown))
       (should-not buffer-read-only)
       (should (member (plist-get first :token) cancelled)))
     (qq-operation-test--resolve first (qq-guild-compose-test--receipt target "accepted"))
     (with-current-buffer draft
       (should (equal "keep me" (buffer-string)))
       (setq count (length calls))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
         (should-error (qq-guild-compose-submit) :type 'user-error))
       (should (= count (length calls)))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (qq-guild-compose-submit)))
     (qq-operation-test--resolve (car calls) (qq-guild-compose-test--receipt target "accepted" "slot-b"))
     (with-current-buffer draft
       (should (eq qq-guild-compose--outcome 'unknown))
       (should (equal "keep me" (buffer-string)))))))

(ert-deftest qq-guild-compose-session-change-and-owner-stop-preserve-uncertain-drafts ()
  (qq-guild-compose-test-with-app
   (let* ((source (qq-guild-compose-test--forum (lambda () (car calls))))
          (draft (qq-guild-compose-test--draft source))
          app target)
     (with-current-buffer draft
       (insert "survive session")
       (setq app qq-guild-compose--app target (copy-tree qq-guild-compose--target))
       (qq-guild-compose-submit))
     (appkit-app-send app '(qq-guild-reset "offline" "gateway-b" t))
     (qq-operation-test--drain)
     (qq-operation-test--resolve (car calls) (qq-guild-compose-test--receipt target "accepted"))
     (with-current-buffer draft
       (should (eq qq-guild-compose--outcome 'unknown))
       (should (equal "survive session" (buffer-string))))
     (appkit-app-close app)
     (qq-operation-test--drain)
     (should (buffer-live-p draft))
     (with-current-buffer draft
       (should-error (qq-guild-compose-submit) :type 'user-error)
       (should (equal "survive session" (buffer-string)))))))

(ert-deftest qq-guild-compose-confirmed-capture-does-not-discard-later-edits ()
  (qq-guild-compose-test-with-app
   (let* ((source (qq-guild-compose-test--forum (lambda () (car calls))))
          (draft (qq-guild-compose-test--draft source))
          target)
     (with-current-buffer draft
       (insert "captured")
       (setq target (copy-tree qq-guild-compose--target))
       (qq-guild-compose-submit)
       (let ((inhibit-read-only t)) (insert " later")))
     (qq-operation-test--resolve (car calls) (qq-guild-compose-test--receipt target "accepted"))
     (with-current-buffer draft
       (should (equal "captured later" (buffer-string)))
       (should (eq qq-guild-compose--outcome 'accepted))
       (should-error (qq-guild-compose-submit) :type 'user-error)))))

(ert-deftest qq-guild-compose-preflight-failure-is-retryable-but-malformed-receipt-is-unknown ()
  (qq-guild-compose-test-with-app
   (let* ((source (qq-guild-compose-test--forum (lambda () (car calls))))
          (draft (qq-guild-compose-test--draft source)))
     (with-current-buffer draft
       (insert "keep body")
       (qq-guild-compose-submit))
     (funcall (plist-get (plist-get (car calls) :options) :errback)
              '((code . "guild_self_identity_unavailable")) "等待频道身份")
     (with-current-buffer draft
       (should-not qq-guild-compose--outcome)
       (should-not buffer-read-only)
       (should (equal "keep body" (buffer-string)))
       (qq-guild-compose-submit))
     (qq-operation-test--resolve (car calls) '((account_id . "slot-a") (receipt . "invalid")))
     (with-current-buffer draft
       (should (eq qq-guild-compose--outcome 'unknown))
       (should-not buffer-read-only)
       (should (equal "keep body" (buffer-string)))))))

(provide 'qq-guild-compose-test)
;;; qq-guild-compose-test.el ends here
