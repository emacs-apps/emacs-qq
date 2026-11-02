;;; qq-reply-test.el --- Native quote presentation tests -*- lexical-binding: t; -*-

(require 'qq-test-state)

(require 'ert)
(require 'qq-reply)
(require 'qq-chat-test)
(require 'qq-message-test)
(require 'qq-forward-test)

(defun qq-reply-test--page (center)
  "Return a canonical private source page for CENTER."
  (copy-tree `((history_version . ,qq-message-history-port-version)
    (account_id . "slot-a")
    (conversation . ((kind . "private") (peer_uin . "10001")))
    (center . ,center) (center_row_key . "42")
    (messages . (,(qq-message-test-native-row
                   (qq-message-test-event :sequence "100" :client-sequence "77"
                                          :segments '(((kind . "text")
                                                       (payload . ((text . "historical source"))))))
                   "42")))
    (unsupported_message_count . 0) (older_cursor) (newer_cursor)
    (has_older . :false) (has_newer_materialized . :false))))

(ert-deftest qq-reply-historical-source-is-detached-and-identity-checked ()
  (qq-message-test-with-state
   (let* ((session "private:10001")
          (data '((message_seq . "77") (sender . ((uin . "10001")))
                  (sent_at . 1784700000)))
          (key (qq-reply-key session data))
          (center (nth 4 key))
          (response (qq-reply-test--page center))
          source request)
     (cl-letf (((symbol-function 'qq-message--call)
                (lambda (action owner params &rest options)
                  (should (equal action "message.get_history_around"))
                  (should (equal owner "slot-a"))
                  (should (= (alist-get 'count params) 1))
                  (should (equal (alist-get 'center params) center))
                  (funcall (plist-get options :callback)
                           (cdr (funcall (plist-get options :projector) (qq-runtime-state) response)))
                  nil)))
       (setq request (qq-reply--load nil key
                                     (lambda (value) (setq source value))
                                     (lambda (reason) (ert-fail reason)))))
     (should (appkit-cancellation-p request))
     (should (equal (alist-get 'preview source) "historical source"))
     (should (equal (alist-get 'native-client-sequence source) "77"))
     (should-not (qq-state-session-messages (qq-runtime-state) session))
     (setf (alist-get 'sent_at (alist-get 'message (car (alist-get 'messages response)))) 1)
     (should-error (qq-reply--check-source (qq-runtime-state) response key))
     (should-not (qq-state-session-messages (qq-runtime-state) session)))))

(ert-deftest qq-reply-private-collisions-require-sender-and-time ()
  (let* ((first '((server-id . "10") (native-client-sequence . "77")
                  (sender-id . "10001") (time . 100)))
         (second '((server-id . "11") (native-client-sequence . "77")
                   (sender-id . "10002") (time . 100)))
         (third '((server-id . "12") (native-client-sequence . "77")
                  (sender-id . "10001") (time . 200)))
         (all (list first second third)))
    (should-not (qq-reply-find-source '((message_seq . "77")) all 'private))
    (should (equal first
                   (qq-reply-find-source
                    '((message_seq . "77") (sender . ((uin . "10001"))) (sent_at . 100))
                    all 'private)))
    (should-not (qq-reply-find-source '((message_seq . "77")) all 'group))))

(ert-deftest qq-reply-visible-resource-refresh-preserves-chat-and-cancels-on-stop ()
  (qq-message-test-with-state
   (let* ((session "private:10001")
          (data '((message_seq . "77") (sender . ((uin . "10001"))) (sent_at . 1784700000)))
          (reply `((server-id . "200") (id . "200") (session-key . ,session)
                   (sender-name . "Peer") (sender-id . "10001") (time . 1784800000)
                   (segments . (((type . "reply") (data . ,data))
                                ((type . "text") (data . ((text . "answer"))))))))
          (second (copy-tree reply))
          callback projector surface (calls 0) canceled)
     (setf (alist-get 'id second) "201" (alist-get 'server-id second) "201")
     (qq-test-put (qq-runtime-state) :messages-by-session session (list reply second))
     (cl-letf (((symbol-function 'qq-message--call)
                (lambda (_action _owner _params &rest options)
                  (cl-incf calls)
                  (setq callback (plist-get options :callback)
                        projector (plist-get options :projector))
                  'reply-request))
	       ((symbol-function 'qq-server-cancel)
                (lambda (token) (push token canceled)))
	       ((symbol-function 'qq-chat--maybe-auto-load-older) #'ignore)
	       ((symbol-function 'qq-chat--maybe-auto-load-newer) #'ignore))
       (save-window-excursion
         (with-temp-buffer
           (qq-chat-mode)
           (qq-runtime-bind-account "slot-a")
           (setq qq-chat--session-key session)
           (qq-chat-test--window "200" nil)
           (switch-to-buffer (current-buffer))
           (qq-chat-render)
           (setq surface (appkit-current-surface))
           (qq-chat--set-draft "unfinished draft")
           (appkit-surface-send surface
                                (list 'qq-render (appkit-projection-change-create :full-p t)))
           (should (= calls 1))
           (let ((point-before (copy-marker (point)))
                 (history-before (copy-tree (qq-state-session-messages (qq-runtime-state) session))))
             (funcall callback (cdr (funcall projector (qq-runtime-state) (qq-reply-test--page (nth 4 (qq-reply-key session data))))))
             (appkit-loop-run-pass (appkit-app-loop (appkit-surface-app surface)))
             (appkit-loop-run-pass (appkit-surface-loop surface))
             (should (string-match-p "historical source" (buffer-substring-no-properties (point-min) (point-max))))
             (should (equal (qq-chat--current-draft-string) "unfinished draft"))
             (should (= (point) point-before))
             (should (equal history-before (qq-state-session-messages (qq-runtime-state) session)))
             (should (= calls 1)))
           ;; Removing viewport interest releases presentation, then revisiting
           ;; reacquires through the same broker.  Teardown cancels that request.
           (cl-letf (((symbol-function 'qq-reply--visible-anchors) (lambda () nil)))
             (appkit-surface-send surface
                                  (list 'qq-render (appkit-projection-change-create :frame-p t))))
           (appkit-surface-send surface
                                (list 'qq-render (appkit-projection-change-create :frame-p t)))
           (should (= calls 2))
           (appkit-surface-stop surface)
           (should (equal canceled '(reply-request)))
           (funcall callback '((preview . "late result")))
           (should-not (appkit-surface-live-p surface))))))))

(ert-deftest qq-forward-reply-quotes-work-without-source-and-do-not-cross-origins ()
  (let* ((quote '((kind . "reply")
                  (payload . ((target . ((kind . "native") (sequence . "77")
                                         (sender . ((uin . "10001")))
                                         (sent_at . 1710000000)
                                         (preview . "embedded old source")))))))
         (reply (qq-forward-native-message-to-internal (qq-runtime-state)
                                                       (qq-forward-test--message "2" "answer" :segments (list quote))))
         (wrong (qq-forward-native-message-to-internal (qq-runtime-state)
                                                       (qq-forward-test--message "1" "wrong group" :sequence "77"
                                                                                 :origin '((kind . "group") (group_uin . "30001")))))
         (model (qq-forward--reply-view-model
                 reply (qq-forward--messages-by-sequence (list wrong reply)))))
    (should-not (plist-get model :jump-entry-id))
    (should (equal (plist-get model :body) "embedded old source"))
    (with-temp-buffer
      (qq-forward--insert-reply-preview-line model (appkit-ui-make-prefix-state "" "") nil)
      (should (string-match-p "embedded old source" (buffer-string))))
    (setf (alist-get 'origin wrong) (copy-tree (alist-get 'origin reply)))
    (setq model (qq-forward--reply-view-model
                 reply (qq-forward--messages-by-sequence (list wrong reply))))
    (should (equal (plist-get model :jump-entry-id) "1"))
    ;; Private forward entries retain client sequence separately from server seq.
    (let* ((origin '((kind . "private") (peer_uin . "10002")
                     (participants . ["10001" "10002"])))
           (native (qq-forward-test--message "3" "private source" :sequence "999" :origin origin)))
      (setf (alist-get 'client_sequence native) "77"
            (alist-get 'origin reply) '((kind . "private") (peer_uin . "10001")
                                        (participants . ["10002" "10001"])))
      (setq model (qq-forward--reply-view-model
                   reply (qq-forward--messages-by-sequence
                          (list (qq-forward-native-message-to-internal (qq-runtime-state) native) reply))))
      (should (equal (plist-get model :jump-entry-id) "3"))
      (should (equal (plist-get model :body) "private source")))))



(ert-deftest qq-reply-native-quote-does-not-fetch-or-invent-a-source ()
  (qq-message-test-with-state
   (let* ((data '((message_seq . "77") (preview . "quoted outside cache")))
          (reply `((id . "200") (session-key . "group:20001")
                   (segments . (((type . "reply") (data . ,data)))))))
     (cl-letf (((symbol-function 'qq-reply--visible-anchors) (lambda () '("200"))))
       (should-not (appkit-render-result-resource-demands (qq-reply-render-result nil (list reply)))))
     (should (equal (plist-get (qq-reply-preview-model data nil) :body) "quoted outside cache"))
     (should-not (qq-state-session-messages (qq-runtime-state) "group:20001")))))

(ert-deftest qq-reply-group-source-without-snowflake-stays-detached ()
  (qq-message-test-with-state
   (let* ((session "group:20001")
          (data '((message_seq . "77") (sender . ((uin . "10001")))
                  (sent_at . 1784700000)))
          (key (qq-reply-key session data))
          (center (nth 4 key))
          (response (qq-reply-test--page center))
          (row (qq-message-test-native-row
                (qq-message-test-event
                 :message-id nil :sequence "77"
                 :conversation '((kind . "group") (group_uin . "20001")))
                "42")))
     (setf (alist-get 'conversation response) '((kind . "group") (group_uin . "20001"))
           (alist-get 'messages response) (list row))
     (let ((source (qq-reply--check-source (qq-runtime-state) response key)))
       (should (equal (alist-get 'session-key source) session))
       (should-not (alist-get 'server-id source))
       (should (equal (alist-get 'canonical-row-key source) "42"))
       (should (equal (plist-get (qq-reply-preview-model data source) :body) "hello")))
     (should-not (qq-state-session-messages (qq-runtime-state) session)))))

(provide 'qq-reply-test)
