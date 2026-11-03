;;; qq-typing-test.el --- Native input status tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'qq-test-state)
(require 'qq-typing)
(require 'qq-core)
(require 'qq-chat)
(require 'qq-root)

(defun qq-typing-test-status (timestamp &optional status no-uin timeout)
  `((conversation . ((kind . "private") (peer_uid . "u_peer")
                     ,@(unless no-uin '((peer_uin . "10002")))))
    (status . ,(or status "typing")) (timeout_seconds . ,(or timeout 6))
    (timestamp . ,timestamp) (interval . 2) (words . "native wording")))

(ert-deftest qq-typing-orders-native-stamps-without-extending-duplicates ()
  (let* ((before (qq-state-store-create
                  :account-id "slot-a"))
         (state before)
         (first (qq-typing-test-status "9007199254740993")))
    (appkit-data-step state (qq-typing--observe state first 100))
    (should-not (qq-state-session-actions before "private:10002"))
    (should-not (appkit-data-map-keys (qq-state-store-sessions state)))
    (should-not (qq-state-store-recent-session-keys state))
    (appkit-data-step state (qq-typing--observe state first 103))
    (should (= (plist-get (cdar (appkit-data-map-get
                                 (qq-state-store-actions state) "private:10002"))
                          :expires-at) 106))
    (appkit-data-step state (qq-typing--observe state
                                                (qq-typing-test-status "9007199254740994" "speaking") 104))
    (appkit-data-step state (qq-typing--expire state 106))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 106)))
      (should (equal (qq-state-action-text state "private:10002") "正在讲话…")))
    (appkit-data-step state (qq-typing--observe state
                                                (qq-typing-test-status "9007199254740995" "idle") 107))
    (appkit-data-step state (qq-typing--observe state first 108))
    (appkit-data-step state (qq-typing--observe state
                                                (qq-typing-test-status "9007199254740995") 108))
    (should-not (appkit-data-map-get (qq-state-store-actions state) "private:10002"))))

(ert-deftest qq-typing-resolves-pending-uid-with-original-deadline ()
  (let ((state (qq-state-store-create
                :account-id "slot-a")))
    (appkit-data-step state (qq-typing--observe state
                                                (qq-typing-test-status "42" nil t) 100))
    (should-not (appkit-data-map-keys (qq-state-store-actions state)))
    (should (appkit-data-map-get (qq-state-store-pending-input-status state) "u_peer"))
    ;; A duplicate may supply newly learned UIN metadata, but no new lifetime.
    (appkit-data-step state
      (qq-typing--observe state (qq-typing-test-status "42") 103))
    (should-not (appkit-data-map-keys (qq-state-store-pending-input-status state)))
    (should (= (plist-get (cdar (appkit-data-map-get
                                 (qq-state-store-actions state) "private:10002"))
                          :expires-at) 106))
    (appkit-data-step state (qq-typing--expire state 106))
    (should-not (appkit-data-map-keys (qq-state-store-actions state)))))

(ert-deftest qq-typing-message-clear-and-zero-timeout-do-not-resurrect ()
  (let ((state (qq-state-store-create
                :account-id "slot-a")))
    (appkit-data-step state (qq-typing--observe state (qq-typing-test-status "42") 100))
    (appkit-data-step state (qq-typing--message-arrived state "private:10002"))
    (appkit-data-step state (qq-typing--observe state (qq-typing-test-status "42") 101))
    (should-not (appkit-data-map-keys (qq-state-store-actions state)))
    (appkit-data-step state (qq-typing--observe state (qq-typing-test-status "43" nil nil 0) 102))
    (appkit-data-step state (qq-typing--observe state
                                                (qq-typing-test-status "43" "speaking") 103))
    (should-not (appkit-data-map-keys (qq-state-store-actions state)))))

(ert-deftest qq-typing-wire-validation-and-account-isolation ()
  (qq-test-with-state (qq-state-store-create
                       :account-id "slot-a")
                      (let ((event `((account_id . "slot-a")
                                     (input_status . ,(qq-typing-test-status "42" nil nil 60)))))
                        (cl-letf (((symbol-function 'qq-account-get)
                                   (lambda (id) (and (member id '("slot-a" "slot-b"))
                                                     '((phase . "online") (uid . "u_self"))))))
                          (qq-runtime-ensure-account "slot-b")
                          (qq-typing--handle-event "conversation.input_status" event)
                          (should (qq-state-action-text (qq-runtime-state "slot-a") "private:10002"))
                          (should-not (qq-state-action-text (qq-runtime-state "slot-b") "private:10002"))
                          ;; Reaction/recall/history updates do not imply the peer finished typing.
                          (qq-typing--state-changed
                           '(:account-id "slot-a" :type message :source event :mutation update
                             :session-key "private:10002" :message ((self-p . nil))))
                          (should (qq-state-action-text (qq-runtime-state "slot-a") "private:10002"))
                          (qq-typing--state-changed
                           '(:account-id "slot-a" :type message :source event :mutation create
                             :session-key "private:10002" :message ((self-p . nil))))
                          (should-not (qq-state-action-text (qq-runtime-state "slot-a") "private:10002"))
                          (setf (alist-get 'timestamp (alist-get 'input_status event)) "43")
                          (qq-typing--handle-event "conversation.input_status" event)
                          (should-error (qq-typing--check (append event '((unexpected . 1)))))
                          (let ((bad (copy-tree event)))
                            (setf (alist-get 'status (alist-get 'input_status bad)) "unknown")
                            (should-error (qq-typing--check bad)))
                          (qq-typing--connection-changed 'disconnected)
                          (should-not (qq-state-action-text (qq-runtime-state "slot-a") "private:10002"))
                          (qq-typing--handle-event "conversation.input_status" event)
                          (qq-runtime-stop-account "slot-a")
                          (qq-runtime-ensure-account "slot-a")
                          (should-not (qq-state-action-text (qq-runtime-state "slot-a") "private:10002"))))))

(ert-deftest qq-typing-core-sends-only-closed-private-request ()
  (let ((qq-runtime--context-account "slot-a") sent)
    (cl-letf (((symbol-function 'qq-core--start-request)
               (lambda (starter _success _failure &rest _) (funcall starter #'ignore #'ignore)))
              ((symbol-function 'qq-rpc-call)
               (lambda (method params &rest _) (setq sent (list method params)))))
      (qq-core-set-input-status "10002" "typing")
      (should (equal sent '("conversation.set_input_status"
                            ((account_id . "slot-a") (peer_uin . "10002") (status . "typing")))))
      (should-error (qq-core-set-input-status "u_peer" "typing"))
      (should-error (qq-core-set-input-status "10002" "cancel")))))

(ert-deftest qq-typing-composer-refreshes-only-on-changes-and-skips-groups ()
  (with-temp-buffer
    (let ((sender (appkit-chat-action-sender-create)) (kind 'private)
          (qq-chat-send-typing t) (qq-chat-typing-interval 5) (now 100) sent)
      (setq-local qq-chat--action-send-function (lambda (action) (push action sent)))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
                ((symbol-function 'qq-chat--session) (lambda () `((type . ,kind) (target-id . "10002"))))
                ((symbol-function 'qq-core-supports-p) (lambda (_) t))
                ((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-account-get) (lambda (_) '((phase . "online"))))
                ((symbol-function 'qq-chat--ensure-action-sender) (lambda (_) sender)))
        (qq-chat--set-my-action 'typing)
        (setq now 104) (qq-chat--set-my-action 'typing)
        (setq now 105) (qq-chat--set-my-action 'typing)
        (qq-chat--set-my-action 'cancel)
        (setq kind 'group now 110) (qq-chat--set-my-action 'typing)
        (should (equal (nreverse sent) '(typing typing nil)))))))

(ert-deftest qq-typing-expiry-refreshes-footer-and-session-preview ()
  (qq-test-with-state (qq-state-store-create
                       :account-id "slot-a")
                      (let ((now 100) (qq-chat-send-typing nil)
                            (qq-runtime-change-hook '(qq-typing--state-changed qq-chat--handle-state-change)))
                        (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
                                  ((symbol-function 'qq-account-get) (lambda (_) '((phase . "online"))))
                                  ((symbol-function 'qq-server-ready-p) (lambda () nil)))
                          (let ((buffer (qq-chat--open-buffer "private:10002")))
                            (unwind-protect
                                (with-current-buffer buffer
                                  (appkit-chatbuf-input-set-text "draft")
                                  (qq-typing--handle-event
                                   "conversation.input_status"
                                   `((account_id . "slot-a")
                                     (input_status . ,(qq-typing-test-status "42" nil nil 600))))
                                  (qq-test-drain)
                                  (should (string-match-p "正在输入…" (buffer-string)))
                                  (should (equal
                                           (appkit-ui-one-line-preview-text
                                            (qq-root--session-preview-model
                                             '((key . "private:10002") (type . private)
                                               (last-message-preview . "previous"))))
                                           ".. 正在输入…"))
                                  (let* ((handle (qq-runtime-account-input-status-expiry
                                                  (qq-runtime-account "slot-a")))
                                         (timer (appkit-handle-object handle)))
                                    (setq now 700)
                                    (apply (timer--function timer) (timer--args timer)))
                                  (qq-test-drain)
                                  (should-not (string-match-p "正在输入…" (buffer-string)))
                                  (should (equal (appkit-chatbuf-input-state) "draft"))
                                  (should (equal
                                           (appkit-ui-one-line-preview-text
                                            (qq-root--session-preview-model
                                             '((key . "private:10002") (type . private)
                                               (last-message-preview . "previous"))))
                                           "previous")))
                              (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest qq-typing-sender-keeps-account-route-through-surface-disposal ()
  (qq-test-with-state (qq-state-store-create
                       :account-id "slot-a")
                      (let ((qq-chat-send-typing t) sent)
                        (cl-letf (((symbol-function 'qq-account-get) (lambda (_) '((phase . "online"))))
                                  ((symbol-function 'qq-server-ready-p) (lambda () t))
                                  ((symbol-function 'qq-core-supports-p) (lambda (_) t))
                                  ((symbol-function 'qq-core-set-input-status)
                                   (lambda (target status &rest _)
                                     (push (list (qq-runtime-require-account-id) target status) sent))))
                          (let ((buffer (qq-chat--open-buffer "private:10002")))
                            (unwind-protect
                                (progn
                                  (with-current-buffer buffer (qq-chat--set-my-action 'typing))
                                  (qq-runtime-with-account "slot-b"
                                    (qq-runtime-ensure-account "slot-b")
                                    (kill-buffer buffer))
                                  (should (equal (nreverse sent)
                                                 '(("slot-a" "10002" "typing")
                                                   ("slot-a" "10002" "idle")))))
                              (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(provide 'qq-typing-test)
