;;; qq-transient-test.el --- Tests for qq-transient -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-chat)
(require 'qq-root)
(require 'qq-state)
(require 'qq-transient)

(defmacro qq-transient-test-with-reset (&rest body)
  "Run BODY with clean qq state."
  `(let ((qq-state-change-hook nil)
         (qq-media-cache-update-hook nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
         ;; ERT may catch a command error before the command loop clears exports.
         (transient-current-prefix nil)
         (transient-current-command nil)
         (transient-current-suffixes nil)
         (transient--current-suffix nil)
         (qq-chat-mode-hook
          (cons (lambda ()
                  (qq-runtime-bind-account "slot-a"))
                qq-chat-mode-hook)))
     (unwind-protect
         (qq-runtime-with-account "slot-a"
                                  (qq-state-reset)
                                  ,@body)
       (qq-runtime-stop-account "slot-a" t)
       (qq-state-reset))))

(defun qq-transient-test--forward-plan (buffer session-key &rest anchors)
  "Return an immutable synthetic forward plan for BUFFER and SESSION-KEY."
  (qq-chat--make-forward-plan
   buffer session-key anchors
   (mapcar (lambda (anchor) `((server-id . ,anchor))) anchors)
   nil
   (buffer-local-value 'qq-chat--forward-plan-owner buffer) nil))


(ert-deftest qq-transient-message-inapt-without-point-message ()
  (qq-transient-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (goto-char (point-min))
     (should (qq-transient--no-message-at-point-p))
     (should (qq-transient--poke-sender-inapt-p))
     (should (qq-transient--reply-inapt-p))
     (should (qq-transient--forward-inapt-p))
     (should (qq-transient--no-message-selection-p))
     (should (qq-transient--forward-selection-inapt-p))
     (should (qq-transient--recall-inapt-p)))))

(ert-deftest qq-transient-message-inapt-with-other-users-message ()
  (qq-transient-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "9007199254742007089")
       (session-key . "private:10001")
       (gateway-account-id . "slot-a")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (self-p . nil)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--set-history-window "9007199254742007089" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "hello")
     (should-not (qq-transient--no-message-at-point-p))
     (should-not (qq-transient--poke-sender-inapt-p))
     (should-not (qq-transient--reply-inapt-p))
     (should-not (qq-transient--forward-inapt-p))
     (should (qq-transient--no-message-selection-p))
     ;; With no explicit selection, the message at point is the plan.
     (should-not (qq-transient--forward-selection-inapt-p))
     ;; Addressability is independent from local authorship policy; QQ decides.
     (should-not (qq-transient--recall-inapt-p))
     (should-not (qq-transient--avatar-inapt-p))
     (setq qq-chat--message-selection
           (list
            (qq-chat--make-message-selection
             "9007199254742007089" '(test-selection-owner)
             '((server-id . "9007199254742007089")))))
     (goto-char (point-max))
     ;; An explicit selection remains forwardable away from the message row.
     (should-not (qq-transient--no-message-selection-p))
     (should-not (qq-transient--forward-selection-inapt-p))
     ;; A request owner disables another submission without changing selection.
     (setq qq-chat--forward-request-owner '(request-owner))
     (should (qq-transient--forward-selection-inapt-p)))))

(ert-deftest qq-transient-group-sequence-is-reply-only-without-message-id ()
  (let ((qq-chat--session-key "group:8209413637")
        (message
         '((id . "history:slot-a:group:8209413637:105544:none")
           (server-id)
           (session-key . "group:8209413637")
           (gateway-account-id . "slot-a")
           (message-seq . "105544")
           (status . received))))
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-transient--message)
               (lambda () message)))
      (should-not (qq-transient--reply-inapt-p))
      (should (qq-transient--recall-inapt-p))
      (setf (alist-get 'server-id message) "7348923749823749823")
      (should-not (qq-transient--recall-inapt-p))
      (setf (alist-get 'server-id message) nil
            (alist-get 'session-key message) "private:10001"
            qq-chat--session-key "private:10001")
      (should (qq-transient--reply-inapt-p))
      (should (qq-transient--recall-inapt-p)))))

(ert-deftest qq-transient-forward-prefix-rejects-device-sessions ()
  (dolist
      (session '("dataline:mobile:dev:a" "dataline:desktop:dev:a"))
    (with-temp-buffer
      (qq-chat-mode)
      (let
          ((plan
            (qq-transient-test--forward-plan (current-buffer) session
                                             "9007199254742007089")))
        (should-error (qq-chat-forward-transient plan)
                      :type 'user-error)))))

(defun qq-transient-test--invoke (command)
  "Run active Transient COMMAND without depending on its chosen key."
  (let ((keys (where-is-internal command transient--transient-map t)))
    (unless keys (ert-fail (format "No active command: %S" command)))
    (execute-kbd-macro keys)))

(defun qq-transient-test--message-buffer ()
  "Create a real, network-free chat with two loaded rows in the current account."
  (let ((account (qq-runtime-require-account-id "message fixture"))
        (buffer (generate-new-buffer " *qq-scoped-message-test*")))
    (qq-state-upsert-session
     "group:20001"
     '((type . group) (target-id . "20001") (title . "Scoped messages")) nil)
    (puthash
     "group:20001"
     (cl-loop for (id text time) in
              '(("9007199254742007089" "First message" 100)
                ("9007199254742007090" "Second message" 101))
              collect
              `((server-id . ,id) (session-key . "group:20001")
                (canonical-row-key . ,(number-to-string time))
                (gateway-account-id . ,account)
                (sender-id . "10001") (sender-name . "Alice")
                (time . ,time) (status . received)
                (segments . (((type . "text") (data . ((text . ,text))))))))
     qq-state--messages-by-session)
    (with-current-buffer buffer
      (qq-chat-mode)
      (qq-runtime-bind-account account)
      (setq-local qq-auto-mark-read nil
                  qq-chat--session-key "group:20001")
      (qq-chat--set-history-window "9007199254742007089" nil)
      (qq-chat-render)
      (qq-chat--set-draft "Unsent draft")
      (goto-char (point-min))
      (search-forward "First message"))
    buffer))

(ert-deftest qq-transient-message-scope-keeps-latest-target-after-point-movement ()
  (qq-transient-test-with-reset
   (save-window-excursion
     (let ((buffer (qq-transient-test--message-buffer)))
       (unwind-protect
           (with-current-buffer buffer
             (switch-to-buffer buffer)
             (qq-transient-msg-operate)
             (let ((messages (qq-state-session-messages qq-chat--session-key)))
               (setf (alist-get 'segments (car messages))
                     '(((type . "text") (data . ((text . "Edited after opening menu"))))))
               (puthash qq-chat--session-key messages qq-state--messages-by-session))
             (goto-char (point-min))
             (search-forward "Second message")
             (qq-transient-test--invoke #'qq-chat-reply-to-message)
             (should (equal (alist-get 'server-id (qq-chat--reply-message))
                            "9007199254742007089"))
             (should
              (equal (alist-get 'segments (qq-chat--reply-message))
                     '(((type . "text") (data . ((text . "Edited after opening menu")))))))
             (should (equal (qq-chat--current-draft-string) "Unsent draft")))
         (when (transient-active-prefix)
           (execute-kbd-macro (kbd "C-q")))
         (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest qq-transient-message-scope-rejects-reopened-surface ()
  (qq-transient-test-with-reset
   (save-window-excursion
     (let ((buffer (qq-transient-test--message-buffer)))
       (unwind-protect
           (with-current-buffer buffer
             (switch-to-buffer buffer)
             (qq-transient-msg-operate)
             (appkit-surface-stop (appkit-current-surface))
             (qq-chat-render)
             (goto-char (point-min))
             (search-forward "Second message")
             (should-error (qq-transient-test--invoke #'qq-chat-reply-to-message)
                           :type 'user-error)
             (should-not (qq-chat--reply-message))
             (should (equal (qq-chat--current-draft-string) "Unsent draft")))
         (when (transient-active-prefix)
           (execute-kbd-macro (kbd "C-q")))
         (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest qq-transient-destructive-prompt-revalidates-captured-surface ()
  (qq-transient-test-with-reset
   (let ((buffer (qq-transient-test--message-buffer))
         dispatched)
     (unwind-protect
         (with-current-buffer buffer
           (let ((message (qq-chat--message-at-point)))
             (cl-letf (((symbol-function 'y-or-n-p)
                        (lambda (&rest _)
                          (appkit-surface-stop (appkit-current-surface))
                          (qq-chat-render)
                          t))
                       ((symbol-function 'qq-core-delete-message-local)
                        (lambda (&rest _) (setq dispatched t))))
               (should-error (qq-chat-delete-message message)
                             :type 'user-error))
             (should-not dispatched)
             (should (= 2 (length (qq-state-session-messages "group:20001"))))
             (should (equal (qq-chat--current-draft-string) "Unsent draft"))))
       (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest qq-transient-forward-prompt-rejects-reopened-source-surface ()
  (qq-transient-test-with-reset
   (let ((buffer (qq-transient-test--message-buffer))
         dispatched)
     (unwind-protect
         (with-current-buffer buffer
           (let ((plan (qq-chat--current-forward-plan t)))
             (cl-letf (((symbol-function 'qq-chat--read-forward-target)
                        (lambda (&rest _)
                          (appkit-surface-stop (appkit-current-surface))
                          (qq-chat-render)
                          "group:30001"))
                       ((symbol-function 'qq-message-send-merged-forward)
                        (lambda (&rest _) (setq dispatched t))))
               (should-error (qq-chat-forward-merged plan) :type 'user-error))
             (should-not dispatched)
             (should-not qq-chat--forward-request-owner)
             (should (equal (qq-chat--current-draft-string) "Unsent draft"))))
       (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest qq-transient-forward-target-abort-keeps-selection-and-no-owner ()
  (qq-transient-test-with-reset
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *qq-transient-forward-abort-test*")))
      (unwind-protect
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (qq-chat-mode)
            (setq qq-chat--session-key "group:20001")
            (let* ((anchor "9007199254742007001")
                   (membership-owner '(test-selection-owner))
                   (membership
                    (qq-chat--make-message-selection
                     anchor membership-owner
                     (list (cons 'server-id anchor))))
                   (plan
                    (qq-chat--make-forward-plan
                     buffer "group:20001" (list anchor)
                     (list (list (cons 'server-id anchor)))
                     (list (cons anchor membership-owner))
                     qq-chat--forward-plan-owner (qq-chat--ensure-view))))
              (setq qq-chat--message-selection (list membership))
              (cl-letf (((symbol-function 'qq-chat--read-forward-target)
                         (lambda (&rest _arguments) (signal 'quit nil))))
                (qq-chat-forward-transient plan)
                (let (quit-seen)
                  (condition-case nil
                      (qq-transient-test--invoke #'qq-transient-forward-merged)
                    (quit (setq quit-seen t)))
                  (should quit-seen))
                (should-not qq-chat--forward-request)
                (should-not qq-chat--forward-request-owner)
                (should
                 (eq membership (car qq-chat--message-selection)))
                (should-not
                 (transient-active-prefix 'qq-chat-forward-transient)))))
        (when (transient-active-prefix)
          (execute-kbd-macro (kbd "C-q")))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))))

(ert-deftest qq-transient-forward-cancel-restores-calling-chat-prefix ()
  (qq-transient-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group)) nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254742007001")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "forward source")))
    qq-state--messages-by-session)
   (save-window-excursion
     (let ((buffer (generate-new-buffer " *qq-transient-forward-stack-test*")))
       (unwind-protect
           (with-current-buffer buffer
             (switch-to-buffer buffer)
             (qq-chat-mode)
             (setq qq-chat--session-key "group:20001")
             (qq-chat--set-history-window "9007199254742007001" nil)
             (qq-chat-render)
             (goto-char (point-min))
             (search-forward "forward source")
             (qq-chat-transient)
             (should (transient-active-prefix 'qq-chat-transient))
             (qq-transient-test--invoke #'qq-chat-forward-transient)
             (should (transient-active-prefix 'qq-chat-forward-transient))
             (execute-kbd-macro (kbd "C-g"))
             (should (transient-active-prefix 'qq-chat-transient))
             (execute-kbd-macro (kbd "C-g"))
             (should-not (transient-active-prefix)))
         (when (transient-active-prefix)
           (execute-kbd-macro (kbd "C-q")))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest qq-transient-forward-suffix-rejects-missing-prefix-scope ()
  (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) nil)))
    (should-error (call-interactively #'qq-transient-forward-merged)
                  :type 'user-error)))

(ert-deftest qq-transient-recall-requires-a-group-poke-message-reference ()
  (qq-transient-test-with-reset
   (let ((qq-chat--session-key "group:20001")
         (message
          '((server-id . "9007199254741004001")
            (session-key . "group:20001")
            (gateway-account-id . "slot-a")
            (self-p . t)
            (timeline-class . service)
            (segments
             . (((type . "gray-tip")
                 (data . ((kind . "poke")))))))))
     (cl-letf (((symbol-function 'qq-transient--message)
                (lambda () message)))
       (should-not (qq-transient--recall-inapt-p))
       (setf (alist-get 'session-key message) "private:10001")
       (should (qq-transient--recall-inapt-p))))))

(ert-deftest qq-transient-poke-is-inapt-in-service-session ()
  (qq-transient-test-with-reset
   (qq-state-upsert-session
    "service:u_mail"
    '((type . service) (title . "QQ邮箱提醒") (target-id . "u_mail"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "service:u_mail")
     (cl-letf (((symbol-function 'qq-chat--message-at-point)
                (lambda () '((sender-id . "10001")))))
       (should (qq-transient--poke-session-inapt-p))
       (should (qq-transient--poke-sender-inapt-p))))))

(ert-deftest qq-transient-composer-cancel-preserves-draft-until-reply-cleared ()
  (qq-transient-test-with-reset
   (save-window-excursion
     (let ((buffer (generate-new-buffer " *qq-transient-composer-test*")))
       (unwind-protect
           (with-current-buffer buffer
             (switch-to-buffer buffer)
             (qq-chat-mode)
             (setq qq-chat--session-key "private:10001")
             (qq-chat-render)
             (qq-chat--set-draft "keep this draft")
             (qq-chat--set-reply-message
              '((server-id . "9007199254742007089")
                (sender-name . "Alice")
                (raw-message . "reply target")))
             (goto-char (point-min))
             (qq-chat-transient)
             (qq-transient-test--invoke #'qq-chat-composer-transient)
             (qq-transient-test--invoke #'qq-chat-cancel-dwim)
             (should-not (qq-chat--reply-message))
             (should (equal (qq-chat--current-draft-string) "keep this draft"))
             (qq-chat-edit-draft)
             (qq-chat-composer-transient)
             (qq-transient-test--invoke #'qq-chat-cancel-dwim)
             (should (equal (qq-chat--current-draft-string) "")))
         (when (transient-active-prefix)
           (execute-kbd-macro (kbd "C-q")))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest qq-transient-input-rejects-message-and-media-operations ()
  (qq-transient-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat--set-draft "not a timeline message")
     (qq-chat-edit-draft)
     (dolist (command '(qq-transient-msg-operate
                        qq-chat-delete-transient
                        qq-chat-message-todo-transient
                        qq-chat-sender-transient
                        qq-chat-media-transient))
       (should-error (call-interactively command) :type 'user-error))
     (should (equal (qq-chat--current-draft-string) "not a timeline message")))))

(ert-deftest qq-transient-media-submenu-copies-point-segment-not-primary ()
  (qq-transient-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001") (type . private)) nil)
   (puthash
    "private:10001"
    '(((server-id . "9007199254742007089")
       (session-key . "private:10001")
       (gateway-account-id . "slot-a")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (segments
        . (((type . "image")
            (data . ((name . "first.png")
                     (url . "https://example.com/first.png"))))
           ((type . "video")
            (data . ((name . "second.mp4")
                     (remote_status . "available")
                     (url . "https://example.com/second.mp4"))))))))
    qq-state--messages-by-session)
   (save-window-excursion
     (let ((buffer (generate-new-buffer " *qq-transient-media-test*"))
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil))
       (unwind-protect
           (with-current-buffer buffer
             (switch-to-buffer buffer)
             (qq-chat-mode)
             (setq qq-chat--session-key "private:10001")
             (qq-chat--set-history-window "9007199254742007089" nil)
             (cl-letf (((symbol-function 'qq-media-segment-download-state)
                        (lambda (_segment)
                          '(:status not-downloaded :path "/tmp/media")))
                       ((symbol-function 'qq-media-segment-preview-capable-p)
                        (lambda (_segment) nil)))
               (qq-chat-render)
               (goto-char (point-min))
               (search-forward "second.mp4")
               (qq-transient-msg-operate)
               ;; Submenu entry must inherit the original exact media card.
               (goto-char (point-min))
               (search-forward "first.png")
               (qq-transient-test--invoke #'qq-chat-media-transient)
               (qq-transient-test--invoke #'appkit-media-card-copy-url)
               (should (equal (car kill-ring)
                              "https://example.com/second.mp4"))))
         (when (transient-active-prefix)
           (execute-kbd-macro (kbd "C-q")))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(provide 'qq-transient-test)

;;; qq-transient-test.el ends here
