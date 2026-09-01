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
   (buffer-local-value 'qq-chat--forward-plan-owner buffer)))

(ert-deftest qq-transient-prefixes-are-commands ()
  (should (commandp #'qq-chat-transient))
  (should (commandp #'qq-chat-message-transient))
  (should (commandp #'qq-chat-delete-transient))
  (should (commandp #'qq-chat-forward-transient))
  (should (commandp #'qq-transient-forward-merged))
  (should (commandp #'qq-chat-toggle-message-selection))
  (should (commandp #'qq-chat-toggle-message-essence))
  (should (commandp #'qq-chat-set-message-todo))
  (should (commandp #'qq-chat-complete-message-todo))
  (should (commandp #'qq-chat-cancel-message-todo))
  (should (commandp #'qq-chat-message-todo-transient))
  (should (commandp #'qq-chat-pin-friend))
  (should (commandp #'qq-chat-unpin-friend))
  (should (commandp #'qq-chat-friend-pin-transient))
  (should (commandp #'qq-chat-clear-message-selection))
  (should (commandp #'qq-presence-transient))
  (should (commandp #'qq-root-transient)))

(ert-deftest qq-transient-root-exposes-closed-presence-menu ()
  (cl-letf (((symbol-function 'qq-connect)
             (lambda () (interactive)))
            ((symbol-function 'qq-disconnect)
             (lambda () (interactive)))
            ((symbol-function 'qq-reset-session-state)
             (lambda () (interactive))))
    (let* ((root-objects (transient-suffixes 'qq-root-transient))
           (presence-entry
            (seq-find
             (lambda (suffix) (equal (oref suffix key) "p"))
             root-objects))
           (presence-objects (transient-suffixes 'qq-presence-transient))
           (commands
            (seq-keep
             (lambda (suffix)
               (when (memq (oref suffix command)
                           '(qq-presence-online
                             qq-presence-q-me
                             qq-presence-away
                             qq-presence-busy
                             qq-presence-do-not-disturb
                             qq-presence-invisible
                             qq-presence-custom))
                 (cons (oref suffix key) (oref suffix command))))
             presence-objects)))
      (should presence-entry)
      (should (eq (oref presence-entry command) 'qq-presence-transient))
      (should (eq (oref presence-entry inapt-if)
                  'qq-transient--presence-inapt-p))
      (should (= (length commands) 7))
      (dolist (expected
               '(("o" . qq-presence-online)
                 ("q" . qq-presence-q-me)
                 ("a" . qq-presence-away)
                 ("b" . qq-presence-busy)
                 ("d" . qq-presence-do-not-disturb)
                 ("i" . qq-presence-invisible)
                 ("c" . qq-presence-custom)))
        (should (eq (cdr (assoc (car expected) commands))
                    (cdr expected))))
      (dolist (suffix
               (seq-filter
                (lambda (candidate)
                  (memq (oref candidate command) (mapcar #'cdr commands)))
                presence-objects))
        (should (eq (oref suffix inapt-if)
                    'qq-transient--presence-inapt-p))))))

(ert-deftest qq-transient-root-exposes-managed-account-switch ()
  (cl-letf (((symbol-function 'qq-connect)
             (lambda () (interactive)))
            ((symbol-function 'qq-disconnect)
             (lambda () (interactive)))
            ((symbol-function 'qq-reset-session-state)
             (lambda () (interactive))))
    (let* ((objects (transient-suffixes 'qq-root-transient))
           (account-switch
            (seq-find
             (lambda (suffix) (equal (oref suffix key) "A"))
             objects)))
      (should account-switch)
      (should
       (eq (oref account-switch command) 'qq-root-switch-account)))))

(ert-deftest qq-transient-delete-prefix-separates-local-and-remote-mutations ()
  (let* ((delete-objects (transient-suffixes 'qq-chat-delete-transient))
         (local (seq-find (lambda (suffix) (equal (oref suffix key) "d"))
                          delete-objects))
         (recall (seq-find (lambda (suffix) (equal (oref suffix key) "r"))
                           delete-objects))
         (message-objects (transient-suffixes 'qq-chat-message-transient)))
    (should (eq (oref local command) 'qq-chat-delete-message))
    (should (eq (oref local inapt-if) 'qq-transient--delete-local-inapt-p))
    (should (eq (oref recall command) 'qq-chat-recall-message))
    (should (eq (oref recall inapt-if) 'qq-transient--recall-inapt-p))
    (should
     (eq (oref (seq-find (lambda (suffix) (equal (oref suffix key) "d"))
                         message-objects)
               command)
         'qq-chat-delete-transient))
    (should-not
     (seq-find (lambda (suffix) (equal (oref suffix key) "R"))
               message-objects))))

(ert-deftest qq-transient-message-prefix-exposes-essence-toggle ()
  (let* ((objects (transient-suffixes 'qq-chat-message-transient))
         (essence
          (seq-find
           (lambda (suffix) (equal (oref suffix key) "e"))
           objects)))
    (should essence)
    (should (eq (oref essence command) 'qq-chat-toggle-message-essence))))

(ert-deftest qq-transient-message-prefix-exposes-closed-todo-actions ()
  (let* ((message-objects (transient-suffixes 'qq-chat-message-transient))
         (todo-entry
          (seq-find
           (lambda (suffix) (equal (oref suffix key) "t"))
           message-objects))
         (todo-objects (transient-suffixes 'qq-chat-message-todo-transient))
         (commands
          (seq-keep
           (lambda (suffix)
             (when (memq (oref suffix command)
                         '(qq-chat-set-message-todo
                           qq-chat-complete-message-todo
                           qq-chat-cancel-message-todo))
               (cons (oref suffix key) (oref suffix command))))
           todo-objects)))
    (should todo-entry)
    (should (eq (oref todo-entry command) 'qq-chat-message-todo-transient))
    (should (= (length commands) 3))
    (should (eq (cdr (assoc "s" commands)) 'qq-chat-set-message-todo))
    (should
     (eq (cdr (assoc "c" commands)) 'qq-chat-complete-message-todo))
    (should (eq (cdr (assoc "x" commands)) 'qq-chat-cancel-message-todo))))

(ert-deftest qq-transient-chat-prefix-exposes-explicit-friend-pin-actions ()
  (let* ((chat-objects (transient-suffixes 'qq-chat-transient))
         (pin-entry
          (seq-find
           (lambda (suffix) (equal (oref suffix key) "t"))
           chat-objects))
         (pin-objects (transient-suffixes 'qq-chat-friend-pin-transient))
         (commands
          (mapcar (lambda (suffix)
                    (cons (oref suffix key) (oref suffix command)))
                  pin-objects)))
    (should pin-entry)
    (should (eq (oref pin-entry command) 'qq-chat-friend-pin-transient))
    (should (eq (oref pin-entry inapt-if)
                'qq-transient--friend-pin-inapt-p))
    (should (eq (cdr (assoc "p" commands)) 'qq-chat-pin-friend))
    (should (eq (cdr (assoc "u" commands)) 'qq-chat-unpin-friend))))

(ert-deftest qq-transient-forward-interface-has-no-legacy-mark-commands ()
  (should-not (fboundp 'qq-chat-forward-message))
  (should-not (fboundp 'qq-chat-toggle-forward-mark))
  (should-not (fboundp 'qq-chat-forward-marked-messages))
  (should-not (fboundp 'qq-chat-clear-forward-marks))
  (should-not (fboundp 'qq-transient--no-forward-marks-p))
  (should-not (fboundp 'qq-transient--forward-marked-inapt-p)))

(ert-deftest qq-transient-forward-prefix-exposes-native-merged-send ()
  (let* ((objects (transient-suffixes 'qq-chat-forward-transient))
         (suffixes
          (mapcar
           (lambda (suffix)
             (cons (oref suffix key) (oref suffix command)))
           objects))
         (merged
          (seq-find
           (lambda (object)
             (eq (oref object command) 'qq-transient-forward-merged))
           objects)))
    (should-not (assoc "i" suffixes))
    (should (eq (cdr (assoc "m" suffixes))
                'qq-transient-forward-merged))
    (should
     (eq (oref merged inapt-if)
         'qq-transient--forward-merged-inapt-p))
    (should (slot-boundp merged 'transient))
    (should-not (oref merged transient))
    (should-not (rassq 'qq-chat-forward-message suffixes))
    (should-not (rassq 'qq-chat-forward-marked-messages suffixes))))

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
              ((symbol-function 'qq-transient--message-at-point)
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


(ert-deftest qq-transient-merged-suffix-is-inapt-for-dataline-desktop ()
  (with-temp-buffer
    (qq-chat-mode)
    (let ((desktop-plan
           (qq-transient-test--forward-plan
            (current-buffer) "dataline:desktop:dev:a"
            "9007199254742007089"))
          (group-plan
           (qq-transient-test--forward-plan
            (current-buffer) "group:20001"
            "9007199254742007089")))
      (setq qq-chat--session-key "dataline:desktop:dev:a")
      (cl-letf (((symbol-function 'transient-scope)
                 (lambda (&rest _) desktop-plan)))
        (should (qq-transient--forward-merged-inapt-p)))
      (setq qq-chat--session-key "group:20001")
      (cl-letf (((symbol-function 'transient-scope)
                 (lambda (&rest _) group-plan)))
        (should-not (qq-transient--forward-merged-inapt-p))))))

(ert-deftest qq-transient-forward-prefix-rejects-dataline-mobile-plan ()
  (with-temp-buffer
    (qq-chat-mode)
    (let ((plan
           (qq-transient-test--forward-plan
            (current-buffer) "dataline:mobile:dev:a"
            "9007199254742007089"))
          setup-called)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (&rest _) (setq setup-called t))))
        (should-error (qq-chat-forward-transient plan) :type 'user-error))
      (should-not setup-called))))

(ert-deftest qq-transient-forward-prefix-captures-immutable-plan-as-scope ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let* ((plan
            (qq-transient-test--forward-plan
             (current-buffer) "group:20001"
             "9007199254742007001" "9007199254742007002"))
           setup-arguments)
      (cl-letf (((symbol-function 'qq-chat--current-forward-plan)
                 (lambda () plan))
                ((symbol-function 'transient-setup)
                 (lambda (&rest arguments)
                   (setq setup-arguments arguments))))
        (call-interactively #'qq-chat-forward-transient))
      (should
       (equal setup-arguments
              (list 'qq-chat-forward-transient nil nil :scope plan)))
      (should-error (qq-chat-forward-transient 'not-a-plan)
                    :type 'user-error))))

(ert-deftest qq-transient-forward-suffix-uses-prefix-scope ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "private:10001")
    (let* ((plan
            (qq-transient-test--forward-plan
             (current-buffer) "private:10001"
             "9007199254742007003"))
           merged-plan
           scope-prefixes)
      (cl-letf (((symbol-function 'transient-scope)
                 (lambda (&rest prefixes)
                   (push prefixes scope-prefixes)
                   plan))
                ((symbol-function 'qq-chat-forward-merged)
                 (lambda (&optional actual-plan _target)
                   (setq merged-plan actual-plan))))
        (call-interactively #'qq-transient-forward-merged))
      (should (eq merged-plan plan))
      (should
       (equal scope-prefixes
              '((qq-chat-forward-transient)))))))

(ert-deftest qq-transient-forward-scope-survives-real-suffix-lifecycle ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *qq-transient-forward-test*"))
          captured-plan)
      (unwind-protect
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (qq-chat-mode)
            (setq qq-chat--session-key "group:20001")
            (let ((plan
                   (qq-transient-test--forward-plan
                    buffer "group:20001"
                    "9007199254742007001")))
              (cl-letf (((symbol-function 'qq-chat-forward-merged)
                         (lambda (&optional actual-plan _target)
                           (setq captured-plan actual-plan))))
                (qq-chat-forward-transient plan)
                (should
                 (eq (transient-scope 'qq-chat-forward-transient) plan))
                ;; Execute through Transient's pre/post-command machinery.  A
                ;; direct function call would not establish
                ;; `transient-current-prefix' and would not test scope export.
                (execute-kbd-macro (kbd "m"))
                (should (eq captured-plan plan))
                (should-not
                 (transient-active-prefix 'qq-chat-forward-transient)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest qq-transient-forward-target-abort-keeps-selection-and-no-owner ()
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
                     qq-chat--forward-plan-owner)))
              (setq qq-chat--message-selection (list membership))
              (cl-letf (((symbol-function 'qq-chat--read-forward-target)
                         (lambda (&rest _arguments) (signal 'quit nil))))
                (qq-chat-forward-transient plan)
                (let (quit-seen)
                  (condition-case nil
                      (execute-kbd-macro (kbd "m"))
                    (quit (setq quit-seen t)))
                  (should quit-seen))
                (should-not qq-chat--forward-request)
                (should-not qq-chat--forward-request-owner)
                (should
                 (eq membership (car qq-chat--message-selection)))
                (should-not
                 (transient-active-prefix 'qq-chat-forward-transient)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

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
             (execute-kbd-macro (kbd "f"))
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
     (cl-letf (((symbol-function 'qq-transient--message-at-point)
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

(ert-deftest qq-root-binds-transient-menu ()
  (qq-transient-test-with-reset
   (with-temp-buffer
     (qq-root-mode)
     (should (eq (key-binding (kbd "?") t) 'qq-root-transient)))))

(provide 'qq-transient-test)

;;; qq-transient-test.el ends here
