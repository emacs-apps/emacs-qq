;;; qq-notifications-test.el --- Tests for qq notifications -*- lexical-binding: t; -*-

(require 'qq-test-state)

(require 'ert)
(require 'qq-notifications)
(require 'qq)

(defun qq-notifications-test--message (&optional mentions)
  "Return one incoming normalized test message with MENTIONS."
  (copy-tree
   `((id . "9007199254741004991")
     (server-id . "9007199254741004991")
     (session-key . "group:20001")
     (time . ,(truncate (float-time)))
     (sender-id . "10001")
     (sender-name . "Alice")
     (self-p . nil)
     (status . received)
     (preview . "hello")
     (mention-kinds . ,mentions))))

(defun qq-notifications-test--message-with-preview (preview)
  "Return one notification message whose preview is PREVIEW."
  ;; Byte compilation may share the literal tail returned by the fixture.
  ;; Keep each message structurally independent before mutating its preview.
  (let ((message (copy-tree
                  (qq-notifications-test--message '(at-me)))))
    (setf (alist-get 'preview message) preview)
    message))

(defun qq-notifications-test--session (_key)
  "Return one observable test session for ignored key _KEY."
  '((key . "group:20001") (type . group) (target-id . "20001")
    (title . "Group") (muted-p . nil)))

(defun qq-notifications-test--accounts (&rest account-ids)
  "Return an isolated registry with managed ACCOUNT-IDS."
  (let ((accounts (make-hash-table :test #'equal)))
    (dolist (account-id account-ids)
      (puthash account-id
               `((account_id . ,account-id)
                 (label . "Personal") (uin . "10000"))
               accounts))
    accounts))

(defmacro qq-notifications-test--with-accounts (&rest body)
  "Run BODY with two isolated managed account state stores."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--account-id
          (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
         (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create))) (qq-multi-account nil)
         (qq-account--accounts (qq-notifications-test--accounts "account-a" "account-b"))
         (qq-account--account-order '("account-a" "account-b")) (qq-account--current-account-id "account-a")
         (qq-account-selection-changed-hook nil) (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-runtime--app nil))
     (qq-runtime-with-account "account-a"
                              (unwind-protect
                                  (progn
                                    (dolist (account-id qq-account--account-order)
                                      (qq-runtime-with-account account-id
                                                               (qq-runtime-transact #'qq-state-upsert-session
                                                                                    "group:20001" (qq-notifications-test--session nil))))
                                    ,@body)
                                (qq-runtime-stop)))))

(defmacro qq-notifications-test--with-display (&rest body)
  "Run BODY with isolated notification and avatar state."
  (declare (indent 0) (debug t))
  `(qq-notifications-test--with-accounts
    (let ((qq-notifications-mode t)
          (qq-notifications--resetting-p nil)
          (qq-notifications--generation 0)
          (qq-notifications--display-owner nil)
          (qq-notifications--last-id nil)
          (qq-notifications--last-id-owner nil)
          (qq-notifications--delay-owners nil)
          (qq-notifications--timeout-owners nil)
          (qq-notifications--history nil)
          (qq-notifications--history-buffer nil)
          (qq-notifications--seen-anchors (make-hash-table :test #'equal))
          (qq-notifications--seen-anchor-order nil)
          (qq-notifications-timeout 60)
          (qq-notifications-extra-args nil)
          (qq-media--resource-cache (make-hash-table :test #'equal))
          (qq-media-cache-update-hook '(qq-notifications--refresh-avatar)))
      (unwind-protect
          (cl-letf (((symbol-function 'qq-notifications--chat-observable-p)
                     (lambda (_key) nil))
                    ((symbol-function 'qq-media-group-avatar-image) #'ignore)
                    ((symbol-function 'qq-media-message-avatar-image) #'ignore))
            ,@body)
        (qq-notifications--forget-avatar-owner qq-notifications--display-owner)
        (unless (eq qq-notifications--last-id-owner qq-notifications--display-owner)
          (qq-notifications--forget-avatar-owner qq-notifications--last-id-owner))
        (dolist (owner (append qq-notifications--delay-owners
                               qq-notifications--timeout-owners))
          (qq-notifications--cancel-owner-timer owner))
        (when (qq-notifications--history-buffer-owned-p
               qq-notifications--history-buffer)
          (qq-notifications--dispose-history-buffer
           qq-notifications--history-buffer))))))

(ert-deftest qq-notifications-focused-chat-does-not-observe-another-account ()
  (qq-notifications-test--with-accounts
   (let ((qq-multi-account t))
     (save-window-excursion
       (with-temp-buffer
         (setq major-mode 'qq-chat-mode)
         (setq-local qq-runtime--account-id "account-b")
         (setq-local qq-chat--session-key "group:20001")
         (set-window-buffer (selected-window) (current-buffer))
         (cl-letf (((symbol-function 'frame-focus-state)
                    (lambda (_frame) t)))
           (should (qq-notifications-message-notify-p
                    (qq-notifications-test--message)))
           (qq-runtime-with-account "account-b"
                                    (should-not (qq-notifications-message-notify-p
                                                 (qq-notifications-test--message))))))))))

(ert-deftest qq-notifications-account-scope-keeps-colliding-message-ids-isolated ()
  (qq-notifications-test--with-display
   (let ((qq-notifications-delay 0)
         (qq-notifications-timeout nil)
         (message-a (qq-notifications-test--message-with-preview "A ONLY"))
         (message-b (qq-notifications-test--message-with-preview "B ONLY"))
         bodies)
     (cl-letf (((symbol-function 'notifications-notify)
                (lambda (&rest arguments)
                  (push (plist-get arguments :body) bodies)
                  (length bodies)))
               ((symbol-function 'notifications-close-notification) #'ignore))
       (dolist (account-id '("account-b" "account-a" "account-a"))
         (qq-notifications--handle-state-change
          (list :type 'message :mutation 'create :source 'event
                :account-id account-id
                :message (if (equal account-id "account-a")
                             message-a message-b))))
       (should (equal bodies '("@你  A ONLY")))
       ;; Selection is global even while execution remains in account A.
       (qq-account--set-current-account "account-b")
       (qq-notifications--handle-state-change
        (list :type 'message :mutation 'create :source 'event
              :account-id "account-b" :message message-b))
       (should (equal bodies '("@你  B ONLY" "@你  A ONLY")))
       (setq qq-multi-account t)
       (dolist (message (list message-a message-b))
         (setf (alist-get 'id message) "9007199254741004992"
               (alist-get 'server-id message) "9007199254741004992"))
       (dolist (account-id '("account-a" "account-b"))
         (qq-notifications--handle-state-change
          (list :type 'message :mutation 'create :source 'event
                :account-id account-id
                :message (if (equal account-id "account-a")
                             message-a message-b))))
       (should (equal bodies
                      '("@你  B ONLY" "@你  A ONLY"
                        "@你  B ONLY" "@你  A ONLY")))
       (should (equal
                (mapcar (lambda (message) (alist-get 'account-id message))
                        (ring-elements qq-notifications--history))
                '("account-b" "account-a" "account-b" "account-a")))))))

(ert-deftest qq-notifications-delayed-delivery-rechecks-selection-and-mode ()
  (qq-notifications-test--with-display
   (let ((qq-notifications-timeout nil)
         (message-a (qq-notifications-test--message-with-preview "A SECRET"))
         (message-b (qq-notifications-test--message-with-preview "B CURRENT"))
         scheduled bodies)
     (cl-letf (((symbol-function 'run-with-timer)
                (lambda (_delay _repeat function &rest arguments)
                  (push (cons function arguments) scheduled)
                  'notification-delay))
               ((symbol-function 'notifications-notify)
                (lambda (&rest arguments)
                  (push (plist-get arguments :body) bodies)
                  'notification))
               ((symbol-function 'notifications-close-notification) #'ignore))
       (qq-notifications--schedule-delayed message-a)
       (qq-account--set-current-account "account-b")
       (apply (caar scheduled) (cdar scheduled))
       (should-not bodies)
       (should-not qq-notifications--history)
       (should-not qq-notifications--delay-owners)
       (setq scheduled nil qq-multi-account t)
       (qq-notifications--schedule-delayed message-a)
       (qq-runtime-with-account "account-b"
                                (qq-notifications--schedule-delayed message-b))
       (setq qq-multi-account nil)
       (dolist (callback (reverse scheduled))
         (apply (car callback) (cdr callback)))
       (should (equal bodies '("@你  B CURRENT")))
       (should-not qq-notifications--delay-owners)
       (should (equal
                (mapcar (lambda (message) (alist-get 'account-id message))
                        (ring-elements qq-notifications--history))
                '("account-b")))))))

(ert-deftest qq-notifications-avatar-callback-rechecks-selection-and-mode ()
  (dolist (transition '(selection mode))
    (qq-notifications-test--with-display
     (let (icon requests closed)
       (cl-letf (((symbol-function 'qq-notifications--avatar-icon)
                  (lambda (_owner _keys) icon))
                 ((symbol-function 'notifications-notify)
                  (lambda (&rest arguments)
                    (push arguments requests)
                    'notification))
                 ((symbol-function 'notifications-close-notification)
                  (lambda (id) (push id closed))))
         (when (eq transition 'mode)
           (setq qq-multi-account t)
           (qq-account--set-current-account "account-b"))
         (qq-notifications--show (qq-notifications-test--message))
         (if (eq transition 'selection)
             (qq-account--set-current-account "account-b")
           (setq qq-multi-account nil))
         (setq icon "/tmp/old-account-avatar.svg")
         (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
         (should (= (length requests) 1))
         ;; Losing presentation scope must not disable owned timeout cleanup.
         (let ((timeout (car qq-notifications--timeout-owners)))
           (cancel-timer (plist-get timeout :timer))
           (qq-notifications--timeout-fired timeout))
         (should (equal closed '(notification)))
         (should-not qq-notifications--last-id)
         (should-not qq-notifications--timeout-owners))))))

(ert-deftest qq-notifications-backend-result-cannot-cross-selection-change ()
  (qq-notifications-test--with-display
   (let ((qq-notifications-timeout nil) closed)
     (cl-letf (((symbol-function 'notifications-notify)
                (lambda (&rest _arguments)
                  (qq-account--set-current-account "account-b")
                  'stale-result))
               ((symbol-function 'notifications-close-notification)
                (lambda (id) (push id closed))))
       (should-not (qq-notifications--show
                    (qq-notifications-test--message-with-preview "A SECRET")))
       (should (equal closed '(stale-result)))
       (should-not qq-notifications--last-id)
       (should-not qq-notifications--history)))))

(ert-deftest qq-notifications-click-and-history-open-originating-account ()
  (qq-notifications-test--with-display
   (save-window-excursion
     (let ((qq-multi-account t)
           (qq-notifications-timeout nil)
           arguments)
       (cl-letf (((symbol-function 'notifications-notify)
                  (lambda (&rest values)
                    (setq arguments values)
                    'notification))
                 ((symbol-function 'notifications-close-notification) #'ignore)
                 ((symbol-function 'qq-chat--load-initial-history) #'ignore)
                 ((symbol-function 'qq-chat-goto-message) #'ignore)
                 ((symbol-function 'qq-completion-preload-members) #'ignore))
         (qq-notifications--show
          (qq-notifications-test--message-with-preview "A HISTORY"))
         (qq-runtime-with-account "account-b"
                                  (qq-notifications--show
                                   (qq-notifications-test--message-with-preview "B SECRET")))
         (funcall (plist-get arguments :on-action) 'notification "default")
         (should (derived-mode-p 'qq-chat-mode))
         (should (equal qq-runtime--account-id "account-b"))
         (should (equal qq-chat--session-key "group:20001"))
         (qq-notifications-history)
         (button-activate (button-at (point-min)))
         (should (derived-mode-p 'qq-chat-mode))
         (should (equal qq-runtime--account-id "account-b"))
         (setq qq-multi-account nil)
         (qq-notifications-history)
         (should (string-match-p "A HISTORY" (buffer-string)))
         (should-not (string-match-p "B SECRET" (buffer-string)))
         (button-activate (button-at (point-min)))
         (should (derived-mode-p 'qq-chat-mode))
         (should (equal qq-runtime--account-id "account-a")))))))

(ert-deftest qq-notifications-muted-chat-requires-native-priority-mention ()
  (qq-notifications-test--with-accounts
   (cl-letf (((symbol-function 'qq-state-session)
              (lambda (state _key) '((key . "group:20001") (type . group) (title . "Group") (muted-p . t))))
             ((symbol-function 'qq-notifications--chat-observable-p)
              (lambda (_key) nil)))
     (should-not (qq-notifications-message-notify-p
                  (qq-notifications-test--message)))
     (should (qq-notifications-message-notify-p
              (qq-notifications-test--message '(at-me))))
     (let ((qq-notifications-at-all-breaks-mute t))
       (should (qq-notifications-message-notify-p
                (qq-notifications-test--message '(at-all)))))
     (let ((qq-notifications-at-all-breaks-mute nil))
       (should-not (qq-notifications-message-notify-p
                    (qq-notifications-test--message '(at-all))))))))

(ert-deftest qq-notifications-suppresses-selected-focused-chat ()
  (qq-notifications-test--with-accounts
   (cl-letf (((symbol-function 'qq-notifications--chat-observable-p)
              (lambda (_key) t)))
     (should-not (qq-notifications-message-notify-p
                  (qq-notifications-test--message '(at-me)))))))

(ert-deftest qq-notifications-nested-backend-display-wins-exact-owner ()
  "A same-generation nested display makes the outer returned id an orphan."
  (let ((qq-notifications-mode t)
        (qq-multi-account nil)
        (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a")
        (qq-notifications-extra-args '(:app-icon "/tmp/test-icon.png"))
        (qq-notifications--resetting-p nil)
        (qq-notifications--generation 3)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications-timeout nil)
        (outer (qq-notifications-test--message-with-preview "OUTER SECRET"))
        (inner (qq-notifications-test--message-with-preview "INNER MESSAGE"))
        inner-result
        closed)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (cl-letf (((symbol-function 'qq-state-session)
                                   (lambda (state &rest args) (apply #'qq-notifications-test--session args)))
                                  ((symbol-function 'qq-notifications--chat-observable-p)
                                   (lambda (_key) nil))
                                  ((symbol-function 'notifications-notify)
                                   (lambda (&rest arguments)
                                     (if (string-match-p "OUTER SECRET"
                                                         (plist-get arguments :body))
                                         (progn
                                           (setq inner-result (qq-notifications--show inner))
                                           'outer-id)
                                       'inner-id)))
                                  ((symbol-function 'notifications-close-notification)
                                   (lambda (id) (push id closed))))
                          (should-not (qq-notifications--show outer))
                          (should (eq inner-result 'inner-id))
                          (should (eq qq-notifications--last-id 'inner-id))
                          (should (eq qq-notifications--last-id-owner
                                      qq-notifications--display-owner))
                          (should (equal closed '(outer-id)))
                          (should (equal (mapcar (lambda (message)
                                                   (alist-get 'preview message))
                                                 (ring-elements qq-notifications--history))
                                         '("INNER MESSAGE")))))))

(ert-deftest qq-notifications-nested-backend-reused-id-keeps-exact-owner ()
  "A stale outer unwind must not close the nested display's reused backend id."
  (let ((qq-notifications-mode t)
        (qq-multi-account nil)
        (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a")
        (qq-notifications-extra-args '(:app-icon "/tmp/test-icon.png"))
        (qq-notifications--resetting-p nil)
        (qq-notifications--generation 3)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications-timeout nil)
        (outer (qq-notifications-test--message-with-preview "OUTER REUSED"))
        (inner (qq-notifications-test--message-with-preview "INNER REUSED"))
        inner-result
        closed)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (cl-letf (((symbol-function 'qq-state-session)
                                   (lambda (state &rest args) (apply #'qq-notifications-test--session args)))
                                  ((symbol-function 'qq-notifications--chat-observable-p)
                                   (lambda (_key) nil))
                                  ((symbol-function 'notifications-notify)
                                   (lambda (&rest arguments)
                                     (when (string-match-p "OUTER REUSED"
                                                           (plist-get arguments :body))
                                       (setq inner-result (qq-notifications--show inner)))
                                     'reused-id))
                                  ((symbol-function 'notifications-close-notification)
                                   (lambda (id) (push id closed))))
                          (should-not (qq-notifications--show outer))
                          (should (eq inner-result 'reused-id))
                          (should (eq qq-notifications--last-id 'reused-id))
                          (should (eq qq-notifications--last-id-owner
                                      qq-notifications--display-owner))
                          (should-not closed)
                          (should (equal (mapcar (lambda (message)
                                                   (alist-get 'preview message))
                                                 (ring-elements qq-notifications--history))
                                         '("INNER REUSED")))))))

(ert-deftest qq-notifications-reset-backend-reused-id-keeps-new-generation ()
  "A stale pre-reset unwind must not close a new account's reused backend id."
  (let ((qq-notifications-mode t)
        (qq-multi-account nil)
        (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a")
        (qq-notifications-extra-args '(:app-icon "/tmp/test-icon.png"))
        (qq-notifications--resetting-p nil)
        (qq-notifications--generation 8)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications-timeout nil)
        (old (qq-notifications-test--message-with-preview
              "OLD_ACCOUNT_SECRET"))
        (new (qq-notifications-test--message-with-preview "NEW ACCOUNT"))
        new-result
        closed)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (cl-letf (((symbol-function 'qq-state-session)
                                   (lambda (state &rest args) (apply #'qq-notifications-test--session args)))
                                  ((symbol-function 'qq-notifications--chat-observable-p)
                                   (lambda (_key) nil))
                                  ((symbol-function 'notifications-notify)
                                   (lambda (&rest arguments)
                                     (when (string-match-p "OLD_ACCOUNT_SECRET"
                                                           (plist-get arguments :body))
                                       (qq-notifications-reset-session-state)
                                       (setq new-result (qq-notifications--show new)))
                                     'reused-id))
                                  ((symbol-function 'notifications-close-notification)
                                   (lambda (id) (push id closed))))
                          (should-not (qq-notifications--show old))
                          (should (eq new-result 'reused-id))
                          (should (> qq-notifications--generation 8))
                          (should (eq qq-notifications--last-id 'reused-id))
                          (should (eq qq-notifications--last-id-owner
                                      qq-notifications--display-owner))
                          (should (= (plist-get qq-notifications--last-id-owner :generation)
                                     qq-notifications--generation))
                          (should-not closed)
                          (should (equal (mapcar (lambda (message)
                                                   (alist-get 'preview message))
                                                 (ring-elements qq-notifications--history))
                                         '("NEW ACCOUNT")))
                          (should-not
                           (string-match-p "OLD_ACCOUNT_SECRET"
                                           (prin1-to-string qq-notifications--history)))))))

(ert-deftest qq-notifications-reset-during-backend-closes-returned-stale-id ()
  (let ((qq-notifications-mode t)
        (qq-multi-account nil)
        (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a")
        (qq-notifications-extra-args '(:app-icon "/tmp/test-icon.png"))
        (qq-notifications--resetting-p nil)
        (qq-notifications--generation 8)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications-timeout nil)
        closed)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (cl-letf (((symbol-function 'qq-state-session)
                                   (lambda (state &rest args) (apply #'qq-notifications-test--session args)))
                                  ((symbol-function 'qq-notifications--chat-observable-p)
                                   (lambda (_key) nil))
                                  ((symbol-function 'notifications-notify)
                                   (lambda (&rest _arguments)
                                     (qq-notifications-reset-session-state)
                                     'stale-returned-id))
                                  ((symbol-function 'notifications-close-notification)
                                   (lambda (id) (push id closed))))
                          (should-not
                           (qq-notifications--show
                            (qq-notifications-test--message-with-preview "OLD_ACCOUNT_SECRET")))
                          (should (equal closed '(stale-returned-id)))
                          (should-not qq-notifications--last-id)
                          (should-not qq-notifications--last-id-owner)
                          (should-not qq-notifications--display-owner)
                          (should-not qq-notifications--history)
                          (should-not qq-notifications--timeout-owners)))))

(ert-deftest qq-notifications-nested-display-during-close-aborts-outer ()
  "Backend close reentry must not let the outer display replace the nested id."
  (let* ((qq-notifications-mode t)
         (qq-multi-account nil)
         (qq-account--accounts (qq-notifications-test--accounts "account-a"))
         (qq-account--current-account-id "account-a")
         (qq-notifications-extra-args '(:app-icon "/tmp/test-icon.png"))
         (qq-notifications--resetting-p nil)
         (qq-notifications--generation 4)
         (previous-owner '(:generation 4))
         (qq-notifications--display-owner previous-owner)
         (qq-notifications--last-id 'previous-id)
         (qq-notifications--last-id-owner previous-owner)
         (qq-notifications--delay-owners nil)
         (qq-notifications--timeout-owners nil)
         (qq-notifications--history nil)
         (qq-notifications-timeout nil)
         (outer (qq-notifications-test--message-with-preview "OUTER CLOSE"))
         (inner (qq-notifications-test--message-with-preview "INNER CLOSE"))
         inner-result
         backend-bodies
         closed)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (cl-letf (((symbol-function 'qq-state-session)
                                   (lambda (state &rest args) (apply #'qq-notifications-test--session args)))
                                  ((symbol-function 'qq-notifications--chat-observable-p)
                                   (lambda (_key) nil))
                                  ((symbol-function 'notifications-notify)
                                   (lambda (&rest arguments)
                                     (push (plist-get arguments :body) backend-bodies)
                                     'inner-id))
                                  ((symbol-function 'notifications-close-notification)
                                   (lambda (id)
                                     (push id closed)
                                     (when (eq id 'previous-id)
                                       (setq inner-result (qq-notifications--show inner))))))
                          (should-not (qq-notifications--show outer))
                          (should (eq inner-result 'inner-id))
                          (should (eq qq-notifications--last-id 'inner-id))
                          (should (equal backend-bodies '("@你  INNER CLOSE")))
                          (should (equal closed '(previous-id)))
                          (should (equal (mapcar (lambda (message)
                                                   (alist-get 'preview message))
                                                 (ring-elements qq-notifications--history))
                                         '("INNER CLOSE")))))))

(ert-deftest qq-notifications-history-reentry-rolls-back-stale-outer-record ()
  "History insertion reentry retains only the exact nested display record."
  (let ((qq-notifications-mode t)
        (qq-multi-account nil)
        (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a")
        (qq-notifications-extra-args '(:app-icon "/tmp/test-icon.png"))
        (qq-notifications--resetting-p nil)
        (qq-notifications--generation 5)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications-timeout nil)
        (outer (qq-notifications-test--message-with-preview "OUTER HISTORY"))
        (inner (qq-notifications-test--message-with-preview "INNER HISTORY"))
        (original-ring-insert (symbol-function 'ring-insert))
        reentered
        closed)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (cl-letf (((symbol-function 'qq-state-session)
                                   (lambda (state &rest args) (apply #'qq-notifications-test--session args)))
                                  ((symbol-function 'qq-notifications--chat-observable-p)
                                   (lambda (_key) nil))
                                  ((symbol-function 'notifications-notify)
                                   (lambda (&rest arguments)
                                     (if (string-match-p "OUTER HISTORY"
                                                         (plist-get arguments :body))
                                         'outer-id
                                       'inner-id)))
                                  ((symbol-function 'notifications-close-notification)
                                   (lambda (id) (push id closed)))
                                  ((symbol-function 'ring-insert)
                                   (lambda (ring object)
                                     (funcall original-ring-insert ring object)
                                     (unless reentered
                                       (setq reentered t)
                                       (qq-notifications--show inner)))))
                          (should-not (qq-notifications--show outer))
                          (should (eq qq-notifications--last-id 'inner-id))
                          (should (equal closed '(outer-id)))
                          (should (equal (mapcar (lambda (message)
                                                   (alist-get 'preview message))
                                                 (ring-elements qq-notifications--history))
                                         '("INNER HISTORY")))))))

(ert-deftest qq-notifications-timeout-scheduling-reentry-keeps-nested-owner ()
  "Timer creation reentry cannot restore the already-retired outer display."
  (let ((qq-notifications-mode t)
        (qq-multi-account nil)
        (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a")
        (qq-notifications-extra-args '(:app-icon "/tmp/test-icon.png"))
        (qq-notifications--resetting-p nil)
        (qq-notifications--generation 6)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications-timeout 2)
        (outer (qq-notifications-test--message-with-preview "OUTER TIMER"))
        (inner (qq-notifications-test--message-with-preview "INNER TIMER"))
        inner-result
        reentered
        cancelled
        closed)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (cl-letf (((symbol-function 'qq-state-session)
                                   (lambda (state &rest args) (apply #'qq-notifications-test--session args)))
                                  ((symbol-function 'qq-notifications--chat-observable-p)
                                   (lambda (_key) nil))
                                  ((symbol-function 'notifications-notify)
                                   (lambda (&rest arguments)
                                     (if (string-match-p "OUTER TIMER"
                                                         (plist-get arguments :body))
                                         'outer-id
                                       'inner-id)))
                                  ((symbol-function 'notifications-close-notification)
                                   (lambda (id) (push id closed)))
                                  ((symbol-function 'run-with-timer)
                                   (lambda (_delay _repeat _function &rest _arguments)
                                     (if reentered
                                         'inner-timer
                                       (setq reentered t
                                             inner-result (qq-notifications--show inner))
                                       'outer-timer)))
                                  ((symbol-function 'timerp)
                                   (lambda (timer)
                                     (memq timer '(outer-timer inner-timer))))
                                  ((symbol-function 'cancel-timer)
                                   (lambda (timer) (push timer cancelled))))
                          (should-not (qq-notifications--show outer))
                          (should (eq inner-result 'inner-id))
                          (should (eq qq-notifications--last-id 'inner-id))
                          (should (eq qq-notifications--last-id-owner
                                      qq-notifications--display-owner))
                          (should (equal closed '(outer-id)))
                          (should (equal cancelled '(outer-timer)))
                          (should (= 1 (length qq-notifications--timeout-owners)))
                          (let ((timeout-owner (car qq-notifications--timeout-owners)))
                            (should (eq (plist-get timeout-owner :display-owner)
                                        qq-notifications--display-owner))
                            (should (eq (plist-get timeout-owner :id) 'inner-id))
                            (should (eq (plist-get timeout-owner :timer) 'inner-timer)))))))

(ert-deftest qq-notifications-state-hook-dedupes-and-delays-only-live-creates ()
  (let ((qq-runtime--account-id
         (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
        (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create))) (qq-notifications-mode t)
        (qq-multi-account nil) (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a") (qq-runtime--accounts (make-hash-table :test #'equal))
        (qq-notifications--resetting-p nil) (qq-notifications--display-owner nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--seen-anchors (make-hash-table :test #'equal))
        (qq-notifications--seen-anchor-order nil) (qq-notifications--generation 0)
        (qq-notifications--delay-owners nil) (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil) (qq-notifications--history-buffer nil)
        (qq-notifications--last-id nil) (qq-notifications-delay 0.5) cancelled scheduled)
    (qq-runtime-with-account "account-a"
                             (cl-letf (((symbol-function 'run-with-timer)
                                        (lambda (_delay _repeat function &rest arguments)
                                          (let ((timer (intern (format "timer-%d"
                                                                       (1+ (length scheduled))))))
                                            (push (list function arguments timer) scheduled)
                                            timer)))
                                       ((symbol-function 'timerp)
                                        (lambda (timer) (memq timer '(timer-1 timer-2))))
                                       ((symbol-function 'cancel-timer)
                                        (lambda (timer) (push timer cancelled))))
                               (let ((event (list :type 'message
                                                  :mutation 'create
                                                  :source 'event
                                                  :message (qq-notifications-test--message '(at-me)))))
                                 (qq-notifications--handle-state-change event)
                                 (qq-notifications--handle-state-change event)
                                 (should (= 1 (length scheduled))))
                               (qq-notifications--handle-state-change
                                (list :type 'message :mutation 'update :source 'event
                                      :message (qq-notifications-test--message '(at-me))))
                               (should (= 1 (length scheduled)))
                               (qq-notifications--handle-state-change '(:type reset))
                               (should (= 0 (hash-table-count qq-notifications--seen-anchors)))
                               (should-not qq-notifications--seen-anchor-order)
                               (should-not qq-notifications--delay-owners)
                               (should (equal cancelled '(timer-1)))))))

(ert-deftest qq-notifications-reset-makes-delayed-secret-callback-inert ()
  (let ((qq-notifications-mode t)
        (qq-multi-account nil)
        (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a")
        (qq-notifications--resetting-p nil)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--seen-anchors (make-hash-table :test #'equal))
        (qq-notifications--seen-anchor-order nil)
        (qq-notifications--generation 7)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications--history-buffer nil)
        (qq-notifications--last-id nil)
        (message (qq-notifications-test--message '(at-me)))
        scheduled
        shown)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (setf (alist-get 'preview message) "OLD DELAY SECRET")
                        (cl-letf (((symbol-function 'run-with-timer)
                                   (lambda (_delay _repeat function &rest arguments)
                                     (setq scheduled (cons function arguments))
                                     'old-delay-timer))
                                  ((symbol-function 'timerp)
                                   (lambda (timer) (eq timer 'old-delay-timer)))
                                  ((symbol-function 'cancel-timer) #'ignore)
                                  ((symbol-function 'qq-notifications--show)
                                   (lambda (value) (push value shown))))
                          (qq-notifications--schedule-delayed message)
                          (should qq-notifications--delay-owners)
                          (qq-notifications-reset-session-state)
                          (apply (car scheduled) (cdr scheduled))
                          (should-not shown)
                          (should-not qq-notifications--delay-owners)
                          (should-not
                           (string-match-p
                            "OLD DELAY SECRET"
                            (prin1-to-string
                             (list qq-notifications--history
                                   qq-notifications--delay-owners
                                   qq-notifications--timeout-owners))))))))

(ert-deftest qq-notifications-reset-barrier-blocks-cancel-close-and-kill-reentry ()
  "Every reset hook remains inert and the final revoke removes direct leaks."
  (let* ((qq-notifications-mode t)
         (qq-notifications--resetting-p nil)
         (qq-notifications--generation 12)
         (display-owner '(:generation 12))
         (qq-notifications--display-owner display-owner)
         (qq-notifications--last-id 'old-id)
         (qq-notifications--last-id-owner display-owner)
         (qq-notifications--seen-anchors (make-hash-table :test #'equal))
         (qq-notifications--seen-anchor-order '("old-anchor"))
         (qq-notifications--history (make-ring 4))
         (history-buffer
          (generate-new-buffer qq-notifications--history-buffer-name))
         (qq-notifications--history-buffer history-buffer)
         (delay-owner '(:generation 12 :timer old-timer))
         (qq-notifications--delay-owners (list delay-owner))
         (qq-notifications--timeout-owners nil)
         (secret-message
          (qq-notifications-test--message-with-preview "OLD_ACCOUNT_SECRET"))
         (scheduled 0)
         (backend-shows 0)
         closed reentry-results)
    (unwind-protect
        (progn
          (ring-insert qq-notifications--history secret-message)
          (with-current-buffer history-buffer
            (special-mode)
            (setq-local qq-notifications--history-buffer-owner
                        qq-notifications--history-buffer-owner-token)
            (let ((inhibit-read-only t))
              (insert "OLD_ACCOUNT_SECRET"))
            (add-hook
             'kill-buffer-hook
             (lambda ()
               (push (qq-notifications--schedule-delayed secret-message)
                     reentry-results)
               (push (qq-notifications--show secret-message) reentry-results)
               (push (qq-notifications-history) reentry-results)
               ;; Even direct mutation by a hostile hook is removed by the
               ;; outermost reset's final pass.
               (push (list :generation qq-notifications--generation
                           :secret "OLD_ACCOUNT_SECRET")
                     qq-notifications--delay-owners)
               (ring-insert (qq-notifications--history-ring)
                            secret-message))
             nil t))
          (cl-letf (((symbol-function 'qq-state-session)
                     (lambda (state &rest args) (apply #'qq-notifications-test--session args)))
                    ((symbol-function 'qq-notifications--chat-observable-p)
                     (lambda (_key) nil))
                    ((symbol-function 'run-with-timer)
                     (lambda (&rest _arguments)
                       (cl-incf scheduled)
                       'unexpected-timer))
                    ((symbol-function 'timerp)
                     (lambda (timer) (eq timer 'old-timer)))
                    ((symbol-function 'cancel-timer)
                     (lambda (_timer)
                       (push (qq-notifications--schedule-delayed secret-message)
                             reentry-results)
                       (push (qq-notifications--show secret-message)
                             reentry-results)))
                    ((symbol-function 'notifications-notify)
                     (lambda (&rest _arguments)
                       (cl-incf backend-shows)
                       'unexpected-id))
                    ((symbol-function 'notifications-close-notification)
                     (lambda (id)
                       (push id closed)
                       (push (qq-notifications--schedule-delayed secret-message)
                             reentry-results)
                       (push (qq-notifications--show secret-message)
                             reentry-results))))
            (qq-notifications-reset-session-state))
          (should-not qq-notifications--resetting-p)
          (should (zerop scheduled))
          (should (zerop backend-shows))
          (should (equal closed '(old-id)))
          (should (cl-every #'null reentry-results))
          (should-not (buffer-live-p history-buffer))
          (should-not qq-notifications--display-owner)
          (should-not qq-notifications--last-id)
          (should-not qq-notifications--last-id-owner)
          (should-not qq-notifications--delay-owners)
          (should-not qq-notifications--timeout-owners)
          (should-not qq-notifications--history)
          (should-not
           (string-match-p
            "OLD_ACCOUNT_SECRET"
            (prin1-to-string
             (list qq-notifications--history
                   qq-notifications--delay-owners
                   qq-notifications--timeout-owners)))))
      (when (buffer-live-p history-buffer)
        (kill-buffer history-buffer)))))

(ert-deftest qq-notifications-history-does-not-claim-ordinary-fixed-buffer ()
  "Only explicit owner markers authorize reset cleanup by fallback name."
  (let ((qq-notifications--resetting-p nil)
        (qq-notifications--generation 14)
        (qq-notifications--history nil)
        (qq-notifications--history-buffer nil)
        ordinary
        owned)
    (unwind-protect
        (progn
          (should-not (get-buffer qq-notifications--history-buffer-name))
          (setq ordinary
                (get-buffer-create qq-notifications--history-buffer-name))
          (with-current-buffer ordinary
            (insert "ORDINARY FIXED NAME"))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _arguments)
                       (setq owned buffer))))
            (qq-notifications-history))
          (should (buffer-live-p ordinary))
          (should (buffer-live-p owned))
          (should-not (eq ordinary owned))
          (with-current-buffer ordinary
            (should (equal (buffer-string) "ORDINARY FIXED NAME"))
            (should-not qq-notifications--history-buffer-owner))
          (with-current-buffer owned
            (should (eq qq-notifications--history-buffer-owner
                        qq-notifications--history-buffer-owner-token))
            (rename-buffer "*renamed-owned-qq-notifications*" t))
          (qq-notifications-reset-session-state)
          (should (buffer-live-p ordinary))
          (should-not (buffer-live-p owned))
          (with-current-buffer ordinary
            (should (equal (buffer-string) "ORDINARY FIXED NAME"))))
      (dolist (buffer (list ordinary owned))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest qq-notifications-timeout-callback-cannot-cross-account-reset ()
  (let ((qq-notifications-mode t)
        (qq-multi-account nil)
        (qq-account--accounts (qq-notifications-test--accounts "account-a"))
        (qq-account--current-account-id "account-a")
        (qq-notifications-extra-args '(:app-icon "/tmp/test-icon.png"))
        (qq-notifications--resetting-p nil)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--seen-anchors (make-hash-table :test #'equal))
        (qq-notifications--seen-anchor-order nil)
        (qq-notifications--generation 9)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications--history-buffer nil)
        (qq-notifications--last-id nil)
        (qq-notifications-timeout 2)
        scheduled
        cancelled
        closed)
    (qq-test-with-state (qq-state-store-create :account-id "account-a")
                        (cl-letf (((symbol-function 'qq-state-session)
                                   (lambda (state _key) '((key . "group:20001") (type . group) (title . "Group") (muted-p))))
                                  ((symbol-function 'qq-notifications--chat-observable-p)
                                   (lambda (_key) nil))
                                  ((symbol-function 'notifications-notify)
                                   (lambda (&rest _arguments) 'old-notification))
                                  ((symbol-function 'notifications-close-notification)
                                   (lambda (id) (push id closed)))
                                  ((symbol-function 'run-with-timer)
                                   (lambda (_delay _repeat function &rest arguments)
                                     (setq scheduled (cons function arguments))
                                     'timeout-timer))
                                  ((symbol-function 'timerp)
                                   (lambda (timer) (eq timer 'timeout-timer)))
                                  ((symbol-function 'cancel-timer)
                                   (lambda (timer) (push timer cancelled))))
                          (qq-notifications--show (qq-notifications-test--message '(at-me)))
                          (should (eq qq-notifications--last-id 'old-notification))
                          (should (= 1 (length qq-notifications--timeout-owners)))
                          (qq-notifications-reset-session-state)
                          (setq qq-notifications--last-id 'new-notification)
                          (apply (car scheduled) (cdr scheduled))
                          (should (eq qq-notifications--last-id 'new-notification))
                          (should (equal cancelled '(timeout-timer)))
                          (should (equal closed '(old-notification)))))))

(ert-deftest qq-notifications-reset-clears-history-buffer-and-timeout-owners ()
  (let* ((qq-runtime--app nil)
         (qq-notifications--seen-anchors (make-hash-table :test #'equal))
         (qq-notifications--seen-anchor-order '("old-anchor"))
         (qq-notifications--generation 11)
         (qq-notifications--resetting-p nil)
         (qq-notifications--display-owner nil)
         (qq-notifications--last-id-owner nil)
         (qq-notifications--history (make-ring 4))
         (history-buffer
          (generate-new-buffer qq-notifications--history-buffer-name))
         (qq-notifications--history-buffer history-buffer)
         (qq-notifications--last-id 'old-notification)
         (delay-owner '(:generation 11 :timer old-delay))
         (timeout-owner
          '(:generation 11 :id old-notification :timer old-timeout))
         (qq-notifications--delay-owners (list delay-owner))
         (qq-notifications--timeout-owners (list timeout-owner))
         cancelled
         closed)
    (unwind-protect
        (progn
          (ring-insert qq-notifications--history
                       '((preview . "OLD HISTORY SECRET")))
          (with-current-buffer history-buffer
            (special-mode)
            (setq-local qq-notifications--history-buffer-owner
                        qq-notifications--history-buffer-owner-token)
            (let ((inhibit-read-only t))
              (insert "OLD BUFFER SECRET")))
          (cl-letf (((symbol-function 'timerp)
                     (lambda (timer) (memq timer '(old-delay old-timeout))))
                    ((symbol-function 'cancel-timer)
                     (lambda (timer) (push timer cancelled)))
                    ((symbol-function 'notifications-close-notification)
                     (lambda (id) (push id closed))))
            (qq-notifications-reset-session-state)
            ;; Even if the event loop later invokes an already-queued timeout,
            ;; it cannot close a new account's notification.
            (setq qq-notifications--last-id 'new-notification)
            (qq-notifications--timeout-fired timeout-owner))
          (should-not (buffer-live-p history-buffer))
          (should-not qq-notifications--history)
          (should-not qq-notifications--delay-owners)
          (should-not qq-notifications--timeout-owners)
          (should (eq qq-notifications--last-id 'new-notification))
          (should (equal (sort (mapcar #'symbol-name cancelled)
                               #'string-lessp)
                         '("old-delay" "old-timeout")))
          (should (equal closed '(old-notification))))
      (when (buffer-live-p history-buffer)
        (kill-buffer history-buffer)))))

(ert-deftest qq-account-reset-invokes-notification-and-media-privacy-boundaries ()
  "The public account reset must include non-view global account state."
  (let* ((qq-runtime--app nil)
         (qq-notifications--seen-anchors (make-hash-table :test #'equal))
         (qq-notifications--seen-anchor-order nil)
         (qq-notifications--generation 20)
         (qq-notifications--resetting-p nil)
         (qq-notifications--display-owner nil)
         (qq-notifications--last-id-owner nil)
         (qq-notifications--history (make-ring 2))
         (history-buffer
          (generate-new-buffer qq-notifications--history-buffer-name))
         (qq-notifications--history-buffer history-buffer)
         (qq-notifications--delay-owners nil)
         (qq-notifications--timeout-owners nil)
         (qq-notifications--last-id nil)
         (media-clears 0)
         (state-resets 0))
    (unwind-protect
        (progn
          (ring-insert qq-notifications--history
                       '((preview . "ACCOUNT RESET HISTORY SECRET")))
          (with-current-buffer history-buffer
            (setq-local qq-notifications--history-buffer-owner
                        qq-notifications--history-buffer-owner-token)
            (insert "ACCOUNT RESET BUFFER SECRET"))
          (cl-letf (((symbol-function 'qq--collect-client-buffers)
                     (lambda () nil))
                    ((symbol-function 'qq--kill-client-buffers) #'ignore)
                    ((symbol-function 'qq-runtime-reset-state) (lambda () (cl-incf state-resets)))
                    ((symbol-function 'qq-media-clear-cache)
                     (lambda () (cl-incf media-clears))))
            (qq-reset-session-state))
          (should (= state-resets 1))
          (should (= media-clears 1))
          (should-not qq-notifications--history)
          (should-not (buffer-live-p history-buffer)))
      (when (buffer-live-p history-buffer)
        (kill-buffer history-buffer)))))

(ert-deftest
    qq-account-reset-drains-reentrant-runtime-and-preserves-foreign-view
    ()
  "Kill-hook replacement apps are drained without claiming a foreign QQ view."
  (let*
      ((current-app
        (appkit-app-start
         (appkit-app-type-create
          :name 'qq
          :init
          (appkit-app-type-init
           qq-runtime--gateway-type)
          :update
          (appkit-app-type-update
           qq-runtime--gateway-type))
         :identity 'reset-current))
       (foreign-app
        (appkit-app-start
         (appkit-app-type-create
          :name 'qq
          :init
          (appkit-app-type-init
           qq-runtime--gateway-type)
          :update
          (appkit-app-type-update
           qq-runtime--gateway-type))
         :identity 'reset-foreign))
       (qq-runtime--app current-app)
       (qq-runtime--accounts (make-hash-table :test #'equal))
       (qq-notifications--resetting-p nil)
       (qq-notifications--generation 40)
       (qq-notifications--display-owner nil)
       (qq-notifications--last-id nil)
       (qq-notifications--last-id-owner nil)
       (qq-notifications--delay-owners nil)
       (qq-notifications--timeout-owners nil)
       (qq-notifications--history nil)
       (qq-notifications--history-buffer nil)
       (current-buffer (generate-new-buffer " *qq-reset-current*"))
       (foreign-buffer (generate-new-buffer " *qq-reset-foreign*"))
       (legacy-buffer (generate-new-buffer " *qq-reset-legacy*"))
       replacement-app replacement-buffer reentered (state-resets 0)
       (media-clears 0))
    (unwind-protect
        (progn
          (with-current-buffer current-buffer
            (qq-root-mode)
            (let ((inhibit-read-only t))
              (insert "OLD_ACCOUNT_SECRET current"))
            (qq-runtime-open-surface
             :app current-app
             :id 'reset-current-root
             :mode 'qq-root-mode
             :buffer
             (current-buffer)
             :render-function #'ignore)
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (unless reentered
                          (setq reentered t replacement-app
                                (appkit-app-start
                                 (appkit-app-type-create
                                  :name 'qq
                                  :init
                                  (appkit-app-type-init
                                   qq-runtime--gateway-type)
                                  :update
                                  (appkit-app-type-update
                                   qq-runtime--gateway-type))
                                 :identity 'reset-replacement)
                                qq-runtime--app replacement-app
                                replacement-buffer
                                (generate-new-buffer
                                 " *qq-reset-replacement*"))
                          (with-current-buffer replacement-buffer
                            (qq-root-mode)
                            (let ((inhibit-read-only t))
                              (insert "OLD_ACCOUNT_SECRET replacement"))
                            (qq-runtime-open-surface
                             :app replacement-app
                             :id 'reset-replacement-root
                             :mode 'qq-root-mode
                             :buffer
                             (current-buffer)
                             :render-function #'ignore))))
                      nil t))
          (with-current-buffer foreign-buffer
            (qq-root-mode)
            (let ((inhibit-read-only t)) (insert "FOREIGN APP DATA"))
            (qq-runtime-open-surface
             :app foreign-app
             :id 'reset-foreign-root
             :mode 'qq-root-mode
             :buffer
             (current-buffer)
             :render-function #'ignore))
          (with-current-buffer legacy-buffer
            (qq-user-mode)
            (let ((inhibit-read-only t))
              (insert "OLD_ACCOUNT_SECRET legacy")))
          (cl-letf
              (((symbol-function 'buffer-list)
                (lambda (&optional _frame)
                  (seq-filter #'buffer-live-p
                              (list current-buffer foreign-buffer legacy-buffer
                                    replacement-buffer))))
               ((symbol-function 'qq-login-cancel) #'ignore)
               ((symbol-function 'qq-core-reset-session-state) #'ignore)
               ((symbol-function 'qq-runtime-reset-state) (lambda () (cl-incf state-resets)))
               ((symbol-function 'qq-media-clear-cache)
                (lambda () (cl-incf media-clears))))
            (qq-reset-session-state))
          (should reentered) (should (= state-resets 1))
          (should (= media-clears 1)) (should-not qq-runtime--app)
          (should-not (buffer-live-p current-buffer))
          (should-not (buffer-live-p legacy-buffer))
          (should-not (buffer-live-p replacement-buffer))
          (should-not (appkit-app-live-p current-app))
          (should-not (appkit-app-live-p replacement-app))
          (should (appkit-app-live-p foreign-app))
          (should (buffer-live-p foreign-buffer))
          (with-current-buffer foreign-buffer
            (should (equal (buffer-string) "FOREIGN APP DATA"))
            (should (appkit-surface-live-p (appkit-current-surface))))
          (dolist (buffer (list current-buffer legacy-buffer replacement-buffer))
            (when
                (and (buffer-live-p buffer)
                     (not (eq buffer foreign-buffer)))
              (with-current-buffer buffer
                (should-not
                 (string-match-p "OLD_ACCOUNT_SECRET"
                                 (buffer-substring-no-properties
                                  (point-min) (point-max))))))))
      (dolist (app (list current-app replacement-app foreign-app))
        (when (appkit-app-live-p app) (appkit-app-close app)))
      (dolist
          (buffer
           (list current-buffer foreign-buffer legacy-buffer
                 replacement-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (setq qq-runtime--app nil))))

(defmacro qq-notifications-test--with-avatar-files (&rest body)
  "Run BODY with real avatar sources and transient notification composites."
  (declare (indent 0) (debug t))
  `(let ((main (make-temp-file "qq-notification-main-" nil ".svg"))
         (badge (make-temp-file "qq-notification-badge-" nil ".svg"))
         badge-ready-p)
     (unwind-protect
         (progn
           (dolist (file (list main badge))
             (with-temp-file file
               (insert "<svg xmlns=\"http://www.w3.org/2000/svg\""
                       " width=\"8\" height=\"8\"><rect width=\"8\""
                       " height=\"8\" fill=\"red\"/></svg>")))
           (cl-letf (((symbol-function 'qq-notifications--avatar-file)
                      (lambda (key)
                        (cond
                         ((equal key "group-avatar:20001") main)
                         ((and badge-ready-p (equal key "avatar:10001")) badge))))
                     ((symbol-function 'appkit-media-image-mime-type)
                      (lambda (_file) "image/svg+xml")))
             ,@body))
       (ignore-errors (delete-file main))
       (ignore-errors (delete-file badge)))))

(ert-deftest qq-notifications-composites-follow-exact-owner-lifetime ()
  (qq-notifications-test--with-display
   (qq-notifications-test--with-avatar-files
    (let ((consumers (make-hash-table :test #'eq))
          (chat-owner (list 'independent-chat))
          lookups released requests)
      (puthash chat-owner t consumers)
      (cl-letf (((symbol-function 'qq-media-group-avatar-image)
                 (lambda (_id)
                   (push (list qq-media--consumer
                               (qq-runtime-current-account-id))
                         lookups)
                   (puthash qq-media--consumer t consumers)))
                ((symbol-function 'qq-media-message-avatar-image)
                 (lambda (_message)
                   (push (list qq-media--consumer
                               (qq-runtime-current-account-id))
                         lookups)
                   (puthash qq-media--consumer t consumers)))
                ((symbol-function 'qq-media-release-consumer)
                 (lambda (owner)
                   (push owner released)
                   (remhash owner consumers)))
                ((symbol-function 'notifications-notify)
                 (lambda (&rest args)
                   (push args requests)
                   (or (plist-get args :replaces-id) (make-symbol "notice"))))
                ((symbol-function 'notifications-close-notification) #'ignore))
        (qq-notifications--show (qq-notifications-test--message))
        (let* ((owner qq-notifications--last-id-owner)
               (first (plist-get owner :icon-file)))
          (should (file-regular-p first))
          (should (string-prefix-p "qq-notification-avatar-"
                                   (file-name-nondirectory first)))
          ;; Both already-ready avatar lookups still acquire this consumer.
          (should (= 2 (length lookups)))
          (dolist (lookup lookups)
            (should (eq (car lookup) owner))
            (should (equal (cadr lookup) "account-a")))
          (setq badge-ready-p t)
          ;; Refresh must use the captured account, not the hook caller.
          (let ((qq-multi-account t)
                (avatar-icon (symbol-function 'qq-notifications--avatar-icon)))
            (qq-test-with-state (qq-state-store-create :account-id "account-b")
                                (cl-letf (((symbol-function 'qq-notifications--avatar-icon)
                                           (lambda (token keys)
                                             (should (eq qq-media--consumer owner))
                                             (should (equal (qq-runtime-current-account-id)
                                                            "account-a"))
                                             (funcall avatar-icon token keys))))
                                  (qq-notifications--refresh-avatar "avatar:10001"))))
          (let ((second (plist-get owner :icon-file)))
            (should (file-regular-p second))
            (should-not (equal first second))
            (should-not (file-exists-p first))
            (qq-notifications--refresh-avatar "avatar:10001")
            (should (= 2 (length requests)))
            (qq-notifications--show (qq-notifications-test--message))
            (should-not (file-exists-p second))
            (should (memq owner released))
            (should-not (gethash owner consumers))
            (should (gethash chat-owner consumers))
            (let* ((replacement qq-notifications--last-id-owner)
                   (third (plist-get replacement :icon-file)))
              (should (file-regular-p third))
              (should (gethash replacement consumers))
              (qq-notifications-reset-session-state)
              (should-not (file-exists-p third))
              (should-not (gethash replacement consumers))
              (should (gethash chat-owner consumers))
              (qq-notifications--forget-avatar-owner replacement)
              (should (= 1 (cl-count replacement released :test #'eq)))))
          (should (file-exists-p main))
          (should (file-exists-p badge))))))))

(ert-deftest qq-notifications-failed-avatar-replacement-deletes-only-new-temp ()
  (qq-notifications-test--with-display
   (qq-notifications-test--with-avatar-files
    (let (failed-icon)
      (cl-letf (((symbol-function 'notifications-notify)
                 (lambda (&rest args)
                   (when (plist-get args :replaces-id)
                     (setq failed-icon (plist-get args :app-icon))
                     (error "Backend replacement failed"))
                   'notice))
                ((symbol-function 'notifications-close-notification) #'ignore))
        (qq-notifications--show (qq-notifications-test--message))
        (let* ((owner qq-notifications--last-id-owner)
               (original (plist-get owner :icon-file)))
          (setq badge-ready-p t)
          (qq-notifications--refresh-avatar "avatar:10001")
          (should failed-icon)
          (should-not (file-exists-p failed-icon))
          (should (equal original (plist-get owner :icon-file)))
          (should (file-exists-p original))
          (qq-notifications--close 'notice owner)
          (should-not (file-exists-p original))
          (should (file-exists-p main))))))))

(ert-deftest qq-notifications-reset-during-composite-cannot-publish-or-leak ()
  (qq-notifications-test--with-display
   (qq-notifications-test--with-avatar-files
    (let ((print-svg (symbol-function 'svg-print))
          (create-temp (symbol-function 'make-temp-file))
          generated requests)
      (cl-letf (((symbol-function 'svg-print)
                 (lambda (&rest args)
                   (apply print-svg args)
                   (qq-notifications-reset-session-state)))
                ((symbol-function 'make-temp-file)
                 (lambda (&rest args)
                   (let ((file (apply create-temp args)))
                     (push file generated)
                     file)))
                ((symbol-function 'notifications-notify)
                 (lambda (&rest args) (push args requests) 'notice)))
        (qq-notifications--show (qq-notifications-test--message))
        (should generated)
        (dolist (file generated)
          (should-not (file-exists-p file)))
        (should-not requests)
        (should-not qq-notifications--last-id)
        (should (file-exists-p main)))))))

(ert-deftest qq-notifications-close-preserves-explicit-user-icon ()
  (qq-notifications-test--with-display
   (let* ((file (make-temp-file "qq-user-notification-icon-" nil ".png"))
          (qq-notifications-extra-args (list :app-icon file)))
     (unwind-protect
         (cl-letf (((symbol-function 'notifications-notify)
                    (lambda (&rest _args) 'notice))
                   ((symbol-function 'notifications-close-notification) #'ignore))
           (qq-notifications--show (qq-notifications-test--message))
           (qq-notifications--close 'notice qq-notifications--last-id-owner)
           (should (file-exists-p file)))
       (delete-file file)))))

(ert-deftest qq-notifications-avatar-update-keeps-history-and-original-timeout ()
  (qq-notifications-test--with-display
   (let (icon requests closed)
     (cl-letf (((symbol-function 'qq-notifications--avatar-icon)
                (lambda (_owner _keys) icon))
               ((symbol-function 'notifications-notify)
                (lambda (&rest args)
                  (push args requests)
                  (or (plist-get args :replaces-id) 'notification)))
               ((symbol-function 'notifications-close-notification)
                (lambda (id) (push id closed))))
       (qq-notifications--show (qq-notifications-test--message))
       (let ((timeout (car qq-notifications--timeout-owners)))
         (setq icon "/tmp/ready-group-and-sender.svg")
         (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
         (should (eq qq-notifications--last-id 'notification))
         (should (eq (plist-get (car requests) :replaces-id) 'notification))
         (should (= 2 (length requests)))
         (should (= 1 (ring-length qq-notifications--history)))
         (should (eq timeout (car qq-notifications--timeout-owners)))
         (should (= 1 (length qq-notifications--timeout-owners)))
         (should-not closed)
         (cancel-timer (plist-get timeout :timer))
         (qq-notifications--timeout-fired timeout)
         (setq icon "/tmp/late-avatar.svg")
         (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
         (should-not qq-notifications--last-id)
         (should (equal closed '(notification)))
         (should (= 2 (length requests))))))))

(ert-deftest qq-notifications-dismissed-notification-is-not-revived-by-avatar ()
  (qq-notifications-test--with-display
   (let* ((custom-closes nil)
          (qq-notifications-extra-args
           (list :on-close
                 (lambda (id reason) (push (list id reason) custom-closes))))
          icon requests closed)
     (cl-letf (((symbol-function 'qq-notifications--avatar-icon)
                (lambda (_owner _keys) icon))
               ((symbol-function 'notifications-notify)
                (lambda (&rest args)
                  (push args requests)
                  (or (plist-get args :replaces-id) 'notification)))
               ((symbol-function 'notifications-close-notification)
                (lambda (id) (push id closed))))
       (qq-notifications--show (qq-notifications-test--message))
       (let ((on-close (plist-get (car requests) :on-close))
             (timeout (car qq-notifications--timeout-owners)))
         (funcall on-close 'notification 'dismissed)
         (setq icon "/tmp/late-sender-avatar.svg")
         (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
         (qq-notifications--timeout-fired timeout)
         (should-not qq-notifications--last-id)
         (should-not qq-notifications--timeout-owners)
         (should-not closed)
         (should (= 1 (length requests)))
         (should (= 1 (ring-length qq-notifications--history)))
         (should (equal custom-closes '((notification dismissed)))))))))

(ert-deftest qq-notifications-avatar-update-cannot-cross-reset-with-reused-id ()
  (qq-notifications-test--with-display
   (let ((old (qq-notifications-test--message-with-preview "OLD ACCOUNT"))
         (new (qq-notifications-test--message-with-preview "NEW ACCOUNT"))
         icon reentered requests closed)
     (cl-letf (((symbol-function 'qq-notifications--avatar-icon)
                (lambda (_owner _keys) icon))
               ((symbol-function 'notifications-notify)
                (lambda (&rest args)
                  (push args requests)
                  (when (and (plist-get args :replaces-id) (not reentered))
                    (setq reentered t icon nil)
                    (qq-notifications-reset-session-state)
                    (qq-notifications--show new))
                  'reused-id))
               ((symbol-function 'notifications-close-notification)
                (lambda (id) (push id closed))))
       (qq-notifications--show old)
       (setq icon "/tmp/old-account-avatar.svg")
       (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
       (should reentered)
       (should (eq qq-notifications--last-id 'reused-id))
       (should (eq qq-notifications--display-owner
                   qq-notifications--last-id-owner))
       (should (equal closed '(reused-id)))
       (should (equal (mapcar (lambda (message) (alist-get 'preview message))
                              (ring-elements qq-notifications--history))
                      '("NEW ACCOUNT")))
       (setq icon "/tmp/new-account-avatar.svg")
       (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
       (should (= 4 (length requests)))
       (should (equal closed '(reused-id)))
       (should (= 1 (ring-length qq-notifications--history)))))))

(ert-deftest qq-notifications-nested-avatar-update-keeps-newest-image ()
  (qq-notifications-test--with-display
   (let (icon reentered requests)
     (cl-letf (((symbol-function 'qq-notifications--avatar-icon)
                (lambda (_owner _keys) icon))
               ((symbol-function 'notifications-notify)
                (lambda (&rest args)
                  (push args requests)
                  (when (and (plist-get args :replaces-id) (not reentered))
                    (setq reentered t icon "/tmp/newest-avatar.svg")
                    (run-hook-with-args 'qq-media-cache-update-hook
                                        "group-avatar:20001"))
                  'notification)))
       (qq-notifications--show (qq-notifications-test--message))
       (let ((timeout (car qq-notifications--timeout-owners)))
         (setq icon "/tmp/first-avatar.svg")
         (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
         (should reentered)
         (should (= 3 (length requests)))
         (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
         (should (= 3 (length requests)))
         (should (eq qq-notifications--last-id 'notification))
         (should (= 1 (ring-length qq-notifications--history)))
         (should (eq timeout (car qq-notifications--timeout-owners))))))))

(ert-deftest qq-notifications-custom-image-is-not-overridden-by-avatar-updates ()
  (qq-notifications-test--with-display
   (let ((qq-notifications-extra-args '(:app-icon "/tmp/custom-icon.png"))
         (avatar-lookups 0)
         requests)
     (cl-letf (((symbol-function 'qq-notifications--avatar-icon)
                (lambda (_owner _keys)
                  (cl-incf avatar-lookups)
                  "/tmp/automatic-avatar.svg"))
               ((symbol-function 'notifications-notify)
                (lambda (&rest args)
                  (push args requests)
                  'notification)))
       (qq-notifications--show (qq-notifications-test--message))
       (run-hook-with-args 'qq-media-cache-update-hook "avatar:10001")
       (should (= 1 (length requests)))
       (should (equal (plist-get (car requests) :app-icon)
                      "/tmp/custom-icon.png"))
       (should (= 0 avatar-lookups))
       (should (= 1 (ring-length qq-notifications--history)))))))

(ert-deftest qq-notifications-body-marks-direct-mention ()
  (let ((qq-notifications-body-limit 80)
        (qq-notifications-show-preview t))
    (should (equal "@你  hello"
                   (qq-notifications--body
                    (qq-notifications-test--message '(at-me)))))))

(provide 'qq-notifications-test)

;;; qq-notifications-test.el ends here
