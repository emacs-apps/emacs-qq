;;; qq-notifications-test.el --- Tests for qq notifications -*- lexical-binding: t; -*-

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
  '((key . "group:20001") (type . group)
    (title . "Group") (muted-p . nil)))

(ert-deftest qq-notifications-muted-chat-requires-native-priority-mention ()
  (cl-letf (((symbol-function 'qq-state-session)
             (lambda (_key)
               '((key . "group:20001") (type . group)
                 (title . "Group") (muted-p . t))))
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
                   (qq-notifications-test--message '(at-all)))))))

(ert-deftest qq-notifications-suppresses-selected-focused-chat ()
  (cl-letf (((symbol-function 'qq-state-session)
             (lambda (_key)
               '((key . "group:20001") (type . group)
                 (title . "Group") (muted-p . nil))))
            ((symbol-function 'qq-notifications--chat-observable-p)
             (lambda (_key) t)))
    (should-not (qq-notifications-message-notify-p
                 (qq-notifications-test--message '(at-me))))))

(ert-deftest qq-notifications-nested-backend-display-wins-exact-owner ()
  "A same-generation nested display makes the outer returned id an orphan."
  (let ((qq-notifications-mode t)
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
    (cl-letf (((symbol-function 'qq-state-session)
               #'qq-notifications-test--session)
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
                     '("INNER MESSAGE"))))))

(ert-deftest qq-notifications-nested-backend-reused-id-keeps-exact-owner ()
  "A stale outer unwind must not close the nested display's reused backend id."
  (let ((qq-notifications-mode t)
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
    (cl-letf (((symbol-function 'qq-state-session)
               #'qq-notifications-test--session)
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
                     '("INNER REUSED"))))))

(ert-deftest qq-notifications-reset-backend-reused-id-keeps-new-generation ()
  "A stale pre-reset unwind must not close a new account's reused backend id."
  (let ((qq-notifications-mode t)
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
    (cl-letf (((symbol-function 'qq-state-session)
               #'qq-notifications-test--session)
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
                       (prin1-to-string qq-notifications--history))))))

(ert-deftest qq-notifications-reset-during-backend-closes-returned-stale-id ()
  (let ((qq-notifications-mode t)
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
    (cl-letf (((symbol-function 'qq-state-session)
               #'qq-notifications-test--session)
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
      (should-not qq-notifications--timeout-owners))))

(ert-deftest qq-notifications-nested-display-during-close-aborts-outer ()
  "Backend close reentry must not let the outer display replace the nested id."
  (let* ((qq-notifications-mode t)
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
    (cl-letf (((symbol-function 'qq-state-session)
               #'qq-notifications-test--session)
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
                     '("INNER CLOSE"))))))

(ert-deftest qq-notifications-history-reentry-rolls-back-stale-outer-record ()
  "History insertion reentry retains only the exact nested display record."
  (let ((qq-notifications-mode t)
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
    (cl-letf (((symbol-function 'qq-state-session)
               #'qq-notifications-test--session)
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
                     '("INNER HISTORY"))))))

(ert-deftest qq-notifications-timeout-scheduling-reentry-keeps-nested-owner ()
  "Timer creation reentry cannot restore the already-retired outer display."
  (let ((qq-notifications-mode t)
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
    (cl-letf (((symbol-function 'qq-state-session)
               #'qq-notifications-test--session)
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
        (should (eq (plist-get timeout-owner :timer) 'inner-timer))))))

(ert-deftest qq-notifications-state-hook-dedupes-and-delays-only-live-creates ()
  (let ((qq-notifications-mode t)
        (qq-notifications--resetting-p nil)
        (qq-notifications--display-owner nil)
        (qq-notifications--last-id-owner nil)
        (qq-notifications--seen-anchors (make-hash-table :test #'equal))
        (qq-notifications--seen-anchor-order nil)
        (qq-notifications--generation 0)
        (qq-notifications--delay-owners nil)
        (qq-notifications--timeout-owners nil)
        (qq-notifications--history nil)
        (qq-notifications--history-buffer nil)
        (qq-notifications--last-id nil)
        (qq-notifications-delay 0.5)
        cancelled
        scheduled)
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
      (should (equal cancelled '(timer-1))))))

(ert-deftest qq-notifications-reset-makes-delayed-secret-callback-inert ()
  (let ((qq-notifications-mode t)
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
               qq-notifications--timeout-owners)))))))

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
                     #'qq-notifications-test--session)
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
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_key)
                 '((key . "group:20001") (type . group)
                   (title . "Group") (muted-p . nil))))
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
      (should (equal closed '(old-notification))))))

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
                    ((symbol-function 'qq-state-reset)
                     (lambda () (cl-incf state-resets)))
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
         (appkit-app-type-create :name 'qq :init
                                 (appkit-app-type-init
                                  qq-runtime--gateway-type)
                                 :update
                                 (appkit-app-type-update
                                  qq-runtime--gateway-type))
         :identity 'reset-current))
       (foreign-app
        (appkit-app-start
         (appkit-app-type-create :name 'qq :init
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
            (qq-runtime-open-surface :app current-app :id
                                     'reset-current-root :mode
                                     'qq-root-mode :buffer
                                     (current-buffer) :render-function
                                     #'ignore)
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (unless reentered
                          (setq reentered t replacement-app
                                (appkit-app-start
                                 (appkit-app-type-create :name 'qq
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
                            (qq-runtime-open-surface :app
                                                     replacement-app
                                                     :id
                                                     'reset-replacement-root
                                                     :mode
                                                     'qq-root-mode
                                                     :buffer
                                                     (current-buffer)
                                                     :render-function
                                                     #'ignore))))
                      nil t))
          (with-current-buffer foreign-buffer
            (qq-root-mode)
            (let ((inhibit-read-only t)) (insert "FOREIGN APP DATA"))
            (qq-runtime-open-surface :app foreign-app :id
                                     'reset-foreign-root :mode
                                     'qq-root-mode :buffer
                                     (current-buffer) :render-function
                                     #'ignore))
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
               ((symbol-function 'qq-state-reset)
                (lambda () (cl-incf state-resets)))
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

(ert-deftest qq-notifications-body-marks-direct-mention ()
  (let ((qq-notifications-body-limit 80)
        (qq-notifications-show-preview t))
    (should (equal "@你  hello"
                   (qq-notifications--body
                    (qq-notifications-test--message '(at-me)))))))

(provide 'qq-notifications-test)

;;; qq-notifications-test.el ends here
