;;; qq-chat-test.el --- Tests for qq-chat -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-chat)
(require 'qq-forward)
(require 'qq-state)
(require 'qq-transient)
(require 'qq)
(require 'qq-operation-test)

(defun qq-chat-test--model-put (&rest properties)
  "Set PROPERTIES on the current chat Surface model.
A chat with an account attaches its real view; a bare test buffer gets a
stand-in Surface that only carries the model."
  (let ((surface
         (or (appkit-current-surface)
             (and qq-chat--session-key (qq-runtime-current-account-id)
                  (qq-chat--ensure-view))
             (setq-local appkit--current-surface
                         (appkit-surface--create
                          :buffer (current-buffer) :alive-p t
                          :loop (appkit-loop--create :model nil))))))
    (setf (appkit-loop--model (appkit-surface-loop surface))
          (apply #'qq-chat--with (appkit-surface-model surface) properties))))

(defmacro qq-chat-test-with-pending-gateway (&rest body)
  "Run BODY with Gateway timeline calls that never answer."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'qq-runtime--rpc-fetch) (lambda (&rest _) nil)))
     ,@body))

(defun qq-chat-test--deliver-timeline (rows &optional center-anchor)
  "Answer the current chat's pending open with ROWS and CENTER-ANCHOR."
  (appkit-surface-send
   (appkit-current-surface)
   (list 'qq-timeline-loaded (plist-get (qq-chat--model) :opening) "opened"
         rows nil nil center-anchor))
  (qq-chat-test--drain-history))

(defun qq-chat-test--window (first last)
  "Open the current chat's timeline over its stored messages FIRST..LAST.
A nil LAST follows latest."
  (let* ((messages (qq-state-session-messages qq-chat--session-key))
         (anchors (mapcar #'qq-state-message-anchor messages))
         (start (or (cl-position first anchors :test #'equal) 0))
         (end (if last
                  (cl-position last anchors :test #'equal)
                (1- (length anchors)))))
    (qq-chat-test--model-put
     :timeline "test"
     :rows (mapcar (lambda (message)
                     (cons (or (alist-get 'canonical-row-key message)
                               (qq-state-message-anchor message))
                           (qq-state-message-anchor message)))
                   (and end (seq-subseq messages start (1+ end))))
     :has-older (> start 0)
     :has-newer (and last t))))

(defmacro qq-chat-test-with-reset (&rest body)
  "Run BODY with clean qq state and disabled live-update hooks."
  `(let ((qq-state-change-hook nil)
         (qq-media-cache-update-hook nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
         (qq-chat-mode-hook
          (cons (lambda ()
                  (qq-runtime-bind-account "slot-a"))
                qq-chat-mode-hook)))
     (qq-state-reset)
     (unwind-protect
         (qq-runtime-with-account "slot-a"
                                  (qq-state-reset)
                                  ,@body)
       (qq-runtime-stop-account "slot-a" t)
       (qq-state-reset))))

(defun qq-chat-test--drain-history ()
  "Deliver the accepted history Effect, App commit, and Surface projection."
  (let ((app (qq-runtime-app)) (turns 0) pending)
    (while
        (progn
          (setq pending nil)
          (dolist (loop
                   (cons (appkit-app-loop app)
                         (mapcar (lambda (entry)
                                   (appkit-surface-loop (cdr entry)))
                                 (hash-table-values (appkit-app-surfaces app)))))
            (when (> (appkit-loop-pending-count loop) 0)
              (setq pending t)
              (appkit-loop-run-pass loop)))
          (and pending (< (cl-incf turns) 12))))
    (should (< turns 12))))

(defmacro qq-chat-test-with-history-metadata (&rest body)
  "Run window-only BODY with history metadata supplied by the fixture.
These tests populate canonical messages explicitly.  Runtime tests cover the
real raw-result commit boundary."
  (declare (indent 0) (debug t))
  `(qq-chat-test-with-reset
     (cl-letf (((symbol-function 'qq-message--commit-history-result)
                (lambda (metadata _owner) metadata)))
       ,@body)))

(ert-deftest qq-chat-private-avatar-failure-settles-without-redraw-retry ()
  (dolist (failure '(resolver download))
    (qq-chat-test-with-reset
      (let ((qq-account--accounts (make-hash-table :test #'equal))
            (qq-media--image-cache (make-hash-table :test #'equal))
            (qq-media--resource-cache (make-hash-table :test #'equal))
            (qq-media--fetching-cache (make-hash-table :test #'equal))
            (qq-media--preview-missing-cache (make-hash-table :test #'equal))
            (qq-media--consumers (make-hash-table :test #'eq))
            (qq-media-cache-update-hook '(qq-chat--rerender-open-chats))
            (qq-chat-show-prompt-avatar t)
            (fetch (symbol-function 'qq-media--fetch-native-user-avatar))
            (attempts 0) (downloads 0)
            notifications rejections buffer)
        (cl-labels
            ((drain ()
               (let ((loop (appkit-surface-loop (appkit-current-surface)))
                     (turns 0))
                 (while (and (< turns 12)
                             (or notifications rejections
                                 (> (appkit-loop-pending-count loop) 0)))
                   (cl-incf turns)
                   (when rejections (funcall (pop rejections) nil 'unavailable))
                   (when notifications
                     (run-hook-with-args 'qq-media-cache-update-hook
                                         (pop notifications)))
                   (when (> (appkit-loop-pending-count loop) 0)
                     (appkit-loop-run-pass loop)))
                 (should-not notifications)
                 (should-not rejections)
                 (should (= 0 (appkit-loop-pending-count loop))))))
          (unwind-protect
              (cl-letf
                  (((symbol-function 'qq-server-capabilities) #'ignore)
                   ((symbol-function 'qq-server-ready-p) (lambda () t))
                   ((symbol-function 'qq-media--fetch-native-user-avatar)
                    (lambda (&rest args)
                      (cl-incf attempts)
                      (apply fetch args)))
                   ((symbol-function 'qq-resource-acquire-url)
                    (lambda (_url _name _success reject)
                      (cl-incf downloads)
                      (push reject rejections)
                      :transfer))
                   ((symbol-function 'qq-media--note-cache-updated)
                    (lambda (key) (push key notifications))))
                (puthash "slot-a" '((account_id . "slot-a")) qq-account--accounts)
                (when (eq failure 'download)
                  (puthash "10001"
                           '((user_id . "10001")
                             (avatar_url . "https://example.invalid/avatar.png"))
                           qq-state--friends-by-id))
                (qq-state-upsert-session
                 "private:10001"
                 '((type . private) (target-id . "10001") (title . "Alice")) nil)
                (setq buffer (qq-chat--open-buffer "private:10001"))
                (with-current-buffer buffer
                  (qq-chat--set-draft "Draft survives avatar failure")
                  (drain)
                  (dotimes (_ 3) (qq-chat-render) (drain))
                  (should (= attempts 1))
                  (should (= downloads (if (eq failure 'download) 1 0)))
                  (should (appkit-surface-live-p (appkit-current-surface)))
                  (should (appkit-chatbuf-prompt-button-live-p))
                  (should (equal "Draft survives avatar failure"
                                 (qq-chat--current-draft-string)))))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun qq-chat-test-native-request (token &optional lifecycle-owner)
  "Return an active native TOKEN request owned by optional LIFECYCLE-OWNER."
  (let ((request (qq-request-create nil nil lifecycle-owner)))
    (setf (qq-request-token request) token)
    request))

(defun qq-chat-test--history-meta (session-key &rest properties)
  "Return unified history metadata for SESSION-KEY and PROPERTIES."
  (append properties
          (list :history-port-version qq-core-history-port-version
                :history-account-id "slot-a"
                :history-session-key session-key
                :history-older-cursor nil
                :history-newer-cursor nil
                :history-has-older-p nil
                :history-has-newer-materialized-p nil)))

(defun qq-chat-test--history-cursor (session-key direction position)
  "Return one opaque direction-scoped history cursor fixture."
  `((version . ,qq-core-history-port-version)
    (account_id . "slot-a")
    (conversation
     . ,(qq-message--history-conversation-params session-key))
    (direction . ,(symbol-name direction))
    (position . ,(copy-tree position))))

(defun qq-chat-test--canonical-message (id time text &optional order)
  "Return one canonical group message for timeline projection tests."
  `((id . ,id)
    (server-id . ,id)
    (session-key . "group:20001")
    (time . ,time)
    (message-seq . ,id)
    (sender-id . "10001")
    (sender-name . "Alice")
    (self-p . nil)
    (status . received)
    (segments . (((type . "text") (data . ((text . ,text))))))
    (raw-message . ,text)
    (preview . ,text)
    (message-type . "group")
    (group-id . "20001")
    (order . ,(or order time))))

(defun qq-chat-test--gateway-message
    (session-key id sequence time &optional text)
  "Return a normalized native Gateway message with exact ID and SEQUENCE."
  `((id . ,id)
    (server-id . ,id)
    (session-key . ,session-key)
    (time . ,time)
    (message-seq . ,sequence)
    (sender-id . "10001")
    (sender-name . "Alice")
    (self-p . nil)
    (status . received)
    (segments . (((type . "text")
                  (data . ((text . ,(or text "native")))))))
    (raw-message . ,(or text "native"))
    (preview . ,(or text "native"))
    (message-type . "group")
    (order . ,time)))

(defun qq-chat-test--apply-gray-tip-message
    (session-key message-id time data)
  "Merge one normalized GrayTip DATA row into SESSION-KEY."
  (cl-multiple-value-bind (message _mutation _previous-anchor)
      (qq-state--merge-normalized-message
       session-key
       `((id . ,message-id)
         (server-id . ,message-id)
         (session-key . ,session-key)
         (time . ,time)
         (sender-name . "QQ")
         (self-p . nil)
         (status . received)
         (timeline-class . service)
         (segments . (((type . "gray-tip") (data . ,(copy-tree data)))))
         (raw-message . ,(alist-get 'text data))
         (preview . ,(alist-get 'text data))
         (message-type . "group")
         (group-id . ,(qq-state-session-key-target-id session-key))
         (order . ,time)))
    message))

(defun qq-chat-test--filter-snapshot (id sequence time text &optional reactions)
  "Return one flat rendering snapshot wire result for chat filter tests."
  `((chat . ((kind . "group") (group_id . "20001")))
    (message_id . ,id)
    (message_seq . ,sequence)
    (sent_at . ,time)
    (sender . ((user_id . "10001") (name . "Alice")))
    (outgoing . :false)
    (state . "live")
    (segments . (((kind . "text")
                  (payload . ((text . ,text))))))
    (reactions . ,(copy-tree reactions))))

(defun qq-chat-test--filter-item (id time text)
  "Return one filter-owned local item."
  (list :message-id id
        :message (qq-chat-test--canonical-message id time text)))

(defun qq-chat-test--selection (&rest anchors)
  "Return opaque selection memberships for synthetic ANCHORS."
  (mapcar
   (lambda (anchor)
     (qq-chat--make-message-selection
      anchor
      (list 'test-message-selection anchor)
      `((server-id . ,anchor))))
   anchors))

(defun qq-chat-test--forward-plan
    (buffer &optional message-id session-key)
  "Return a minimal immutable forwarding plan owned by BUFFER."
  (let ((message-id (or message-id "9007199254743009336")))
    (qq-chat--make-forward-plan
     buffer (or session-key "group:20001") (list message-id)
     (list `((id . ,message-id) (server-id . ,message-id))) nil
     (buffer-local-value 'qq-chat--forward-plan-owner buffer)
     (with-current-buffer buffer (qq-chat--ensure-view)))))

(ert-deftest qq-chat-date-break-label-matches-telega-format ()
  (let ((qq-chat-date-break-format "%d %B %Y %a")
        (system-time-locale "C"))
    (should (equal (qq-chat--message-day-label "2026-08-11")
                   "11 August 2026 Tue"))))

(ert-deftest qq-chat-date-break-projection-honors-toggle-and-day-boundary ()
  (let* ((first-time (float-time (encode-time 0 0 12 11 8 2026)))
         (second-time (float-time (encode-time 0 0 12 12 8 2026)))
         (before-midnight
          (float-time (encode-time 0 59 23 11 8 2026)))
         (after-midnight
          (float-time (encode-time 0 1 0 12 8 2026)))
         (first (qq-chat-test--canonical-message "1" first-time "first"))
         (same-day
          (qq-chat-test--canonical-message "2" (+ first-time 60) "same"))
         (next-day
          (qq-chat-test--canonical-message "3" second-time "next"))
         (before
          (qq-chat-test--canonical-message "4" before-midnight "before"))
         (after
          (qq-chat-test--canonical-message "5" after-midnight "after"))
         (undated (copy-tree first)))
    (setf (alist-get 'time undated) nil)
    (let ((qq-chat-use-date-breaks t))
      (should-not
       (plist-get (qq-chat--compute-message-render-context nil first nil)
                  :insert-date))
      (should-not
       (plist-get (qq-chat--compute-message-render-context
                   undated next-day nil)
                  :insert-date))
      (should-not
       (plist-get (qq-chat--compute-message-render-context
                   first same-day nil)
                  :insert-date))
      (should
       (equal (plist-get (qq-chat--compute-message-render-context
                          first next-day nil)
                         :insert-date)
              "2026-08-12"))
      (let ((context
             (qq-chat--compute-message-render-context before after nil)))
        (should (equal (plist-get context :insert-date) "2026-08-12"))
        (should-not (plist-get context :compact))))
    (let ((qq-chat-use-date-breaks nil))
      (should-not
       (plist-get (qq-chat--compute-message-render-context
                   first next-day nil)
                  :insert-date)))))

(ert-deftest qq-chat-date-break-row-uses-telega-style-chrome ()
  (with-temp-buffer
    (let ((fill-column 32))
      (qq-chat--insert-date-separator-row "11 August 2026 Tue"))
    (should (equal (buffer-string)
                   "──────(11 August 2026 Tue)──────\n"))
    (should (eq (get-text-property (point-min) 'face)
                'qq-msg-date-separator))))

(ert-deftest qq-chat-header-line-prunes-recalled-message-selection ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001")
       (status . recalled)
       (time . 100)
       (raw-message . "[message recalled]")))
    qq-state--messages-by-session)
   (qq-state-set-connection-status 'ready)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001"
           qq-chat--message-selection
           (qq-chat-test--selection "9007199254743009336"))
     (qq-chat--header-line-update)
     (should-not qq-chat--message-selection)
     (should-not header-line-format))))

(ert-deftest qq-chat-header-line-recognizes-ready-connection-state ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-state-set-connection-status 'ready)
     (should-not (qq-chat--header-line))
     (qq-state-set-connection-status 'reconnecting)
     (should (stringp (qq-chat--header-line))))))

(ert-deftest qq-chat-empty-header-still-renames-session-buffer ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001") (unread-badge-count . 5))
    nil)
   (qq-state-set-connection-status 'ready)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--header-line-update)
     (should-not header-line-format)
     (let ((old-name (buffer-name)))
       (qq-state-upsert-session "private:10001" '((title . "Renamed")) nil)
       (qq-chat--header-line-update)
       (should-not (equal old-name (buffer-name)))
       (should (equal (buffer-name) (qq-chat--buffer-name "private:10001")))
       (should-not header-line-format))
     (qq-operation-test--seed '(send-forward "private:10001") :pending t)
     (qq-chat--header-line-update)
     (should (stringp header-line-format))
     (qq-operation-test--seed '(send-forward "private:10001") :pending nil)
     (qq-chat--header-line-update)
     (should-not header-line-format))))

(ert-deftest qq-chat-header-count-is-current-message-count-not-badge ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001")
      (unread-message-count . 2) (unread-badge-count . 9)) nil)
   (qq-state-upsert-session
    "private:10002"
    '((title . "Bob") (target-id . "10002")
      (unread-message-count . 40) (unread-badge-count . 40)) nil)
   (qq-state-set-connection-status 'ready)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--header-line-update)
     (should (string-match-p "\\b2\\b" header-line-format))
     (should-not (string-match-p "\\b\\(?:9\\|40\\|42\\)\\b" header-line-format))
     (should-not (string-match-p "Alice" header-line-format))
     ;; No loaded first-unread identity is needed to display an exact count.
     (should-not (qq-chat--first-unread-anchor nil))
     (dolist (count '(1 0 nil))
       (qq-state-upsert-session
        "private:10001" `((unread-message-count . ,count)) nil)
       (qq-chat--header-line-update)
       (if count
           (if (zerop count)
               (should-not header-line-format)
             (should (string-match-p "\\b1\\b" header-line-format)))
         (should-not header-line-format))))))

(ert-deftest qq-chat-history-navigation-keybindings-follow-telega-contract ()
  (with-temp-buffer
    (qq-chat-mode)
    (should (eq (key-binding (kbd "M-g >") t) 'qq-chat-read-all))
    (should (eq (key-binding (kbd "M-g r") t) 'qq-chat-read-all))
    (should (eq (key-binding (kbd "M-g x") t)
                'qq-chat-goto-pop-message))
    ;; Paging is automatic near either edge.  Keep the ordinary Emacs
    ;; beginning/end commands available instead of overloading them.
    (should-not (lookup-key qq-chat-mode-map (kbd "M-<")))
    (should-not (lookup-key qq-chat-mode-map (kbd "M->")))
    (should (eq (key-binding (kbd "M-<") t) 'beginning-of-buffer))
    (should (eq (key-binding (kbd "M->") t) 'end-of-buffer))))

(ert-deftest qq-chat-attach-reader-is-require-match-and-dispatches-command ()
  (let ((qq-chat-attach-commands
         '(("image" qq-chat-attach-image)
           ("clipboard" qq-chat-attach-clipboard)))
        reader-args
        dispatched
        seen-prefix)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq reader-args args)
                 "clipboard"))
              ((symbol-function 'qq-chat-attach-clipboard)
               (lambda (&optional _as-file-p)
                 (interactive "P")
                 (setq dispatched 'qq-chat-attach-clipboard
                       seen-prefix current-prefix-arg))))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'qq-chat-attach))
      (should (equal (nth 1 reader-args) '("image" "clipboard")))
      (should (eq (nth 3 reader-args) t))
      (should (eq dispatched 'qq-chat-attach-clipboard))
      (should (equal seen-prefix '(4))))))

(ert-deftest qq-chat-explicit-media-attachers-select-the-requested-segment-type ()
  (let (calls)
    (cl-letf (((symbol-function 'appkit-media-read-file-name)
               (lambda (prompt &rest _)
                 (concat "/tmp/" (substring prompt 7 -2))))
              ((symbol-function 'qq-chat-attach-file)
               (lambda (path type)
                 (push (cons path type) calls))))
      (call-interactively #'qq-chat-attach-image)
      (call-interactively #'qq-chat-attach-video)
      (call-interactively #'qq-chat-attach-document))
    (should
     (equal (nreverse calls)
            '(("/tmp/image" . "image")
              ("/tmp/video" . "video")
              ("/tmp/file" . "file"))))))

(ert-deftest qq-chat-public-reset-cleans-forwarding-after-view-shutdown ()
  (let ((qq-runtime--app (appkit-app-start qq-runtime--gateway-type :identity 'chat-reset-test))
        (qq-runtime--accounts (make-hash-table :test #'equal))
        (qq-state--partitions (make-hash-table :test #'equal))
        (qq-state--active-account-id nil)
        (qq-state-change-hook '(qq-chat--handle-state-change))
        (qq-media-cache-update-hook nil)
        (buffer (generate-new-buffer " *qq-public-reset-chat*"))
        view plan dispatch-called)
    (unwind-protect
        (progn
          (qq-state-select-account "slot-a")
          (qq-runtime-ensure-account "slot-a")
          (qq-state-reset)
          (qq-state-upsert-session
           "group:20001"
           '((type . group) (title . "Old account")
             (target-id . "20001"))
           nil)
          (with-current-buffer buffer
            (qq-chat-mode)
            (qq-runtime-bind-account "slot-a")
            (setq qq-chat--session-key "group:20001")
            (qq-chat-test--model-put :timeline "test" :rows nil)
            (qq-chat-render)
            (let ((anchor "9007199254743009336"))
              (setq view (appkit-current-surface)
                    plan (qq-chat-test--forward-plan buffer anchor)
                    qq-chat--message-selection
                    (qq-chat-test--selection anchor))))
          (should (appkit-surface-live-p view))
          (qq-reset-session-state)
          (should-not (buffer-live-p buffer))
          (should-not (appkit-surface-live-p view))
          ;; A forwarding plan captured by the destroyed account view cannot
          ;; dispatch from any surviving buffer in the new runtime.
          (with-temp-buffer
            (cl-letf
                (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (&rest _arguments) (setq dispatch-called t))))
              (should-error
               (qq-chat-forward-merged plan "private:10002")
               :type 'user-error)))
          (should-not dispatch-called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (qq-runtime-stop-account "slot-a" t)
      (when (appkit-app-live-p qq-runtime--app)
        (appkit-app-close qq-runtime--app))
      (setq qq-runtime--app nil)
      (qq-state-reset))))

(ert-deftest qq-chat-deleted-tail-does-not-return-after-frame-refresh ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((type . private) (title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "abc")
     (delete-backward-char 1)
     (should (equal "ab" (qq-chat--current-draft-string)))
     (qq-chat--update-frame)
     (should (equal "ab" (appkit-chatbuf-input-string)))
     (should (equal "ab" (qq-chat--current-draft-string))))))

(ert-deftest qq-chat-evil-insert-return-sends-or-completes-the-draft ()
  (skip-unless (require 'evil nil t))
  (qq-evil-setup)
  (save-window-excursion
    (qq-chat-test-with-reset
      (qq-state-upsert-session
       "private:10001"
       '((type . private) (title . "Alice") (target-id . "10001")) nil)
      (with-temp-buffer
        (qq-chat-mode)
        (setq qq-chat--session-key "private:10001")
        (switch-to-buffer (current-buffer))
        (qq-chat-render)
        (evil-local-mode 1)
        (evil-insert-state)
        (qq-chat-edit-draft)
        (let (sent catalog-callback)
          (cl-letf (((symbol-function 'qq-core-send-message)
                     (lambda (session segments &rest _)
                       (push (cons session segments) sent)))
                    ((symbol-function 'qq-media-ensure-custom-faces)
                     (lambda (success &rest _)
                       (setq catalog-callback success))))
            (dolist (key '("RET" "<return>"))
              (execute-kbd-macro "Hello")
              (execute-kbd-macro (kbd key))
              (should
               (equal (pop sent)
                      '("private:10001"
                        ((type . "text") (data . ((text . "Hello")))))))
              (should (equal "" (qq-chat--current-draft-string)))
              (should (eq evil-state 'insert)))
            (execute-kbd-macro "/fav")
            (execute-kbd-macro (kbd "RET"))
            (should-not sent)
            (should catalog-callback)
            (should (equal "/fav" (qq-chat--current-draft-string)))
            (execute-kbd-macro (kbd "M-1 RET"))
            (should-not sent)
            (should (equal "/fav\n" (qq-chat--current-draft-string)))
            (execute-kbd-macro (kbd "DEL C-c C-c"))
            (should
             (equal (pop sent)
                    '("private:10001"
                      ((type . "text") (data . ((text . "/fav")))))))
            (should (equal "" (qq-chat--current-draft-string)))))))))

(ert-deftest qq-chat-evil-corfu-return-accepts-favorite-before-sending ()
  (skip-unless (display-graphic-p))
  (skip-unless (and (require 'evil nil t) (require 'corfu nil t)))
  (qq-evil-setup)
  (save-window-excursion
    (dolist (key '("RET" "<return>"))
      (qq-chat-test-with-reset
        (qq-state-upsert-session
         "private:10001"
         '((type . private) (title . "Alice") (target-id . "10001")) nil)
        (with-temp-buffer
          (qq-chat-mode)
          (setq qq-chat--session-key "private:10001")
          (switch-to-buffer (current-buffer))
          (qq-chat-render)
          (evil-local-mode 1)
          (evil-insert-state)
          (qq-chat-edit-draft)
          (corfu-mode 1)
          (setq qq-completion--custom-faces
                '(((favorite_emoji_id . "favorite-one"))
                  ((favorite_emoji_id . "favorite-two"))))
          (let ((corfu-preselect 'first)
                (corfu-auto nil)
                (expected '(((type . "favorite_emoji")
                             (data . ((favorite_emoji_id . "favorite-two"))))))
                sent)
            (cl-letf (((symbol-function 'qq-core-send-message)
                       (lambda (_session segments &rest _) (push segments sent))))
              (execute-kbd-macro "/fav")
              (execute-kbd-macro (kbd "TAB <down>"))
              (execute-kbd-macro (kbd key))
              (should-not sent)
              (should (equal expected (qq-chat--current-input-segments)))
              (execute-kbd-macro (kbd key))
              (should (equal sent (list expected)))
              (should (equal "" (qq-chat--current-draft-string))))))))))

(defun qq-chat-test--exercise-semantic-return (modal)
  "Exercise plain timeline Return, completion and send restoration with MODAL."
  (save-window-excursion
    (dolist (key '("RET" "<return>"))
      (qq-chat-test-with-reset
        (qq-state-upsert-session
         "group:20001"
         '((type . group) (title . "Group") (target-id . "20001")) nil)
        (puthash "group:20001"
                 (list (qq-chat-test--canonical-message
                        "1" 100 "Ordinary timeline message"))
                 qq-state--messages-by-session)
        (with-temp-buffer
          (qq-chat-mode)
          (setq qq-chat--session-key "group:20001")
          (switch-to-buffer (current-buffer))
          (qq-chat-test--window "1" nil)
          (qq-chat-render)
          (when modal
            (evil-local-mode 1)
            (evil-normal-state))
          (qq-chat--set-draft "Unsent draft")
          (goto-char (point-min))
          (search-forward "Ordinary timeline")
          (let ((position (point))
                (draft (appkit-chatbuf-input-state)))
            (should-not (appkit-chatbuf-point-in-input-p))
            (execute-kbd-macro (kbd key))
            (should (= position (point)))
            (should (equal draft (appkit-chatbuf-input-state)))
            (should (equal "Unsent draft" (qq-chat--current-draft-string)))
            (when modal
              (should (eq evil-state 'normal))))
          (qq-operation-test--seed '(group-members "20001") :value '((members)))
          (setq-local appkit-chat-completion-functions
                      '(appkit-chat-completion-at-point))
          (qq-chat--set-draft "@nobody")
          (qq-chat-edit-draft)
          (let (sent failure)
            (cl-letf (((symbol-function 'qq-core-send-message)
                       (lambda (session segments raw _success error)
                         (push (list session segments raw) sent)
                         (setq failure error))))
              ;; A known empty native member result uses the real CAPF path.
              (should (eq 'member
                          (plist-get (qq-completion-token-at-point) :kind)))
              (execute-kbd-macro (kbd key))
              (should-not sent)
              (should (equal "@nobody" (appkit-chatbuf-input-string)))
              (execute-kbd-macro (kbd "C-c C-c"))
              (should
               (equal sent
                      '(("group:20001"
                         (((type . "text") (data . ((text . "@nobody")))))
                         "@nobody"))))
              (should (equal "" (appkit-chatbuf-input-state)))
              (funcall failure nil "test send failure")
              (should (equal "@nobody" (appkit-chatbuf-input-state)))
              (qq-chat-render)
              (qq-chat--set-draft "")
              (qq-chat-edit-draft)
              (execute-kbd-macro "rfoed!c")
              (should (equal "rfoed!c" (qq-chat--current-draft-string)))
              (should (= 1 (length sent))))))))))

(ert-deftest qq-chat-native-semantic-return-keeps-empty-completion-and-draft ()
  (qq-chat-test--exercise-semantic-return nil))

(ert-deftest qq-chat-evil-semantic-return-keeps-empty-completion-and-draft ()
  (skip-unless (require 'evil nil t))
  (qq-evil-setup)
  (qq-chat-test--exercise-semantic-return t))

(ert-deftest qq-chat-return-completes-unresolved-token-without-sending ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (dolist (token '("@green" "/斜眼" "/fav" ":rocket:"))
       (qq-chat--set-draft token)
       (qq-chat-edit-draft)
       (let (completed sent)
         (cl-letf (((symbol-function 'qq-completion-complete)
                    (lambda () (setq completed t) nil))
                   ((symbol-function 'qq-core-send-message)
                    (lambda (&rest _args) (setq sent t))))
           (qq-chat-return-dwim nil))
         (should completed)
         (should-not sent)
         (should (equal token (appkit-chatbuf-input-string))))))))

(ert-deftest qq-chat-prefixed-return-inserts-newline-without-completion-or-send ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "@green")
     (cl-letf (((symbol-function 'qq-completion-complete)
                (lambda () (ert-fail "prefix RET must not complete")))
               ((symbol-function 'qq-core-send-message)
                (lambda (&rest _args) (ert-fail "prefix RET must not send"))))
       (qq-chat-return-dwim '(4)))
     (should (equal "@green\n" (appkit-chatbuf-input-string))))))

(ert-deftest qq-chat-return-sends-non-completion-path-and-colon-text ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (cl-letf (((symbol-function 'qq-completion--base-face-candidates)
                (lambda () nil))
               ((symbol-function 'appkit-chat-emoji-candidates)
                (lambda (&optional _force) nil)))
       (dolist (text '("/tmp/foo" "https://example.com" ":unknown" ":)"))
         (qq-chat--set-draft text)
         (qq-chat-edit-draft)
         (let (sent)
           (cl-letf (((symbol-function 'qq-completion-complete)
                      (lambda ()
                        (ert-fail "ordinary text must not enter completion")))
                     ((symbol-function 'qq-core-send-message)
                      (lambda (_session segments &rest _args)
                        (setq sent segments))))
             (qq-chat-return-dwim nil))
           (should sent)))))))

(ert-deftest qq-chat-service-session-has-no-composer ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "service:u_mail"
    '((title . "QQ邮箱提醒")
      (type . service)
      (target-id . "u_mail")
      (chat-type . "103"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "service:u_mail")
     (qq-chat-render)
     (should-not (appkit-chatbuf-input-region-bounds))
     (should-not (string-match-p ">>> " (buffer-string)))
     (should-error (qq-chat-edit-draft) :type 'user-error)
     (should-error (qq-chat-send-message) :type 'user-error))))

(ert-deftest qq-chat-set-draft-preserves-shared-timeline-and-composer ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (qq-chat-render)
     (let ((ewoc (appkit-chat-timeline-ewoc))
           (input-start (appkit-chatbuf-input-start-position))
           (prompt-start (appkit-chatbuf-prompt-start-position)))
       (qq-chat--set-draft "updated body")
       (should (eq ewoc (appkit-chat-timeline-ewoc)))
       (should (= input-start (appkit-chatbuf-input-start-position)))
       (should (= prompt-start (appkit-chatbuf-prompt-start-position)))
       (should (equal "updated body" (appkit-chatbuf-input-state)))
       (should (equal "updated body" (qq-chat--current-draft-string)))))))

(ert-deftest qq-chat-refuses-to-sync-timeline-text-into-canonical-input ()
  (with-temp-buffer
    (qq-chat-mode)
    (appkit-chatbuf-input-state-set "safe draft")
    (cl-letf (((symbol-function 'appkit-chat-timeline-live-p) (lambda () t))
              ((symbol-function 'appkit-chatbuf-input-start-position)
               (lambda () 2))
              ((symbol-function 'appkit-chatbuf-prompt-start-position)
               (lambda () 1))
              ((symbol-function 'appkit-chat-timeline-footer-start-position)
               (lambda () 100))
              ((symbol-function 'appkit-chatbuf-input-state-sync)
               (lambda (&rest _args)
                 (ert-fail "invalid boundary must not sync input"))))
      (let ((result (qq-chat--sync-draft-from-buffer)))
        (should (equal "safe draft" result))
        (should (equal "safe draft" (appkit-chatbuf-input-state)))))))

(ert-deftest qq-chat-refuses-canonical-input-containing-message-rows ()
  (with-temp-buffer
    (qq-chat-mode)
    (appkit-chatbuf-input-state-set
     (propertize "not a draft" 'qq-chat-message-anchor "m1"))
    (should-error (qq-chat--render-canonical-input)
                  :type 'error)))

(ert-deftest qq-chat-mode-and-render-do-not-duplicate-prompt ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-render)
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((count 0)
           (pos (point-min)))
       (while (< pos (point-max))
         (when (and (eq (get-text-property pos 'field) 'appkit-chatbuf-prompt)
                    (not (eq (get-text-property (max (point-min) (1- pos)) 'field)
                             'appkit-chatbuf-prompt)))
           (setq count (1+ count)))
         (setq pos (1+ pos)))
       (should (= 1 count))))))

(ert-deftest qq-chat-render-keeps-composer-after-timeline-footer ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((footer (appkit-chat-timeline-footer-start-position))
           (prompt (appkit-chatbuf-prompt-start-position))
           (input (appkit-chatbuf-input-start-position)))
       (should (<= footer prompt input))
       (should (appkit-chatbuf-prompt-button-live-p))
       (should (string-suffix-p ">>> " (buffer-string)))))))

(ert-deftest qq-chat-render-preserves-footer-position ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat--set-reply-message
      '((server-id . "42")
        (session-key . "private:10001")
        (sender-name . "Alice")
        (raw-message . "[CQ:image,file=x.png,url=http://example.com/x]")
        (preview . "[image]")
        (segments . (((type . "image")
                      (data . ((file . "x.png")
                               (url . "http://example.com/x"))))))))
     (qq-chat--update-frame)
     (goto-char (point-min))
     (search-forward "Reply to Alice")
     (search-forward "[image]")
     (goto-char (point-min))
     (should-not (search-forward "[CQ:" nil t))
     (goto-char (point-min))
     (search-forward "Reply to Alice")
     (beginning-of-line)
     (let ((before (point)))
       (qq-chat-render)
       (should (= before (point)))
       (should-not (appkit-chatbuf-point-in-input-p)))
     ;; Cancel reply (C-c C-k / footer ×).
     (should (qq-chat--reply-message))
     (qq-chat-cancel-dwim)
     (should-not (qq-chat--reply-message))
     (goto-char (point-min))
     (should-not (search-forward "Reply to Alice" nil t)))))

(ert-deftest qq-chat-render-rejects-missing-sender-presentation ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Test Group")
      (target-id . "20001")
      (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "")
       (time . 100)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--ensure-view)
     (qq-chat-test--window "m1" nil)
     (should (equal (mapcar #'qq-chat--message-anchor
                            (qq-chat--timeline-messages))
                    '("m1")))
     (should-error (qq-chat-render) :type 'error))))

(ert-deftest qq-chat-render-recovery-rebuilds-rows-without-overwriting-rich-draft ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (puthash "private:10001"
            '(((server-id . "m1") (sender-id . "10001")
               (sender-name . "Alice") (time . 100) (raw-message . "complete message")))
            qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (qq-chat-test--window "m1" nil)
     (qq-chat-render)
     (qq-chat--set-draft
      (concat "unsent "
              (appkit-chatbuf-input-object-string
               "photo" '((type . "image") (data . ((file . "/draft/photo.png")))))
              " text"))
     (let ((draft (appkit-chatbuf-input-state))
           (visible (appkit-chatbuf-input-string))
           (printer (symbol-function 'qq-chat--row-printer))
           (fail-p t)
           (surface (appkit-current-surface)))
       (cl-letf (((symbol-function 'qq-chat--row-printer)
                  (lambda (row)
                    (if fail-p
                        (progn
                          (setq fail-p nil)
                          (insert "partial row")
                          (error "Interrupted row printer"))
                      (funcall printer row)))))
         (appkit-surface-send
          surface (list 'qq-render (appkit-projection-change-create :keys '("m1")))))
       (should (appkit-surface-live-p surface))
       (should (equal-including-properties draft (appkit-chatbuf-input-state)))
       (should (equal-including-properties visible (appkit-chatbuf-input-string)))
       (should (appkit-chatbuf-prompt-button-live-p))
       (should (qq-chat--composer-boundary-valid-p))
       (should (string-match-p "complete message" (buffer-string)))
       (should-not (string-match-p "partial row" (buffer-string)))))))

(ert-deftest qq-chat-message-title-face-colors-stable-sender-identity ()
  (let* ((original
          '((sender-native-id . "u_42")
            (sender-id . "10001")
            (sender-name . "Original Name")))
         (renamed
          '((sender-native-id . "u_42")
            (sender-id . "10001")
            (sender-name . "Renamed User")))
         (expected
          (list (appkit-name-color-face "10001")
                'qq-msg-user-title)))
    (should (equal expected (qq-chat--message-title-face original)))
    (should
     (equal (qq-chat--message-title-face original)
            (qq-chat--message-title-face renamed)))
    (should
     (equal (list (appkit-name-color-face "10001") 'qq-msg-self-title)
            (qq-chat--message-title-face
             '((sender-id . "10001") (self-p . t)))))))

(ert-deftest qq-chat-render-shows-group-card-and-nickname-like-telega ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Test Group")
      (target-id . "20001")
      (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "m1")
       (session-key . "group:20001")
       (message-type . "group")
       (sender-id . "10001")
       (sender-name . "Alice Card")
       (sender-secondary-name . "Alice Nick")
       (sender-card . "Alice Card")
       (sender-nickname . "Alice Nick")
       (time . 100)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-test--window "m1" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (should (search-forward "Alice Card • Alice Nick" nil t)))))

(ert-deftest qq-chat-render-uses-message-remark-with-nickname-trail ()
  (qq-chat-test-with-reset
   (qq-state-apply-friend-categories
    '(((category_id . 0) (sort_id . 0) (name . "好友")
       (online_count . 0)
       (friends . (((user_id . "10001")
                    (remark . "Current Directory Remark")
                    (nickname . "Alice Nick")))))))
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice Remark")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (session-key . "private:10001")
       (message-type . "private")
       (sender-id . "10001")
       (sender-name . "Alice Remark")
       (sender-secondary-name . "Alice Nick")
       (sender-nickname . "Alice Nick")
       (sender-remark . "Alice Remark")
       (time . 100)
       (raw-message . "hello")
       (raw-event . ((sender . ((user_id . "10001")
                                (nickname . "Alice Nick")))))))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-test--window "m1" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (should (search-forward "Alice Remark • Alice Nick" nil t)))))

(ert-deftest qq-chat-render-reuses-existing-message-nodes ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first"))
      ((server-id . "m2")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 200)
       (raw-message . "second")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-test--window "m1" nil)
     (qq-chat-render)
     (let ((ewoc (appkit-chat-timeline-ewoc))
           (node-m1 (appkit-chat-timeline-node "m1"))
           (node-m2 (appkit-chat-timeline-node "m2")))
       (puthash
        "private:10001"
        '(((server-id . "m1")
           (sender-id . "10001")
           (sender-name . "Alice")
           (time . 100)
           (raw-message . "first"))
          ((server-id . "m2")
           (sender-id . "10001")
           (sender-name . "Alice")
           (time . 200)
           (raw-message . "second updated"))
          ((server-id . "m3")
           (sender-id . "10001")
           (sender-name . "Alice")
           (time . 300)
           (raw-message . "third")))
        qq-state--messages-by-session)
       (qq-chat-test--window "m1" nil)
       (qq-chat-render)
       (should (eq ewoc (appkit-chat-timeline-ewoc)))
       (should (eq node-m1 (appkit-chat-timeline-node "m1")))
       (should (eq node-m2 (appkit-chat-timeline-node "m2")))
       (should (appkit-chat-timeline-node "m3"))
       (should (equal '("m1" "m2" "m3")
                      (appkit-chat-timeline-keys)))
       (should (string-match-p "second updated" (buffer-string)))))))

(ert-deftest qq-chat-render-preserves-empty-active-region-after-set-mark ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first"))
      ((server-id . "m2")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 200)
       (raw-message . "second")))
    qq-state--messages-by-session)
   (let ((transient-mark-mode t))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-test--window "m1" nil)
       (qq-chat-render)
       (goto-char (point-min))
       (search-forward "second")
       (beginning-of-line)
       (push-mark (point) t t)
       (let ((before (point)))
         (qq-chat-render)
         (should mark-active)
         (should (= before (point)))
         (should (= before (mark t))))))))

(ert-deftest qq-chat-node-invalidation-preserves-empty-active-region-after-set-mark ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first"))
      ((server-id . "m2")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 200)
       (raw-message . "second")))
    qq-state--messages-by-session)
   (let ((transient-mark-mode t))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-test--window "m1" nil)
       (qq-chat-render)
       (goto-char (point-min))
       (search-forward "second")
       (beginning-of-line)
       (push-mark (point) t t)
       (let ((before (point)))
         (appkit-chat-timeline-invalidate (appkit-chat-timeline-keys))
         (should mark-active)
         (should (= before (point)))
         (should (= before (mark t))))))))

(ert-deftest qq-chat-handle-state-change-ignores-unrelated-events ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (events)
       (cl-letf (((symbol-function 'qq-chat-render)
                  (lambda () (push 'render events)))
                 ((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest _) (push 'timeline events)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (push 'frame events)))
                 ((symbol-function 'qq-chat--header-line-update)
                  (lambda () (push 'header-line events))))
         (qq-chat--handle-state-change
          '(:type heartbeat :account-id "slot-a" :timestamp 1.0))
         (should-not events))))))

(ert-deftest qq-chat-projected-sync-replaces-empty-timeline-placeholder ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-test--window nil nil)
     (qq-chat-render)
     (should (equal (appkit-chat-timeline-keys)
                    (list qq-chat--empty-placeholder)))
     (goto-char (point-min))
     (should (search-forward "No messages loaded yet." nil t))
     (let ((messages '(((server-id . "m1")
                        (sender-id . "10001")
                        (sender-name . "Alice")
                        (time . 100)
                        (raw-message . "first")))))
       (puthash "private:10001" messages qq-state--messages-by-session)
       (qq-chat-test--window "m1" nil)
       (qq-chat--sync-timeline)
       (should (equal (appkit-chat-timeline-keys) '("m1")))
       (goto-char (point-min))
       (should (search-forward "first" nil t))
       (goto-char (point-min))
       (should-not (search-forward "No messages loaded yet." nil t))))))

(ert-deftest qq-chat-source-update-redisplays-reply-dependent-and-composer ()
  "Updating a source message refreshes its reply rows and active aux."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((source-old
           '((server-id . "100")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "old body"))))))))
          (source-new
           '((server-id . "100")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "new body"))))))))
          (reply
           '((server-id . "200")
             (sender-id . "10002")
             (sender-name . "Bob")
             (time . 200)
             (segments . (((type . "reply")
                           (data . ((id . "100"))))
                          ((type . "text")
                           (data . ((text . "answer"))))))))
          (initial (list source-old reply))
          (updated (list source-new reply)))
     (puthash "private:10001" initial qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-test--window "100" nil)
       (qq-chat-render)
       (qq-chat--set-reply-message source-old)
       (qq-chat--set-draft "draft stays")
       (qq-chat--update-frame)
       (puthash "private:10001" updated qq-state--messages-by-session)
       (qq-chat--apply-message-state-change
        (list :type 'message
              :session-key "private:10001"
              :message-anchor "100"
              :message source-new))
       (should (equal "new body"
                      (qq-state-message-preview (qq-chat--reply-message))))
       (should (equal "draft stays" (qq-chat--current-draft-string)))
       (goto-char (text-property-any (point-min) (point-max) 'qq-chat-reply-button t))
       (search-forward "Alice")
       (let ((faces (get-text-property (match-beginning 0) 'face)))
         (should (eq (car faces) (appkit-name-color-face "10001")))
         (should (memq 'appkit-chat-reply-author faces)))))))

(ert-deftest qq-chat-source-recall-redisplays-reply-dependent ()
  "Hidden recalled source still refreshes the row that quotes it."
  (qq-chat-test-with-reset
   (let ((qq-chat-show-recalled-messages nil)
         (source
          '((server-id . "m1")
            (sender-id . "10001")
            (sender-name . "Alice")
            (time . 100)
            (segments . (((type . "text")
                          (data . ((text . "old body"))))))))
         (reply
          '((server-id . "m2")
            (sender-id . "10002")
            (sender-name . "Bob")
            (time . 200)
            (segments . (((type . "reply")
                          (data . ((id . "m1"))))
                         ((type . "text")
                          (data . ((text . "answer")))))))))
     (qq-state-upsert-session
      "private:10001"
      '((title . "Alice")
        (target-id . "10001"))
      nil)
     (puthash "private:10001" (list source reply) qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-test--window "m1" nil)
       (qq-chat-render)
       (let ((recalled (qq-state--as-recalled-message source)))
         (puthash "private:10001" (list recalled reply)
                  qq-state--messages-by-session)
         (qq-chat--apply-message-state-change
          (list :type 'message :message-anchor "m1" :message recalled)))
       (should-not (appkit-chat-timeline-node "m1"))
       (should (appkit-chat-timeline-node "m2"))
       (should-not (string-match-p "old body" (buffer-string)))))))

(ert-deftest qq-chat-history-source-arrival-redisplays-reply-dependent ()
  "History source arrival resolves an already rendered reply preview."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((source
           '((server-id . "100")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "source arrived"))))))))
          (reply
           '((server-id . "200")
             (sender-id . "10002")
             (sender-name . "Bob")
             (time . 200)
             (segments . (((type . "reply")
                           (data . ((id . "100"))))
                          ((type . "text")
                           (data . ((text . "answer")))))))))
     (puthash "private:10001" (list reply) qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-test--window nil nil)
       (qq-chat-render)
       (should-not (string-match-p "source arrived" (buffer-string)))
       (puthash "private:10001" (list source reply) qq-state--messages-by-session)
       (qq-chat--sync-timeline :changed-resources '((:message "100")))
       (goto-char (text-property-any (point-min) (point-max) 'qq-chat-reply-button t))
       (should (search-forward "source arrived" (line-end-position) t))))))

(ert-deftest qq-chat-pending-promote-rekeys-anchor-to-snowflake ()
  "Optimistic local-id row is rekeyed to NT snowflake after send."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-test--window nil nil)
     (qq-chat-render)
     (let* ((local-id "local-9")
            (snowflake "9007199254741004645")
            (pending `(((local-id . ,local-id)
                        (self-p . t)
                        (status . pending)
                        (sender-id . "90001")
                        (sender-name . "Me")
                        (time . 100)
                        (raw-message . "hi")
                        (preview . "hi"))))
            (sent `(((local-id . ,local-id)
                     (server-id . ,snowflake)
                     (id . ,snowflake)
                     (self-p . t)
                     (status . sent)
                     (sender-id . "90001")
                     (sender-name . "Me")
                     (time . 100)
                     (raw-message . "hi")
                     (preview . "hi")))))
       (puthash "private:10001" pending qq-state--messages-by-session)
       (qq-chat--sync-timeline)
       (let ((node (appkit-chat-timeline-node local-id)))
         (should node)
         (should (equal (appkit-chat-timeline-keys) (list local-id)))
         (setq qq-chat--message-selection
               (qq-chat-test--selection local-id))
         (puthash "private:10001" sent qq-state--messages-by-session)
         (qq-chat--apply-message-state-change
          (list :type 'message
                :message-anchor snowflake
                :previous-anchor local-id
                :message (car sent)))
         (should-not (appkit-chat-timeline-node local-id))
         (should (eq node (appkit-chat-timeline-node snowflake)))
         (should
          (equal (qq-chat--message-selection-anchors) (list snowflake))))
       (should (equal (appkit-chat-timeline-keys) (list snowflake)))
       (goto-char (point-min))
       (should (search-forward "hi" nil t))))))

(ert-deftest qq-chat-correlated-duplicate-collapse-keeps-canonical-node ()
  "A historical duplicate is removed without rekeying onto an existing row."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Group")
      (target-id . "20001")
      (type . group))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-test--window nil nil)
     (let* ((legacy-id "72057595033281691")
            (snowflake "2083983882109904220")
            (legacy `((server-id . ,legacy-id)
                      (sender-id . "90001")
                      (sender-name . "Me")
                      (time . 100)
                      (raw-message . "same")))
            (canonical `((local-id . "local-1")
                         (server-id . ,snowflake)
                         (sender-id . "90001")
                         (sender-name . "Me")
                         (time . 100)
                         (raw-message . "same"))))
       (puthash "group:20001" (list legacy canonical)
                qq-state--messages-by-session)
       (qq-chat-test--window legacy-id nil)
       (qq-chat--sync-timeline)
       (let ((canonical-node (appkit-chat-timeline-node snowflake)))
         (should (appkit-chat-timeline-node legacy-id))
         (should canonical-node)
         (setq qq-chat--message-selection
               (qq-chat-test--selection legacy-id))
         (puthash "group:20001" (list canonical)
                  qq-state--messages-by-session)
         (qq-chat--apply-message-state-change
          (list :type 'message
                :message-anchor snowflake
                :previous-anchor legacy-id
                :message canonical))
         (should-not (appkit-chat-timeline-node legacy-id))
         (should (eq canonical-node
                     (appkit-chat-timeline-node snowflake)))
         (should
          (equal (qq-chat--message-selection-anchors) (list snowflake))))))))

(ert-deftest qq-chat-projected-sync-restores-empty-timeline-placeholder ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let ((messages '(((server-id . "m1")
                      (sender-id . "10001")
                      (sender-name . "Alice")
                      (time . 100)
                      (raw-message . "first")))))
     (puthash "private:10001" messages qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-test--window "m1" nil)
       (qq-chat-render)
       (should (equal (appkit-chat-timeline-keys) '("m1")))
       (puthash "private:10001" nil qq-state--messages-by-session)
       (qq-chat--sync-timeline)
       (should (equal (appkit-chat-timeline-keys)
                      (list qq-chat--empty-placeholder)))
       (goto-char (point-min))
       (should (search-forward "No messages loaded yet." nil t))))))

(ert-deftest qq-chat-prompt-presents-destination-avatar ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Readable Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let ((avatar (copy-sequence "#")))
       (put-text-property 0 1 'display 'group-avatar avatar)
       (cl-letf (((symbol-function
                   'qq-media-session-avatar-display-string)
                  (lambda (_session) avatar)))
         (let ((prompt (qq-chat--prompt-text)))
           (should (equal "# >>> " (substring-no-properties prompt)))
           (should (eq 'group-avatar
                       (get-text-property 0 'display prompt)))
           (should
            (equal "Message destination: Readable Group"
                   (get-text-property 0 'help-echo prompt)))))))))

(ert-deftest qq-chat-compact-message-shows-its-own-send-status ()
  "A continuation row must not hide its pending or failed state."
  (with-temp-buffer
    (qq-chat--insert-compact-message-body
     '((status . pending)
       (segments . (((type . "text")
                     (data . ((text . "continuation")))))))
     (appkit-ui-make-prefix-state "  " "  ")
     nil
     "09:36")
    (goto-char (point-min))
    (should (search-forward "continuation" nil t))
    (let ((line-end (line-end-position)))
      (should (search-forward "…" line-end t))
      (should (equal (get-text-property (1- (point)) 'help-echo)
                     "pending"))
      (should (search-forward "09:36" line-end t)))))

(ert-deftest qq-chat-face-occurrences-survive-normal-and-compact-rendering ()
  "Both message layouts must render every repeated face as a visible glyph."
  (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
  (let ((buffer (generate-new-buffer " *qq-face-occurrences-test*"))
        (file (make-temp-file "qq-face-occurrences-" nil ".svg"))
        (qq-media--resource-cache (make-hash-table :test #'equal))
        (qq-media--image-cache (make-hash-table :test #'equal))
        (message
         `((segments
            . ,(make-list
                3 '((type . "face")
                    (data . ((id . "178") (description . "/斜眼笑")))))))))
    (unwind-protect
        (save-window-excursion
          (with-temp-file file
            (insert "<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16'><rect width='16' height='16' fill='orange'/></svg>"))
          (qq-media--cache-resource "face:178" `((file . ,file)))
          (let ((image (create-image file 'svg nil :scale 1.0)))
            (qq-media--cache-image "face:178" image)
            (switch-to-buffer buffer)
            (dolist (compact '(nil t))
              (erase-buffer)
              (if compact
                  (qq-chat--insert-compact-message-body message "" nil nil)
                (qq-chat--insert-message-body message "" nil))
              (goto-char (point-min))
              (redisplay t)
              (should (= (car (window-text-pixel-size
                               nil (point-min) (point-max)))
                         (* 3 (car (image-size image t)))))
              (should (equal (buffer-substring-no-properties
                              (point-min) (point-max))
                             "/斜眼笑/斜眼笑/斜眼笑\n")))))
      (kill-buffer buffer)
      (delete-file file))))

(ert-deftest qq-chat-native-priority-mentions-use-attention-face ()
  (cl-letf (((symbol-function 'qq-state-self-user-id) (lambda () "90001")))
    (let ((self (qq-chat--segment-inline-string
                 '((type . "at") (data . ((qq . "90001") (name . "Me"))))))
          (all (qq-chat--segment-inline-string
                '((type . "at") (data . ((qq . "all"))))))
          (other (qq-chat--segment-inline-string
                  '((type . "at") (data . ((qq . "10002") (name . "Bob")))))))
      (should (eq 'at-me (get-text-property 0 'qq-chat-mention-kind self)))
      (should (eq 'at-all (get-text-property 0 'qq-chat-mention-kind all)))
      (should (eq 'ordinary (get-text-property 0 'qq-chat-mention-kind other)))
      (should (eq 'qq-msg-mention-self (get-text-property 0 'face self)))
      (should (eq 'qq-msg-mention-self (get-text-property 0 'face all)))
      (should
       (equal (list (appkit-name-color-face "10002") 'qq-msg-mention)
              (get-text-property 0 'face other)))
      (should (equal "90001"
                     (get-text-property 0 'qq-chat-mention-user-id self)))
      (should-not (get-text-property 0 'qq-chat-mention-user-id all))
      (should (equal "10002"
                     (get-text-property 0 'qq-chat-mention-user-id other))))))

(ert-deftest qq-chat-historical-mention-opens-mentioned-user-profile ()
  (let ((message
         '((segments
            . (((type . "text") (data . ((text . "你好，"))))
               ((type . "at")
                (data . ((qq . "10001") (name . "Alice Card"))))
               ((type . "text") (data . ((text . "！"))))))))
        opened-user-id)
    (with-temp-buffer
      (qq-chat--insert-message-body message nil nil)
      (goto-char (point-min))
      (should (search-forward "@Alice Card" nil t))
      (let ((button (button-at (1- (point)))))
        (should button)
        (goto-char (button-start button))
        (should (equal (key-binding (kbd "RET")) #'push-button))
        (should (equal "10001"
                       (get-text-property (point) 'qq-chat-mention-user-id)))
        (cl-letf (((symbol-function 'qq-user-open)
                   (lambda (user-id) (setq opened-user-id user-id))))
          (button-activate button)))
      (should (equal opened-user-id "10001")))))

(ert-deftest qq-chat-at-all-is-emphasized-but-not-a-user-link ()
  (let ((mention (qq-chat--segment-inline-string
                  '((type . "at") (data . ((qq . "all")))))))
    (with-temp-buffer
      (insert mention)
      (goto-char (point-min))
      (should (equal (buffer-string) "@全体成员"))
      (should-not (button-at (point)))
      (should (eq (get-text-property (point) 'qq-chat-mention-kind) 'at-all)))))

(ert-deftest qq-chat-renders-clickable-reaction-chips ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254741004001")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (segments . (((type . "text") (data . ((text . "hello"))))))
       (reactions . (((emoji-id . "178")
                      (emoji-type . "1")
                      (count . 3)
                      (chosen-p . t))))))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-test--window "9007199254741004001" nil)
     (cl-letf (((symbol-function 'qq-media-face-display-string)
                (lambda (_emoji-id) "/斜眼笑")))
       (qq-chat-render))
     (goto-char (point-min))
     (should (search-forward "/斜眼笑 3" nil t))
     (let ((button (button-at (1- (point)))))
       (should button)
       (should (eq (button-get button 'face) 'qq-msg-reaction-chosen))))))

(ert-deftest qq-chat-renders-group-essence-badge ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254741004001")
       (session-key . "group:20001")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (segments . (((type . "text") (data . ((text . "hello"))))))
       (essence-p . t)))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-test--window "9007199254741004001" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (should (search-forward "★ 精华消息" nil t))
     (should (eq (get-text-property (1- (point)) 'face)
                 'font-lock-keyword-face)))))

(ert-deftest qq-chat-reaction-chip-toggles-current-selection ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254741004001")
       (session-key . "group:20001")
       (gateway-account-id . "slot-a")
       (reactions . (((emoji-id . "178")
                      (emoji-type . "1")
                      (count . 1)
                      (chosen-p . t))))))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let (called)
       (cl-letf (((symbol-function 'qq-account-current-id)
                  (lambda () "slot-a"))
                 ((symbol-function 'qq-core-set-message-reaction)
                  (lambda (message reaction set &rest _)
                    (setq called (list message reaction set)))))
         (qq-chat-toggle-message-reaction
          "9007199254741004001"
          '((emoji-id . "178") (emoji-type . "1"))))
       (should (equal (alist-get 'server-id (nth 0 called))
                      "9007199254741004001"))
       (should (equal (alist-get 'session-key (nth 0 called))
                      "group:20001"))
       (should (equal (nth 1 called)
                      '((emoji-id . "178") (emoji-type . "1"))))
       (should-not (nth 2 called))))))

(ert-deftest qq-chat-reaction-bindings-follow-session-and-point-context ()
  (require 'qq-evil)
  (let ((saved-overrides appkit-binding-overrides))
    (unwind-protect
        (dolist (overrides '(nil ((react :read-only ("<f8>") :evil ("<f8>")))))
          (let ((appkit-binding-overrides overrides)
                (qq-evil-enable-integration t))
            (qq-evil-setup)
            (appkit-bindings-refresh)
            (dolist (modal (if (require 'evil nil t) '(nil t) '(nil)))
              (dolist (type '(private group))
                (qq-chat-test-with-reset
                 (let* ((session-key (format "%s:20001" type))
                        (key (kbd (if overrides "<f8>" "!"))))
                   (qq-state-upsert-session
                    session-key `((type . ,type) (target-id . "20001")) nil)
                   (with-temp-buffer
                     (qq-chat-mode)
                     (setq qq-chat--session-key session-key)
                     (when modal
                       (evil-local-mode 1)
                       (evil-normal-state))
                     (qq-chat-render)
                     (dotimes (_ 2)
                       (goto-char (point-min))
                       (appkit-chatbuf-update-context-mode)
                       (should (eq (eq (key-binding key) #'qq-chat-react-to-message)
                                   (eq type 'group)))
                       (when overrides
                         (should-not (eq (key-binding (kbd "!"))
                                         #'qq-chat-react-to-message)))
                       (goto-char (appkit-chatbuf-input-start-position))
                       (appkit-chatbuf-update-context-mode)
                       (should-not (eq (key-binding key)
                                       #'qq-chat-react-to-message))))))))))
      (let ((appkit-binding-overrides saved-overrides))
        (appkit-bindings-refresh)))))

(ert-deftest qq-chat-react-rejects-private-message-before-chooser ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((type . private) (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let ((message '((server-id . "9007199254741004001")
                      (session-key . "private:10001")
                      (gateway-account-id . "slot-a")))
           prompted sent)
       (cl-letf (((symbol-function 'qq-completion-read-reaction)
                  (lambda () (setq prompted t) "178"))
                 ((symbol-function 'qq-core-set-message-reaction)
                  (lambda (&rest _) (setq sent t))))
         (should-error (qq-chat-react-to-message message) :type 'user-error)
         (should-not prompted)
         (should-not sent))))))

(ert-deftest qq-chat-react-command-adds-picked-face ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (let ((message '((server-id . "9007199254741004001")
                    (session-key . "group:20001")
                    (gateway-account-id . "slot-a"))))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (let (called)
         (cl-letf (((symbol-function 'qq-core-set-message-reaction)
                    (lambda (selected reaction set &rest _)
                      (setq called (list selected reaction set))))
                   ((symbol-function 'qq-account-current-id)
                    (lambda () "slot-a")))
           (qq-chat-react-to-message message "178"))
         (should (eq (nth 0 called) message))
         (should (equal (nth 1 called)
                        '((emoji-id . "178") (emoji-type . "1"))))
         (should (eq (nth 2 called) t)))))))

(ert-deftest qq-chat-essence-command-toggles-current-state ()
  (qq-chat-test-with-reset
   (progn
     (qq-state-upsert-session
      "group:20001"
      '((type . group) (title . "Group") (target-id . "20001"))
      nil)
     (let ((message '((server-id . "9007199254741004001")
                      (session-key . "group:20001")
                      (message-seq . "9007199254740999")
                      (native-random . 7)
                      (gateway-account-id . "slot-a")
                      (essence-p . t))))
       (with-temp-buffer
         (qq-chat-mode)
         (setq qq-chat--session-key "group:20001")
         (let (called)
           (cl-letf (((symbol-function 'qq-account-current-id)
                      (lambda () "slot-a"))
                     ((symbol-function 'qq-core-set-message-essence)
                      (lambda (selected set &rest _)
                        (setq called (list selected set)))))
             (qq-chat-toggle-message-essence message))
           (should (eq (nth 0 called) message))
           (should-not (nth 1 called))))))))

(ert-deftest qq-chat-native-essence-capability-uses-the-message-reference ()
  (qq-chat-test-with-reset
   (progn
     (qq-state-upsert-session
      "group:20001"
      '((type . group) (title . "Group") (target-id . "20001"))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-account-current-id)
                  (lambda () "slot-a")))
         (let ((message
                '((server-id . "9007199254741004001")
                  (session-key . "group:20001")
                  (gateway-account-id . "slot-a"))))
           (should (qq-chat--message-essence-capable-p message))
           (setf (alist-get 'gateway-account-id message) "slot-b")
           (should-not (qq-chat--message-essence-capable-p message))))))))

(ert-deftest qq-chat-todo-commands-route-all-closed-operations ()
  (qq-chat-test-with-reset
   (progn
     (qq-state-upsert-session
      "group:20001"
      '((type . group) (title . "Group") (target-id . "20001"))
      nil)
     (let ((message '((server-id . "9007199254741004001")
                      (session-key . "group:20001")
                      (message-seq . "9007199254740999")
                      (gateway-account-id . "slot-a")))
           calls)
       (with-temp-buffer
         (qq-chat-mode)
         (setq qq-chat--session-key "group:20001")
         (cl-letf (((symbol-function 'qq-account-current-id)
                    (lambda () "slot-a"))
                   ((symbol-function 'qq-core-set-message-todo)
                    (lambda (selected operation &rest _)
                      (push (list selected operation) calls))))
           (qq-chat-set-message-todo message)
           (qq-chat-complete-message-todo message)
           (qq-chat-cancel-message-todo message))
         (should
          (equal (mapcar #'cadr (nreverse calls))
                 '(set complete cancel))))))))

(ert-deftest qq-chat-native-todo-capability-uses-reference-and-account ()
  (qq-chat-test-with-reset
   (progn
     (qq-state-upsert-session
      "group:20001"
      '((type . group) (title . "Group") (target-id . "20001"))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-account-current-id)
                  (lambda () "slot-a")))
         (let ((message
                '((server-id . "9007199254741004001")
                  (session-key . "group:20001")
                  (gateway-account-id . "slot-a"))))
           (should (qq-chat--message-todo-capable-p message))
           (setf (alist-get 'gateway-account-id message) "slot-b")
           (should-not (qq-chat--message-todo-capable-p message))))))))

(ert-deftest qq-chat-poke-row-hides-ordinary-message-actions ()
  (qq-chat-test-with-reset
   (progn
     (qq-state-upsert-session
      "group:20001"
      '((type . group) (title . "Group") (target-id . "20001"))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-account-current-id)
                  (lambda () "slot-a")))
         (let ((message
                '((server-id . "9007199254741004001")
                  (session-key . "group:20001")
                  (gateway-account-id . "slot-a")
                  (timeline-class . service)
                  (segments
                   . (((type . "gray-tip")
                       (data . ((kind . "poke")))))))))
           (should-not (qq-chat--message-forwardable-p message))
           (should-not (qq-chat--message-reactable-p message))
           (should-not (qq-chat--message-essence-capable-p message))
           (should-not (qq-chat--message-todo-capable-p message))))))))

(ert-deftest qq-chat-poke-sender-uses-sender-not-message-target ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let ((at-point '((sender-id . "10002")
                       (target-id . "20001")
                       (self-p . t)))
           call)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda () at-point))
                 ((symbol-function 'qq-core-send-poke)
                  (lambda (session-key target-id &optional callback _errback)
                    (setq call (list session-key target-id))
                    (when callback (funcall callback nil)))))
         (qq-chat-poke-sender))
       (should (equal call '("group:20001" "10002")))))))

(ert-deftest qq-chat-poke-sender-can-target-self-in-private-chat ()
  (qq-chat-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (qq-state-upsert-session
    "private:10002"
    '((type . private) (title . "Peer")
      (target-id . "10002") (peer-uin . "10002"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10002")
     (let (call)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda () '((sender-id . "90001") (self-p . t))))
                 ((symbol-function 'qq-core-send-poke)
                  (lambda (session-key target-id &optional _callback _errback)
                    (setq call (list session-key target-id)))))
         (qq-chat-poke-sender))
       (should (equal call '("private:10002" "90001")))))))

(ert-deftest qq-chat-refuses-sequence-only-group-recall ()
  (let ((qq-chat--session-key "group:20001")
        (message '((id . "history:slot-a:group:20001:105544:none")
                   (session-key . "group:20001")
                   (gateway-account-id . "slot-a")
                   (message-seq . "105544")
                   (segments . (((type . "text"))))))
        prompted
        called)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _) (setq prompted t)))
              ((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-core-recall-message)
               (lambda (&rest _) (setq called t))))
      (should-error (qq-chat--recall-message-internal message)
                    :type 'user-error))
    (should-not prompted)
    (should-not called)))

(ert-deftest qq-chat-refuses-an-unaddressable-poke-before-confirmation ()
  (let ((message
         '((server-id . "9007199254741004001")
           (self-p . t)
           (timeline-class . service)
           (segments
            . (((type . "gray-tip")
                (data . ((kind . "poke"))))))))
        prompted
        api-called)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (setq prompted t)))
              ((symbol-function 'qq-core-recall-poke)
               (lambda (&rest _)
                 (setq api-called t))))
      (should-error (qq-chat--recall-message-internal message)
                    :type 'user-error))
    (should-not prompted)
    (should-not api-called)))

(ert-deftest qq-chat-send-poke-uses-explicit-member-chooser ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let (chooser-call api-call)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda () '((sender-id . "10002")
                               (target-id . "20001"))))
                 ((symbol-function 'qq-completion-read-poke-target)
                  (lambda (session-key callback &optional initial-user-id)
                    (setq chooser-call (list session-key initial-user-id))
                    (funcall callback "10003")))
                 ((symbol-function 'qq-core-send-poke)
                  (lambda (session-key target-id &optional _callback _errback)
                    (setq api-call (list session-key target-id)))))
         (qq-chat-send-poke))
       (should (equal chooser-call '("group:20001" "10002")))
       (should (equal api-call '("group:20001" "10003")))))))

(ert-deftest qq-chat-poke-rejects-service-and-invalid-senders ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "service:u_mail"
    '((type . service) (title . "Mail") (target-id . "u_mail"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "service:u_mail")
     (should-error (qq-chat-send-poke "10002") :type 'user-error))
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (cl-letf (((symbol-function 'qq-chat--message-at-point)
                (lambda () '((sender-id . "0")))))
       (should-error (qq-chat-poke-sender) :type 'user-error)))))

(ert-deftest qq-chat-private-poke-rejects-third-party-target-before-backend ()
  (qq-chat-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (qq-state-upsert-session
    "private:10002"
    '((type . private) (title . "Peer") (target-id . "10002"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10002")
     (let ((api-called nil))
       (cl-letf (((symbol-function 'qq-core-send-poke)
                  (lambda (&rest _args) (setq api-called t))))
         (should-error (qq-chat-send-poke "77777") :type 'user-error))
       (should-not api-called)))))

(ert-deftest qq-chat-node-refresh-defers-while-mark-active ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first")))
    qq-state--messages-by-session)
   (let ((transient-mark-mode t))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-test--window "m1" nil)
       (qq-chat-render)
       (goto-char (point-min))
       (search-forward "first")
       (beginning-of-line)
       (push-mark (point) t t)
       (let (invalidated)
         (cl-letf (((symbol-function 'appkit-ewoc-invalidate-key)
                    (lambda (_ewoc _table key) (push key invalidated))))
           (qq-chat--request-row-redisplay '("m1"))
           (should-not invalidated)
           (setq mark-active nil)
           (appkit-chat-timeline-flush-deferred)
           (should (equal invalidated '("m1")))))))))

(ert-deftest qq-chat-row-refresh-uses-shared-invalidation-api ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first"))
      ((server-id . "m2")
       (sender-id . "10002")
       (sender-name . "Bob")
       (time . 200)
       (raw-message . "second")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let (call)
       (cl-letf (((symbol-function 'appkit-chat-timeline-invalidate)
                  (lambda (keys &rest options)
                    (setq call (cons keys options)))))
         (qq-chat--request-row-redisplay '("m1" "m2"))
         (should (equal (car call) '("m1" "m2")))
         (should (eq (plist-get (cdr call) :defer-while-mark-active) t)))))))

(ert-deftest qq-chat-first-unread-anchor-requires-exact-native-position ()
  (dolist (metadata
           '(((unread-message-count . 2))
             ((unread-badge-count . 2))
             ((first-unread-message-id . "m3"))
             ((first-unread-message-seq . "3"))))
    (qq-chat-test-with-reset
     (qq-state-upsert-session
      "private:10001"
      (append '((title . "Alice") (target-id . "10001")) metadata)
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (let ((appkit-chat-show-unread-divider t))
         (should-not
          (qq-chat--first-unread-anchor
           '(((server-id . "m1"))
             ((server-id . "m2"))
             ((server-id . "m3"))))))))))

(ert-deftest qq-chat-first-unread-anchor-latches-before-history-loads ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001")
      (first-unread-message-id . "m3")
      (first-unread-message-seq . "3"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let ((appkit-chat-show-unread-divider t))
       (qq-chat--project-timeline nil)
       (qq-state-upsert-session
        "private:10001"
        '((first-unread-message-id . "m4")
          (first-unread-message-seq . "4"))
        nil)
       (should-not
        (qq-chat--first-unread-anchor '(((server-id . "m4")))))
       (should (equal "m3"
                      (qq-chat--first-unread-anchor
                       '(((server-id . "m3"))
                         ((server-id . "m4"))))))))))

(ert-deftest qq-chat-unread-boundary-clears-only-after-confirmed-completion ()
  (qq-chat-test-with-reset
   (let* ((session-key "group:20001")
          (first (qq-chat-test--canonical-message "100" 100 "first"))
          (second (qq-chat-test--canonical-message "200" 101 "second"))
          (third (qq-chat-test--canonical-message "300" 102 "third"))
          (messages (list first second third))
          (qq-auto-mark-read nil)
          (qq-chat-show-recalled-messages nil)
          (appkit-chat-show-unread-divider t))
     (qq-state-upsert-session
      session-key
      '((title . "Group") (type . group) (target-id . "20001")
        (unread-message-count . 2)
        (first-unread-message-id . "200")
        (first-unread-message-seq . "200"))
      nil)
     (puthash session-key messages qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key)
       (qq-chat-test--window "100" "300")
       (qq-chat-render)
       (qq-chat--set-draft "draft stays here")
       (cl-labels
           ((divider-anchor ()
              (when-let* ((position
                           (text-property-any
                            (point-min) (point-max)
                            'appkit-chat-read-divider t)))
                (get-text-property position 'qq-chat-message-anchor))))
         (should (equal (divider-anchor) "200"))
         (should-not
          (plist-get (appkit-chat-timeline-row-context
                      (appkit-chat-timeline-row "200"))
                     :compact))
         (should
          (plist-get (appkit-chat-timeline-row-context
                      (appkit-chat-timeline-row "300"))
                     :compact))
         ;; A receipt changes the server frontier, not this reading boundary.
         (qq-state-upsert-session
          session-key
          '((unread-message-count . 1)
            (first-unread-message-id . "300")
            (first-unread-message-seq . "300"))
          nil)
         (qq-chat--apply-read-state-change)
         (should (equal (divider-anchor) "200"))
         ;; A missing count/position or a cleared badge cannot prove completion.
         (qq-state-upsert-session
          session-key
          '((unread-message-count . nil) (unread-badge-count . 0)
            (first-unread-message-id . nil) (first-unread-message-seq . nil))
          nil)
         (qq-chat--apply-read-state-change)
         (should (equal (divider-anchor) "200"))
         ;; Hidden and deleted anchors never move the bar to the next row.
         (setf (alist-get 'status second) 'recalled)
         (qq-chat--sync-timeline)
         (should-not (divider-anchor))
         (puthash session-key (list first third) qq-state--messages-by-session)
         (qq-chat--sync-timeline)
         (should-not (divider-anchor))
         (setf (alist-get 'status second) 'received)
         (puthash session-key messages qq-state--messages-by-session)
         (qq-chat--sync-timeline)
         (should (equal (divider-anchor) "200"))
         (qq-state-upsert-session
          session-key
          '((unread-message-count . 0)
            (first-unread-message-id . nil)
            (first-unread-message-seq . nil))
          nil)
         (qq-chat--apply-read-state-change)
         (should-not (divider-anchor))
         (should (equal (qq-chat--current-draft-string) "draft stays here"))
         (should (equal (cdar (plist-get (qq-chat--model) :rows)) "100"))
         (should (equal (cdar (last (plist-get (qq-chat--model) :rows))) "300"))
         (should (equal (qq-state-session-messages session-key) messages))
         ;; A replacement Surface starts a new reading session.
         (qq-state-upsert-session
          session-key
          '((unread-message-count . 1)
            (first-unread-message-id . "300")
            (first-unread-message-seq . "300"))
          nil)
         (appkit-surface-stop (appkit-current-surface))
         ;; A new Surface has its own window, even with the same cached rows.
         (qq-chat-test--window "100" "300")
         (qq-chat-render)
         (should (equal (divider-anchor) "300"))
         (should (equal (qq-chat--current-draft-string) "draft stays here")))))))

(defconst qq-chat-test--1x1-png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
  "Minimal 1x1 PNG bytes for clipboard attach tests.")

(ert-deftest qq-chat-attach-clipboard-from-image-png ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (cl-letf (((symbol-function 'appkit-media-clipboard-source)
                (lambda () (list :mime-type "image/png" :suffix ".png"
                                 :data qq-chat-test--1x1-png))))
       (qq-chat-attach-clipboard)
       (should (appkit-chatbuf-input-has-objects-p))
       (let ((segments (qq-chat--current-input-segments)))
         (should (= 1 (length segments)))
         (should (equal "image" (alist-get 'type (car segments))))
         (should (file-readable-p
                  (alist-get 'file (alist-get 'data (car segments))))))))))

(ert-deftest qq-chat-clipboard-draft-removal-deletes-only-owned-sources ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((user-file (make-temp-file "qq-user-attachment-" nil ".txt")))
       (unwind-protect
           (let ((clipboard (qq-chat--yank-media "application/octet-stream"
                                                "clipboard bytes" t)))
             (qq-chat-attach-file user-file "file")
             (qq-chat--sync-draft-from-buffer)
             (should (file-exists-p clipboard))
             (should (equal (file-name-directory clipboard)
                            (file-name-as-directory
                             (expand-file-name temporary-file-directory))))
             (should (equal qq-chat--clipboard-files (list clipboard)))
             (with-temp-buffer
               (insert-file-contents-literally clipboard)
               (should (equal (buffer-string) "clipboard bytes")))
             (qq-chat-clear-draft)
             (should-not (file-exists-p clipboard))
             (should (file-exists-p user-file))
             (should-not qq-chat--clipboard-files))
         (delete-file user-file))))))

(ert-deftest qq-chat-clipboard-send-retains-source-until-success ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((file (qq-chat--yank-media "application/octet-stream" "bytes" t))
           complete)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session segments &optional _raw callback _errback)
                    (should (equal (alist-get 'file
                                              (alist-get 'data (car segments)))
                                   file))
                    (should (file-readable-p file))
                    (setq complete callback))))
         (qq-chat-send-message)
         (should (equal "" (appkit-chatbuf-input-state)))
         (qq-chat--set-draft "new draft while Gateway stages")
         (should (file-exists-p file))
         (funcall complete '((sent . t)))
         (should-not (file-exists-p file))
         (should-not qq-chat--clipboard-send-owners)
         (should (equal "new draft while Gateway stages"
                        (appkit-chatbuf-input-state)))
         (funcall complete '((sent . t))))))))

(ert-deftest qq-chat-clipboard-send-failure-retains-only-restorable-draft ()
  (dolist (newer-draft-p '(nil t))
    (qq-chat-test-with-reset
     (qq-state-upsert-session
      "private:10001" '((title . "Alice") (target-id . "10001")) nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (let ((file (qq-chat--yank-media "application/octet-stream" "bytes" t))
             failed)
         (cl-letf (((symbol-function 'qq-core-send-message)
                    (lambda (_session _segments &optional _raw _callback errback)
                      (setq failed errback))))
           (qq-chat-send-message)
           (when newer-draft-p (qq-chat--set-draft "newer"))
           (should (file-exists-p file))
           (funcall failed nil "staging failed")
           (should-not qq-chat--clipboard-send-owners)
           (if newer-draft-p
               (progn
                 (should (equal (appkit-chatbuf-input-state) "newer"))
                 (should-not (file-exists-p file)))
             (should (member file
                             (qq-chat--clipboard-draft-files
                              (appkit-chatbuf-input-state))))
             (should (file-exists-p file))
             (qq-chat-clear-draft)
             (should-not (file-exists-p file)))))))))

(ert-deftest qq-chat-clipboard-synchronous-send-error-keeps-draft-source ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((file (qq-chat--yank-media "application/octet-stream" "bytes" t)))
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (&rest _args) (error "staging unavailable"))))
         (should-error (qq-chat-send-message))
         (should (file-exists-p file))
         (should-not qq-chat--clipboard-send-owners)
         (should (member file
                         (qq-chat--clipboard-draft-files
                          (appkit-chatbuf-input-state))))
         (qq-chat-clear-draft)
         (should-not (file-exists-p file)))))))

(ert-deftest qq-chat-clipboard-teardown-revokes-pending-send-restoration ()
  (dolist (teardown '(kill mode reset))
    (qq-chat-test-with-reset
     (qq-state-upsert-session
      "private:10001" '((title . "Alice") (target-id . "10001")) nil)
     (let ((buffer (generate-new-buffer " *qq-clipboard-lifetime*"))
           file failed)
       (unwind-protect
           (progn
             (with-current-buffer buffer
               (qq-chat-mode)
               (setq qq-chat--session-key "private:10001")
               (qq-chat-render)
               (setq file
                     (qq-chat--yank-media "application/octet-stream" "bytes" t))
               (cl-letf (((symbol-function 'qq-core-send-message)
                          (lambda (_session _segments
                                            &optional _raw _callback errback)
                            (setq failed errback))))
                 (qq-chat-send-message))
               (should (file-exists-p file))
               (pcase teardown
                 ('kill (kill-buffer buffer))
                 ('mode (fundamental-mode))
                 ('reset (qq-reset-session-state))))
             (should-not (file-exists-p file))
             (cl-letf (((symbol-function 'qq-chat--restore-failed-send)
                        (lambda (&rest _args)
                          (ert-fail "Retired clipboard owner must not restore"))))
               (funcall failed nil "late staging failure")))
         (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest qq-chat-clipboard-adoption-error-deletes-created-source ()
  (let ((file (qq-chat--write-binary-temp-file "bytes" ".bin")))
    (unwind-protect
        (cl-letf (((symbol-function 'qq-chat-attach-file)
                   (lambda (&rest _args) (error "Composer unavailable"))))
          (should-error (qq-chat--attach-clipboard-file file "file"))
          (should-not (file-exists-p file)))
      (ignore-errors (delete-file file)))))

(ert-deftest qq-chat-image-object-label-uses-one-line-preview-and-file-metadata ()
  (let ((path (make-temp-file "qq-composer-preview" nil ".png")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "123456"))
          (cl-letf (((symbol-function
                      'qq-media--one-line-preview-image-from-file)
                     (lambda (file)
                       (should (equal file path))
                       '(:composer-preview)))
                    ((symbol-function
                      'appkit-media-one-line-image-display-string)
                     (lambda (image fallback)
                       (propertize fallback 'display image))))
            (let ((label
                   (qq-chat--segment-object-label
                    `((type . "image")
                      (data . ((file . ,path) (name . "preview.png")))))))
              (should (string-match-p "\\[image\\]" label))
              (should (string-match-p "preview.png" label))
              (should (string-match-p "(6)" label))
              (should (equal '(:composer-preview)
                             (get-text-property
                              (string-match "▧" label) 'display label))))))
      (ignore-errors (delete-file path)))))

(ert-deftest qq-chat-equal-adjacent-input-objects-send-as-two-segments ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt "> ")
    (let* ((segment '((type . "at")
                      (data . ((qq . "10001") (name . "Alice Card")))))
           (object (qq-chat--segment-input-object segment))
           (label (plist-get object :label)))
      (insert (appkit-chatbuf-input-object-string label object))
      (insert (appkit-chatbuf-input-object-string label object))
      (should (equal (list segment segment)
                     (qq-chat--current-input-segments))))))

(ert-deftest qq-chat-segment-insertion-inside-object-keeps-both-atomic ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((first '((type . "at")
                    (data . ((qq . "10001") (name . "Alice Card")))))
           (second '((type . "face") (data . ((id . "178"))))))
       (qq-chat--insert-input-segment-object first)
       (goto-char (1+ (appkit-chatbuf-input-start-position)))
       (qq-chat--insert-input-segment-object second)
       (appkit-chatbuf-input-prune-broken-objects)
       (should (equal (list first second)
                      (qq-chat--current-input-segments)))))))

(ert-deftest qq-chat-attach-face-inserts-face-segment ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let ((qq-media--face-names-table (make-hash-table :test #'equal)))
     (puthash "178" "/斜眼笑" qq-media--face-names-table)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (qq-chat-attach-face "178")
       (should (appkit-chatbuf-input-has-objects-p))
       (let ((segments (qq-chat--current-input-segments)))
         (should (= 1 (length segments)))
         (should (equal "face" (alist-get 'type (car segments))))
         (should (equal "178"
                        (alist-get 'id (alist-get 'data (car segments)))))
         (should (equal "basic"
                        (alist-get 'face_type
                                   (alist-get 'data (car segments))))))))))

(defmacro qq-chat-test-with-favorite-composer (&rest body)
  "Run BODY in a fresh favorite-face composer."
  (declare (indent 0) (debug t))
  `(qq-chat-test-with-reset
     (qq-state-upsert-session
      "private:10001"
      '((type . private) (title . "Alice") (target-id . "10001")) nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       ,@body)))

(defun qq-chat-test--interact-with-picker (table interact)
  "Emulate minibuffer entry for TABLE, then perform user INTERACT.
Keep the real shared reader, its setup hook, and its lifetime cleanup."
  (with-temp-buffer
    (let ((minibuffer-completion-table table)
          (minibuffer-completion-predicate nil))
      (unwind-protect
          (progn
            (run-hooks 'minibuffer-setup-hook)
            (funcall interact table))
        (run-hooks 'minibuffer-exit-hook)))))

(defun qq-chat-test--picker-status ()
  "Return non-input text displayed by the current picker's overlays."
  (mapconcat
   (lambda (overlay)
     (concat (overlay-get overlay 'before-string)
             (overlay-get overlay 'after-string)))
   (let ((overlays (overlay-lists)))
     (append (car overlays) (cdr overlays)))
   ""))

(defun qq-chat-test--deliver-favorites (chat &rest fields)
  "Install favorite catalog FIELDS and notify CHAT as the App would."
  (apply #'qq-operation-test--seed '(favorite-emojis) fields)
  (with-current-buffer chat
    (run-hook-with-args 'qq-runtime-data-hook '(favorite-emojis))))

(ert-deftest qq-chat-attach-custom-face-picker-fills-when-catalog-arrives ()
  (qq-chat-test-with-favorite-composer
    (let* ((chat (current-buffer))
           (favorite-id "10001_0_0_0_DEADBEEFDEADBEEFDEADBEEFDEADBEEF_0_0")
           (face `((favorite_emoji_id . ,favorite-id)
                   (md5 . "deadbeefdeadbeefdeadbeefdeadbeef")))
           requests)
      (cl-letf (((symbol-function 'qq-runtime-request)
                 (lambda (request &rest _) (push request requests)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (qq-chat-test--interact-with-picker
                    table
                    (lambda (live-table)
                      ;; The picker opens at once and loads in place.
                      (should-not (all-completions "" live-table))
                      (should-not (string-empty-p (qq-chat-test--picker-status)))
                      (insert "deadbeef")
                      (qq-chat-test--deliver-favorites chat :value `((entries ,face)))
                      (should (equal "deadbeef" (buffer-string)))
                      (car (all-completions "" live-table)))))))
        (qq-chat-attach-custom-face)
        (should (equal requests '((favorite-emojis))))
        (should (equal (qq-chat--current-input-segments)
                       `(((type . "favorite_emoji")
                          (data . ((favorite_emoji_id . ,favorite-id)))))))))))

(ert-deftest qq-chat-favorite-picker-empty-and-error-stay-nonselectable ()
  (dolist (outcome '(empty error))
    (qq-chat-test-with-favorite-composer
      (qq-chat--set-draft "Keep my draft")
      (let ((chat (current-buffer)))
        (cl-letf (((symbol-function 'qq-runtime-request) #'ignore)
                  ((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _)
                     (qq-chat-test--interact-with-picker
                      table
                      (lambda (live-table)
                        (let ((loading (qq-chat-test--picker-status)))
                          (insert "search while loading")
                          (if (eq outcome 'empty)
                              (qq-chat-test--deliver-favorites chat :value '((entries)))
                            (qq-chat-test--deliver-favorites
                             chat :ready nil :error "catalog unavailable"))
                          (should-not (equal loading (qq-chat-test--picker-status)))
                          (should-not (all-completions "" live-table))
                          (should (equal "search while loading" (buffer-string))))
                        (signal 'quit nil))))))
          (should (eq 'quit (condition-case nil
                                (progn (qq-chat-attach-custom-face) 'returned)
                              (quit 'quit))))
          (should-not (appkit-chatbuf-input-has-objects-p))
          (should (equal "Keep my draft" (qq-chat--current-draft-string))))))))

(ert-deftest qq-chat-attach-clipboard-uri-list-local-file ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let ((path (make-temp-file "qq-clip-uri" nil ".txt")))
     (unwind-protect
         (progn
           (with-temp-file path (insert "hello clip"))
           (with-temp-buffer
             (qq-chat-mode)
             (setq qq-chat--session-key "private:10001")
             (qq-chat-render)
             (cl-letf (((symbol-function 'appkit-media-clipboard-source)
                        (lambda () (list :files (list path)))))
               (qq-chat-attach-clipboard)
               (let ((segments (qq-chat--current-input-segments)))
                 (should (= 1 (length segments)))
                 (should (equal "file" (alist-get 'type (car segments))))
                 (should (equal path
                                (alist-get 'file
                                           (alist-get 'data (car segments)))))))))
       (ignore-errors (delete-file path))))))

(ert-deftest qq-chat-attach-clipboard-as-file-prefix ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (cl-letf (((symbol-function 'appkit-media-clipboard-source)
                (lambda () (list :mime-type "image/png" :suffix ".png"
                                 :data qq-chat-test--1x1-png))))
       (qq-chat-attach-clipboard t)
       (should (equal "file"
                      (alist-get 'type
                                 (car (qq-chat--current-input-segments)))))))))

(ert-deftest qq-chat-attach-file-inserts-structured-object-and-sends-segments ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((path (make-temp-file "qq-chat-attach" nil ".txt")))
       (unwind-protect
           (let (sent-session sent-segments sent-raw)
             (qq-chat-attach-file path "file")
             (should (appkit-chatbuf-input-has-objects-p))
             (should (equal '("file")
                            (mapcar (lambda (segment) (alist-get 'type segment))
                                    (qq-chat--current-input-segments))))
             (qq-chat--set-pending-reply
              '((server-id . "42")
                (session-key . "private:10001")
                (sender-name . "Alice")
                (raw-message . "source")
                (segments . (((type . "text")
                              (data . ((text . "source"))))))))
             (cl-letf (((symbol-function 'qq-core-send-message)
                        (lambda (session-key segments &optional raw-message
                                             _callback _errback)
                          (setq sent-session session-key)
                          (setq sent-segments segments)
                          (setq sent-raw raw-message))))
               (qq-chat-send-message))
             (should (equal "private:10001" sent-session))
             (should-not sent-raw)
             (should (equal "reply" (alist-get 'type (nth 0 sent-segments))))
             (should (equal "42"
                            (alist-get
                             'message_id
                             (alist-get
                              'target
                              (alist-get 'data (nth 0 sent-segments))))))
             (should (equal "file" (alist-get 'type (nth 1 sent-segments))))
             (should (equal path
                            (alist-get 'file (alist-get 'data (nth 1 sent-segments)))))
             (should (equal (file-name-nondirectory path)
                            (alist-get 'name (alist-get 'data (nth 1 sent-segments))))))
         (ignore-errors (delete-file path)))))))

(ert-deftest qq-chat-attach-file-rejects-directories ()
  (let ((directory (make-temp-file "qq-chat-attach-directory-" t)))
    (unwind-protect
        (should-error (qq-chat-attach-file directory)
                      :type 'user-error)
      (delete-directory directory))))

(ert-deftest qq-chat-file-prefix-stages-image-as-document-without-sending ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((path (make-temp-file "qq-prefix-" nil ".png")))
       (unwind-protect
           (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) path))
                     ((symbol-function 'qq-core-send-message)
                      (lambda (&rest _) (ert-fail "Attaching must not send"))))
             (call-interactively #'qq-chat-attach-file)
             (should (equal "image" (alist-get 'type
                                             (car (qq-chat--current-input-segments)))))
             (qq-chat--set-draft "")
             (let ((current-prefix-arg '(4)))
               (call-interactively #'qq-chat-attach-file))
             (let ((segment (car (qq-chat--current-input-segments))))
               (should (equal "file" (alist-get 'type segment)))
               (should (equal path (alist-get 'file (alist-get 'data segment)))))
             (should (file-exists-p path)))
         (delete-file path))))))

(ert-deftest qq-chat-send-failure-restores-rich-draft-and-reply ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let* ((reply '((server-id . "9007199254742007094")
                     (session-key . "private:10001")
                     (sender-name . "Alice")
                     (raw-message . "source")))
            error-fn)
       (qq-chat--insert-input-segment-object
        '((type . "at")
          (data . ((qq . "10001") (name . "Alice Card")))))
       (qq-chat--set-reply-message reply)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session _segments &optional _raw _callback errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (should (equal "" (appkit-chatbuf-input-string)))
         (should-not (appkit-chatbuf-aux-state))
         (funcall error-fn nil "network failed"))
       (should (appkit-chatbuf-input-has-objects-p))
       (should (equal '("at")
                      (mapcar (lambda (segment) (alist-get 'type segment))
                              (qq-chat--current-input-segments))))
       (should (equal "9007199254742007094"
                      (alist-get 'server-id (qq-chat--reply-message))))))))

(ert-deftest qq-chat-synchronous-send-error-restores-rich-draft-and-reply ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((reply '((server-id . "9007199254742007094")
                    (session-key . "private:10001")
                    (raw-message . "source"))))
       (qq-chat--insert-input-segment-object
        '((type . "at")
          (data . ((qq . "10001") (name . "Alice Card")))))
       (qq-chat--set-reply-message reply)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (&rest _args) (error "transport exploded"))))
         (should-error (qq-chat-send-message) :type 'error))
       (should (appkit-chatbuf-input-has-objects-p))
       (should (equal "9007199254742007094"
                      (alist-get 'server-id (qq-chat--reply-message))))))))

(ert-deftest qq-chat-frame-error-during-send-restores-canonical-state ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "keep me")
     (qq-chat--set-reply-message
      '((server-id . "9007199254742007094")
        (session-key . "private:10001")
        (raw-message . "source")))
     (let ((updates 0))
       (cl-letf (((symbol-function 'qq-chat--update-frame)
                  (lambda ()
                    (cl-incf updates)
                    (when (= updates 1)
                      (error "frame exploded"))))
                 ((symbol-function 'qq-core-send-message)
                  (lambda (&rest _args)
                    (ert-fail "frame error must happen before API send"))))
         (should-error (qq-chat-send-message) :type 'error))
       (should (= updates 2)))
     (should (equal "keep me" (qq-chat--current-draft-string)))
     (should (equal "9007199254742007094"
                    (alist-get 'server-id (qq-chat--reply-message)))))))

(ert-deftest qq-chat-send-rejects-numeric-reply-id-before-clearing-draft ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "keep me")
     (qq-chat--set-reply-message
      '((server-id . 42)
        (session-key . "private:10001")
        (raw-message . "source")))
     (cl-letf (((symbol-function 'qq-core-send-message)
                (lambda (&rest _args)
                  (ert-fail "invalid reply id must fail before API send"))))
       (should-error (qq-chat-send-message) :type 'user-error))
     (should (equal "keep me" (qq-chat--current-draft-string)))
     (should (= 42 (alist-get 'server-id (qq-chat--reply-message)))))))

(ert-deftest qq-chat-stale-send-failure-never-overwrites-new-draft ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "old draft")
     (let (error-fn)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session _segments &optional _raw _callback errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (insert "new draft")
         (funcall error-fn nil "late failure"))
       (should (equal "new draft" (appkit-chatbuf-input-string)))
       (should (equal "new draft" (qq-chat--current-draft-string)))))))

(ert-deftest qq-chat-stale-send-failure-never-overwrites-new-object ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "old draft")
     (let (error-fn)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session _segments &optional _raw _callback errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (qq-chat--insert-input-segment-object
          '((type . "face") (data . ((id . "178")))))
         (funcall error-fn nil "late failure"))
       (should (equal '("face")
                      (mapcar (lambda (segment) (alist-get 'type segment))
                              (qq-chat--current-input-segments))))))))

(ert-deftest qq-chat-stale-send-failure-never-overwrites-new-reply ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "old draft")
     (let ((new-reply '((server-id . "9007199254742007095")
                        (session-key . "private:10001")
                        (raw-message . "new source")))
           error-fn)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session _segments &optional _raw _callback errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (qq-chat--set-reply-message new-reply)
         (funcall error-fn nil "late failure"))
       (should (equal "" (appkit-chatbuf-input-string)))
       (should (equal "9007199254742007095"
                      (alist-get 'server-id (qq-chat--reply-message))))))))

(ert-deftest qq-chat-send-success-settles-late-error-callback ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "sent draft")
     (let (success-fn error-fn)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session _segments &optional _raw success errback)
                    (setq success-fn success
                          error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (funcall success-fn nil)
         (funcall error-fn nil "late failure"))
       (should (equal "" (qq-chat--current-draft-string)))
       (should-not (appkit-chatbuf-aux-state))))))

(ert-deftest qq-chat-failed-send-revision-prevents-empty-state-aba ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "old draft")
     (let (error-fn cleared-revision)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session _segments &optional _raw _success errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (setq cleared-revision (appkit-chatbuf-composer-revision))
         (insert "new")
         (delete-region (appkit-chatbuf-input-start-position) (point-max))
         (should (> (appkit-chatbuf-composer-revision) cleared-revision))
         (funcall error-fn nil "late failure"))
       (should (equal "" (qq-chat--current-draft-string)))
       (should-not (appkit-chatbuf-aux-state))))))

(ert-deftest qq-chat-private-reply-resolves-origseq-as-client-sequence ()
  "Private SourceMsg OrigSeq must not be used as a history Message Sequence."
  (qq-chat-test-with-reset
   (let* ((session-key "private:10001")
          (source
           `((id . "100")
             (server-id . "100")
             (session-key . ,session-key)
             (message-seq . "42108")
             (native-client-sequence . "47705")
             (message-type . "private")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 1710000100)
             (raw-message . "quoted text")
             (preview . "quoted text")
             (segments . (((type . "text")
                           (data . ((text . "quoted text"))))))))
          (reply
           `((id . "200")
             (server-id . "200")
             (session-key . ,session-key)
             (message-seq . "42112")
             (native-client-sequence . "60924")
             (message-type . "private")
             (sender-id . "10002")
             (sender-name . "Bob")
             (time . 1710000200)
             (raw-message . "reply body")
             (preview . "reply body")
             (segments . (((type . "reply")
                           (data . ((message_seq . "47705"))))
                          ((type . "text")
                           (data . ((text . "reply body")))))))))
     (qq-state-upsert-session
      session-key
      '((type . private) (title . "Alice") (target-id . "10001"))
      nil)
     (puthash session-key (list source reply) qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key)
       (qq-chat-test--window "100" nil)
       (should (equal "100" (qq-chat--message-reply-id reply)))
       (should (qq-message-history-locator-matches-p
                (qq-chat--message-reply-locator reply) source))
       (qq-chat-render)
       (goto-char (qq-chat--message-position "200"))
       (qq-chat-goto-reply)
       (should
        (equal "100"
               (get-text-property (point) 'qq-chat-message-anchor)))))))

(ert-deftest qq-chat-reply-dependencies-follow-current-conversation-history ()
  "Resolve off-window sources without confusing sequence domains or stale IDs."
  (dolist (kind '(group private))
    (qq-chat-test-with-reset
     (let* ((qq-chat--session-key (format "%s:20001" kind))
            (field (if (eq kind 'private)
                       'native-client-sequence
                     'message-seq))
            (other-field (if (eq kind 'private)
                             'message-seq
                           'native-client-sequence))
            (source (list (cons 'server-id "100") (cons field "7")
                          (cons 'sender-name "Alice") (cons 'preview "Original")))
            (duplicate `((server-id . "101") (,field . "7")))
            (decoy `((server-id . "90") (,other-field . "7")))
            (reply
             '((id . "200") (server-id . "200") (sender-name . "Bob")
               (segments . (((type . "reply")
                             (data . ((message_seq . "7") (id . "900")))))))))
       (cl-labels
           ((dependencies ()
              (seq-filter
               (lambda (key) (eq (car key) :message))
               (appkit-projection-row-dependencies
                (car (qq-chat--project-timeline (list reply)))))))
         ;; Only the reply is visible; its source is elsewhere in cached history.
         (puthash qq-chat--session-key (list decoy source reply)
                  qq-state--messages-by-session)
         (should (equal (dependencies) '((:message "100"))))
         (puthash qq-chat--session-key (list decoy source duplicate reply) qq-state--messages-by-session)
         (should-not (dependencies))
         (puthash qq-chat--session-key (list decoy source reply) qq-state--messages-by-session)
         ;; A later projection must observe changed identities and removals.
         (setf (alist-get 'server-id source) "102")
         (should (equal (dependencies) '((:message "102"))))
         (puthash qq-chat--session-key (list decoy reply)
                  qq-state--messages-by-session)
         ;; Neither the wrong sequence domain nor the numeric reserve is an ID.
         (should-not (dependencies))
         (puthash qq-chat--session-key (list source reply)
                  qq-state--messages-by-session)
         (should (equal (dependencies) '((:message "102"))))
         (let ((qq-chat--session-key (format "%s:20002" kind)))
           (should-not (dependencies))))))))

(ert-deftest qq-chat-history-merge-preserves-retained-rows-and-summary-order ()
  (qq-chat-test-with-reset
    (let* ((early (qq-chat-test--canonical-message "100" 10 "old"))
           (latest (qq-chat-test--canonical-message "300" 30 "latest" 1))
           (tie (qq-chat-test--canonical-message "200" 30 "earlier sequence" 2))
           (retained (list early latest tie))
           (snapshot (copy-tree retained)))
      (puthash "group:20001" retained qq-state--messages-by-session)
      (qq-state--merge-normalized-message
       "group:20001" (qq-chat-test--canonical-message "50" 5 "older") nil 'history)
      (qq-state--merge-normalized-message
       "group:20001" (qq-chat-test--canonical-message "100" 10 "updated") nil 'history)
      (should (equal retained snapshot))
      (should (equal (mapcar (lambda (m) (alist-get 'server-id m))
                            (qq-state-session-messages "group:20001"))
                     '("50" "100" "300" "200")))
      ;; The same-second summary follows native sequence, not arrival order.
      (should (equal (alist-get 'last-message-id (qq-state-session "group:20001"))
                     "300"))
      (should (equal (alist-get 'preview
                               (cadr (qq-state-session-messages "group:20001")))
                     "updated")))))

(ert-deftest qq-chat-quote-redraw-reuses-context-and-observes-source-updates ()
  (qq-chat-test-with-reset
    (let* ((source (qq-chat-test--canonical-message "100" 10 "Original quotation"))
           (reply (qq-chat-test--canonical-message "200" 20 "Reply body")))
      (push '((type . "reply") (data . ((message_seq . "100"))))
            (alist-get 'segments reply))
      (puthash "group:20001" (list source reply) qq-state--messages-by-session)
      (with-temp-buffer
        (qq-chat-mode)
        (setq qq-chat--session-key "group:20001")
        (qq-chat-test--window "200" nil)
        (qq-chat-render)
        (qq-chat--set-draft "Keep this draft")
        (goto-char (point-min))
        (let ((printer (symbol-function 'qq-chat--row-printer))
              (prints 0))
          (cl-letf (((symbol-function 'qq-state-session-messages)
                     (lambda (&rest _) (ert-fail "Redraw copied the conversation")))
                    ((symbol-function 'qq-chat--row-printer)
                     (lambda (row) (cl-incf prints) (funcall printer row))))
            (qq-chat--request-row-redisplay '("200")))
          (should (= prints 1)))
        (should (string-match-p "Original quotation" (buffer-string)))
        (qq-state--merge-normalized-message
         "group:20001"
         (qq-chat-test--canonical-message "100" 10 "Updated quotation") nil 'history)
        (qq-chat--sync-timeline :changed-resources '((:message "100")))
        (should (string-match-p "Updated quotation" (buffer-string)))
        (should-not (string-match-p "Original quotation" (buffer-string)))
        (should (equal (qq-chat--current-draft-string) "Keep this draft"))))))

(ert-deftest qq-chat-goto-reply-jumps-to-sequence-only-history-anchor ()
  (qq-chat-test-with-reset
   (let* ((session-key "group:20001")
          (history-anchor "history:slot-a:group:20001:100:none")
          (source
           `((id . ,history-anchor)
             (server-id)
             (session-key . ,session-key)
             (message-seq . "100")
             (message-type . "group")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 1710000100)
             (raw-message . "source")
             (preview . "source")
             (segments . (((type . "text")
                           (data . ((text . "source"))))))))
          (reply
           `((id . "200")
             (server-id . "200")
             (session-key . ,session-key)
             (message-seq . "101")
             (message-type . "group")
             (sender-id . "10002")
             (sender-name . "Bob")
             (time . 1710000200)
             (raw-message . "reply body")
             (preview . "reply body")
             (segments . (((type . "reply")
                           (data . ((message_seq . "100"))))
                          ((type . "text")
                           (data . ((text . "reply body")))))))))
     (qq-state-upsert-session
      session-key
      '((type . group) (title . "Group") (target-id . "20001"))
      nil)
     (puthash session-key (list source reply) qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key)
       (qq-chat-test--window history-anchor "200")
       (qq-chat-render)
       (goto-char (qq-chat--message-position "200"))
       (qq-chat-goto-reply)
       (should
        (equal history-anchor
               (get-text-property (point) 'qq-chat-message-anchor)))))))

(ert-deftest qq-chat-remote-reply-preview-renders-exact-row-and-jumps-back ()
  (dolist (kind '(group private))
    (qq-chat-test-with-history-metadata
     (let* ((session-key (format "%s:20001" kind))
            (anchor (if (eq kind 'group)
                        "timeline:slot-a:group:20001:42" "100"))
            (source (copy-tree (qq-chat-test--canonical-message
                                "100" 100 "old quoted source")))
            (reply (copy-tree (qq-chat-test--canonical-message
                               "200" 200 "new reply")))
            (qq-auto-mark-read nil))
       (setf (alist-get 'id source) anchor
             (alist-get 'server-id source) (and (eq kind 'private) "100")
             (alist-get 'session-key source) session-key
             (alist-get 'session-key reply) session-key
             (alist-get 'native-client-sequence source) "77"
             (alist-get 'canonical-row-key source) "42"
             (alist-get 'canonical-row-key reply) "43"
             (alist-get 'segments reply)
             `(((type . "reply")
                (data . ,(if (eq kind 'private)
                             '((message_seq . "77") (sender . ((uin . "10001")))
                               (sent_at . 100))
                           '((message_seq . "100")))))
               ((type . "text") (data . ((text . "new reply"))))))
       (qq-state-upsert-session
        session-key `((type . ,kind) (title . "Conversation")
                      (target-id . "20001")) nil)
       (puthash session-key (list reply) qq-state--messages-by-session)
       (with-temp-buffer
         (qq-chat-mode)
         (setq qq-chat--session-key session-key)
         (qq-chat-test--window "200" nil)
         (qq-chat-render)
         (goto-char (text-property-any
                     (point-min) (point-max) 'qq-chat-reply-button t))
         (qq-chat-test-with-pending-gateway
           (funcall (get-text-property (point) 'action) nil)
           (qq-chat-test--drain-history)
           (should (qq-chat--loading))
           ;; The target was not cached, and group history may have no QQ ID.
           (puthash session-key (list source reply) qq-state--messages-by-session)
           (qq-chat-test--deliver-timeline (list (cons "42" anchor) (cons "43" "200")) anchor)
           (should-not qq-chat--pending-jump-center)
           (should-not (qq-chat--loading))
           (should (equal (get-text-property (point) 'qq-chat-message-anchor)
                          anchor))
           (qq-chat-goto-pop-message)
           (should (equal (get-text-property (point) 'qq-chat-message-anchor)
                          "200"))))))))

(ert-deftest qq-chat-read-all-jumps-to-latest-without-paging-through-history ()
  (dolist (live-window '(nil t))
    (qq-chat-test-with-history-metadata
     (let* ((session-key "group:20001")
            (old (qq-chat-test--canonical-message "100" 100 "old message"))
            (latest (qq-chat-test--canonical-message "200" 200 "latest message"))
            (qq-auto-mark-read nil)
            calls)
       (qq-state-upsert-session
        session-key '((type . group) (title . "Group") (target-id . "20001")
                      (first-unread-message-id . "100")
                      (first-unread-message-seq . "100")) nil)
       (puthash session-key (if live-window (list old latest) (list old))
                qq-state--messages-by-session)
       (with-temp-buffer
         (qq-chat-mode)
         (setq qq-chat--session-key session-key)
         (qq-chat-test--model-put :latest (and live-window "200"))
         (qq-chat-test--window "100" (unless live-window "100"))
         (qq-chat-render)
         (qq-chat--set-draft "keep this draft")
         (should (qq-chat--goto-loaded-message "100"))
         (cl-letf (((symbol-function 'qq-server-gateway-instance-id) (lambda () "gateway-a"))
                   ((symbol-function 'qq-server-cancel) #'ignore)
                   ((symbol-function 'qq-rpc-call)
                    (lambda (method _params &rest options)
                      (push (cons method options) calls)
                      (make-symbol "transport"))))
           (qq-chat-read-all)
           (qq-chat-test--drain-history)
           (let ((call (assoc "conversation.mark_read" calls)))
             ;; Delivery may occur while an unrelated buffer is current.
             (with-temp-buffer
               (funcall (plist-get (cdr call) :callback)
                        `((account_id . "slot-a")
                          (conversation . ,(qq-message--conversation-params session-key))
                          (read_sequence . "200")))))
           (qq-chat-test--drain-history)
           ;; Only a timeline that does not reach latest is reopened.
           (should (= (seq-count (lambda (call) (equal (car call) "timeline.open")) calls)
                      (if live-window 0 1)))
           (unless live-window
             (puthash session-key (list old latest) qq-state--messages-by-session)
             (qq-chat-test--deliver-timeline (list (cons "100" "100") (cons "200" "200"))))
           (should-not qq-chat--pending-jump-center)
           (should-not (qq-chat--loading))
           (should-not (plist-get (qq-chat--model) :has-newer))
           (should (equal (get-text-property (point) 'qq-chat-message-anchor) "200"))
           (should-not (qq-chat--first-unread-anchor (list old latest)))
           (should-not (text-property-any (point-min) (point-max)
                                          'appkit-chat-read-divider t))
           (should (equal (qq-chat--current-draft-string) "keep this draft"))))))))

(ert-deftest qq-chat-replies-to-sequence-only-group-history ()
  (qq-chat-test-with-reset
   (let* ((session-key "group:20001")
          (message
           `((id . "history:slot-a:group:20001:100:none")
             (server-id)
             (session-key . ,session-key)
             (message-seq . "100")
             (message-type . "group")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 1710000100)
             (raw-message . "source")
             (preview . "source")
             (segments . (((type . "text")
                           (data . ((text . "source")))))))))
     (qq-state-upsert-session
      session-key
      '((type . group) (title . "Group") (target-id . "20001"))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key)
       (qq-chat-render)
       (qq-chat--set-pending-reply message)
       (should (eq (plist-get (appkit-chatbuf-aux-state) :aux-type) 'reply))
       (should (eq (plist-get (appkit-chatbuf-aux-state) :aux-msg) message))
       (should-not (plist-get (appkit-chatbuf-aux-state) :message-id))
       (let ((card (qq-chat--reply-context-text)))
         (should
          (equal "× ▏ Reply to Alice\n  ▏ source\n"
                 (substring-no-properties card)))
         (should
          (eq (get-text-property 0 appkit-ui-action-property card)
              #'qq-chat-cancel-reply)))))))

(ert-deftest qq-chat-evil-return-cancels-reply-without-changing-draft ()
  (skip-unless (require 'evil nil t))
  (qq-evil-setup)
  (save-window-excursion
    (qq-chat-test-with-reset
      (qq-state-upsert-session
       "group:20001"
       '((type . group) (title . "Group") (target-id . "20001")) nil)
      (with-temp-buffer
        (qq-chat-mode)
        (setq qq-chat--session-key "group:20001")
        (switch-to-buffer (current-buffer))
        (qq-chat-render)
        (qq-chat--set-draft "Keep my draft")
        (evil-local-mode 1)
        (dolist (state '(normal motion insert emacs))
          (dolist (key '("RET" "<return>"))
            (qq-chat--set-pending-reply
             '((id . "100") (server-id . "100")
               (session-key . "group:20001") (message-type . "group")
               (message-seq . "100") (sender-id . "10001")
               (sender-name . "Alice") (preview . "Reply source")
               (segments . (((type . "text")
                             (data . ((text . "Reply source"))))))))
            (evil-change-state state)
            (goto-char
             (text-property-any (point-min) (point-max)
                                appkit-ui-action-property
                                #'qq-chat-cancel-reply))
            (execute-kbd-macro (kbd key))
            (should-not (qq-chat--reply-message))
            (should-not (appkit-chatbuf-aux-active-p))
            (should (equal "Keep my draft" (qq-chat--current-draft-string)))
            (should (eq state evil-state))))))))

(ert-deftest qq-chat-message-reply-id-from-segments ()
  (should
   (equal "42"
          (qq-chat--message-reply-id
           (list (cons (quote segments)
                       (list (list (cons (quote type) "reply")
                                   (cons (quote data)
                                         (list (cons (quote id) "42"))))
                             (list (cons (quote type) "text")
                                   (cons (quote data)
                                         (list (cons (quote text) "hi"))))))))))
  (should-not
   (qq-chat--message-reply-id
    (list (cons (quote segments)
                (list (list (cons (quote type) "text")
                            (cons (quote data)
                                  (list (cons (quote text) "hi"))))))))))

(ert-deftest qq-chat-input-segments-keep-cjk-text-after-image-object ()
  "Image object must not swallow following Chinese when sending.

Regression: object text-properties were rear-sticky, so CJK typed after an
attachment inherited `appkit-chatbuf-input-object' and was dropped on parse."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    (list (cons (quote title) "Alice")
          (cons (quote target-id) "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (let* ((path (make-temp-file "qq-img" nil ".png"))
            (segments nil))
       (unwind-protect
           (progn
             (with-temp-file path (insert "x"))
             (qq-chat-attach-file path "image")
             ;; Type CJK after the attachment object (normal user flow).
             (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
             (insert "你好世界")
             (qq-chat--sync-draft-from-buffer)
             (setq segments (qq-chat--current-input-segments))
             (should (equal '("image" "text")
                            (mapcar (lambda (s) (alist-get (quote type) s))
                                    segments)))
             (should (equal "你好世界"
                            (alist-get (quote text)
                                       (alist-get (quote data)
                                                  (nth 1 segments))))))
         (ignore-errors (delete-file path)))))))

(ert-deftest qq-chat-media-preview-uses-qq-opener-not-browser-url ()
  "A compact QQ card uses shared context and no inline action toolbar."
  (let* ((segment '((type . "image")
                    (data . ((url . "https://example.com/picture.gif")
                             (name . "picture.gif")))))
         preview-url
         opened-segment)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-segment-download-state)
                   (lambda (_segment)
                     '(:status not-downloaded :path "/tmp/picture.gif")))
                  ((symbol-function 'qq-media-segment-preview-capable-p)
                   (lambda (_segment) t))
                  ((symbol-function 'qq-media-segment-preview-image)
                   (lambda (_segment) '(image :type png :data "")))
                  ((symbol-function 'qq-media-segment-preview-fetching-p)
                   (lambda (_segment) nil))
                  ((symbol-function 'appkit-media-insert-image-slices)
                   (lambda (_image url &optional _prefix _fallback)
                     (setq preview-url url)
                     (insert "PREVIEW")))
                  ((symbol-function 'qq-media-segment-open)
                   (lambda (media-segment &rest _keys)
                     (setq opened-segment media-segment))))
          (qq-chat--insert-segment-media-line segment nil nil)
          (should-not preview-url)
          (should (string-match-p (regexp-quote "[image] picture.gif")
                                  (buffer-string)))
          (dolist (old-action '("[Open]" "[Play]" "[Copy URL]"
                                "[Download]" "[Save As]" "transfer:"))
            (should-not (string-match-p (regexp-quote old-action)
                                        (buffer-string))))
          (goto-char (point-min))
          (search-forward "PREVIEW")
          (let ((context (appkit-media-card-context-at-point)))
            (should (equal (plist-get context :payload) segment))
            (should (functionp (plist-get context :download-action)))
            (should (functionp (plist-get context :save-as-action)))
            (should (functionp (plist-get context :copy-url-action)))
            (appkit-media-card-call-action 'open context))
          (should (equal opened-segment segment)))))))

(ert-deftest qq-chat-native-record-meta-shows-closed-duration ()
  (should
   (equal
    (qq-chat--segment-media-meta-line
     '((type . "record") (data . ((duration_seconds . 65)))))
    "1:05"))
  (should
   (equal
    (qq-chat--segment-media-meta-line
     '((type . "record") (data . ((duration_seconds . 0)))))
    "")))

(ert-deftest qq-chat-native-record-adapts-state-to-appkit-voice-note ()
  (let* ((segment
          '((type . "record")
            (data
             . ((media_id
                 . "media-11223344-5566-7788-99aa-bbccddeeff00")
                (duration_seconds . 17)
                (summary . "[语音]")))))
         (action #'ignore)
         control)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-segment-capabilities)
                   (lambda (_segment)
                     '(:open t :status "Playing"
                       :remote-status "materialized")))
                  ((symbol-function 'qq-chat--segment-media-card-context)
                   (lambda (_segment &optional _capabilities _host-cell)
                     (list :open-action action)))
                  ((symbol-function 'qq-media-native-record-playback-state)
                   (lambda (_segment)
                     '(:status playing :duration-seconds 17
                       :played-seconds 4)))
                  ((symbol-function 'appkit-chat-ins-insert-voice-note)
                   (lambda (&rest keys)
                     (setq control keys)
                     (insert "VOICE-CONTROL\n"))))
          (qq-chat--insert-segment-media-line segment nil nil)
          (should (string-match-p "VOICE-CONTROL" (buffer-string)))
          (should (eq (plist-get control :state) 'playing))
          (should (= (plist-get control :duration-seconds) 17))
          (should (= (plist-get control :played-seconds) 4))
          (should (eq (plist-get control :action) action))
          ;; The Appkit control owns the duration; the card header no longer
          ;; duplicates it as a QQ-specific detail.
          (should-not (string-match-p "(0:17)" (buffer-string))))))))

(ert-deftest qq-chat-video-preview-keeps-video-alt-text ()
  (let ((segment '((type . "video")
                   (data . ((name . "short.mp4")
                            (url . "https://example.com/short.mp4")
                            (remote_status . "available")))))
        fallback)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-segment-preview-capable-p)
                   (lambda (_segment) t))
                  ((symbol-function 'qq-media-segment-preview-image)
                   (lambda (_segment) '(image :type png :data "")))
                  ((symbol-function 'qq-media-segment-preview-fetching-p)
                   (lambda (_segment) nil))
                  ((symbol-function 'appkit-media-insert-image-slices)
                   (lambda (_image _action &optional _prefix alt-text)
                     (setq fallback alt-text)
                     (insert "VIDEO-PREVIEW"))))
          (qq-chat--insert-segment-media-line segment nil nil)
          (should (equal fallback "[video]")))))))

(ert-deftest qq-chat-renders-files-as-named-media-cards ()
  (dolist (type '("file" "group_file"))
    (let ((message
           `((segments
              . (((type . ,type)
                  (data . ((file_name . "report.pdf")
                           (file_size . "1572864")))))))))
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (cl-letf (((symbol-function 'qq-media-segment-capabilities)
                     (lambda (_segment) nil)))
            (qq-chat--insert-message-body message nil nil)
            (should (string-match-p "report\\.pdf" (buffer-string)))
            (should (string-match-p "1\\.5 MB" (buffer-string)))
            (when (equal type "group_file")
              (should-not (string-match-p "\\[group_file\\]"
                                          (buffer-string))))))))))

(ert-deftest qq-chat-media-card-context-targets-exact-segment-at-point ()
  "Shared card context keeps multi-segment QQ messages unambiguous."
  (let ((first '((type . "image")
                 (data . ((name . "first.png")
                          (url . "https://example.com/first.png")))))
        (second '((type . "video")
                  (data . ((name . "second.mp4")
                           (url . "https://example.com/second.mp4"))))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-segment-download-state)
                   (lambda (_segment)
                     '(:status not-downloaded :path "/tmp/media")))
                  ((symbol-function 'qq-media-segment-preview-capable-p)
                   (lambda (_segment) nil)))
          (qq-chat--insert-segment-media-line first nil nil)
          (qq-chat--insert-segment-media-line second nil nil)
          (goto-char (point-min))
          (search-forward "second.mp4")
          (should
           (equal second
                  (plist-get (appkit-media-card-context-at-point) :payload))))))))

(ert-deftest qq-chat-renders-structured-mail-segment ()
  (let ((segment '((type . "mail")
                   (data . ((sender . "Henrik Lissner")
                            (subject . "Re: Doom Emacs")
                            (content . "Closed the issue as completed.")
                            (detail . "邮件详情")
                            (url . "https://mail.qq.com/example")))))
        opened-url)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &optional _new-window)
                     (setq opened-url url))))
          (qq-chat--insert-mail-segment segment nil nil)
          (should (string-match-p "Mail · Henrik Lissner" (buffer-string)))
          (should (string-match-p "Re: Doom Emacs" (buffer-string)))
          (should (string-match-p "Closed the issue as completed" (buffer-string)))
          (should-not (string-match-p "com.tencent.template.public" (buffer-string)))
          (goto-char (point-min))
          (search-forward "邮件详情")
          (button-activate (button-at (1- (point))))
          (should (equal opened-url "https://mail.qq.com/example")))))))

(ert-deftest qq-chat-native-link-preview-renders-actionable-card ()
  (let* ((url "https://www.cambridgeenglish.org/test-your-english/")
         (wire `((kind . "text")
                 (payload . ((text . ,url)
                             (link_preview
                              . ((title . "测试您的英语 | 剑桥英语")
                                 (description . "测试您的英语水平")
                                 (is_default_picture . :false)))))))
         (segment (qq-message--segment-to-internal wire))
         (message `((segments . (,segment))))
         opened)
    (with-temp-buffer
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (target &rest _) (setq opened target))))
        (qq-chat--insert-message-body message nil nil)
        (should (string-match-p "测试您的英语 | 剑桥英语" (buffer-string)))
        (should (string-match-p "测试您的英语水平" (buffer-string)))
        (goto-char (point-min))
        (should (search-forward url nil t))
        (should-not (search-forward url nil t))
        (goto-char (point-min))
        (should (get-text-property (point) 'line-prefix))
        (should (button-at (point)))
        (should-not opened)
        (push-button (point))
        (should (equal opened url))))
    (should (qq-chat--message-has-block-segments-p message))
    ;; Compact/reply/copy text remains authored text, not preview metadata.
    (should (equal (qq-state-message-preview-from-segments (list segment))
                   url))))

(ert-deftest qq-chat-plain-url-remains-inline-text ()
  (let* ((url "https://example.org/")
         (segment (qq-message--segment-to-internal
                   `((kind . "text") (payload . ((text . ,url))))))
         (message `((segments . (,segment)))))
    (with-temp-buffer
      (qq-chat--insert-message-body message nil nil)
      (should (equal (buffer-string) (concat url "\n")))
      (should-not (button-at (point-min))))
    (should-not (qq-chat--message-has-block-segments-p message))))

(ert-deftest qq-chat-link-preview-falls-back-to-url-without-a-default-icon ()
  (let* ((url "https://example.org/")
         (segment
          `((type . "text")
            (data . ((text . ,url)
                     (link_preview
                      . ((icon_url . "https://example.org/default.png")
                         (is_default_picture . t))))))))
    (cl-letf (((symbol-function 'qq-media-url-preview-image)
               (lambda (&rest _) (ert-fail "Default icon must not fetch"))))
      (with-temp-buffer
        (qq-chat--insert-message-body `((segments . (,segment))) nil nil)
        (should (equal (buffer-string) (concat url "\n")))
        (should (button-at (point-min)))))
    (should-not (qq-media-link-preview-cache-key segment))))

(ert-deftest qq-chat-link-preview-rejects-unsafe-remote-locators ()
  (dolist (pair '(("javascript:alert(1)" "https://example.org/icon.png")
                  ("https:///missing-host" "https://example.org/icon.png")
                  ("https://example.org/" "file:///tmp/icon.png")
                  ("https://example.org/" "data:image/png;base64,AAAA")
                  ("https://example.org/" "https://user:pass@example.org/icon")
                  ("https://example.org/" "https://example.org/has space")))
    (let ((segment
           `((type . "text")
             (data . ((text . ,(car pair))
                      (link_preview
                       . ((title . "Unsafe preview")
                          (icon_url . ,(cadr pair))
                          (is_default_picture . :false))))))))
      (cl-letf (((symbol-function 'qq-media-url-preview-image)
                 (lambda (&rest _) (ert-fail "Unsafe icon must not fetch"))))
        (with-temp-buffer
          (qq-chat--insert-message-body `((segments . (,segment))) nil nil)
          (should (string-match-p "Unsafe preview" (buffer-string)))
          (unless (qq-media-http-url-p (car pair))
            (should-not (button-at (point-min))))))
      (should-not (qq-media-link-preview-cache-key segment)))))

(ert-deftest qq-chat-link-preview-rejects-malformed-native-metadata ()
  (dolist (preview '(((title . "Title"))
                     ((is_default_picture . "false"))
                     ((is_default_picture . :false) (icon_url . 12))
                     ((is_default_picture . :false) (title . nil))
                     ((is_default_picture . :false) (action . "execute"))))
    (should-error
     (qq-message--segment-to-internal
      `((kind . "text")
        (payload . ((text . "https://example.org/")
                    (link_preview . ,preview))))))))

(ert-deftest qq-chat-renders-normalized-ark-card-segment ()
  (let ((segment '((type . "card")
                   (data . ((kind . "share")
                            (source . "豆包")
                            (title . "和豆包的对话")
                            (content . "点击查看对话内容")
                            (url . "https://example.com/thread")))))
        opened-url)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &optional _new-window)
                     (setq opened-url url))))
          (qq-chat--insert-card-segment segment nil nil)
          (should (string-match-p "Share · 豆包" (buffer-string)))
          (should (string-match-p "和豆包的对话" (buffer-string)))
          (should (string-match-p "点击查看对话内容" (buffer-string)))
          (should-not (string-match-p "com.tencent.tuwen.lua" (buffer-string)))
          (should-not (string-match-p "\\[Open\\]" (buffer-string)))
          (goto-char (point-min))
          (should (button-at (point)))
          (push-button (point))
          (should (equal opened-url "https://example.com/thread")))))))

(ert-deftest qq-chat-renders-card-preview-image-without-an-open-label ()
  (let ((segment '((type . "card")
                   (data . ((kind . "share")
                            (source . "QQ空间")
                            (title . "一条说说")
                            (image . "https://example.com/preview.png"))))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-url-preview-display-string)
                   (lambda (&rest _args) "PREVIEW")))
          (qq-chat--insert-card-segment segment nil nil)
          (should (string-match-p "Share · QQ空间" (buffer-string)))
          (should (string-match-p "PREVIEW" (buffer-string)))
          (should-not (string-match-p "\\[Open\\]" (buffer-string))))))))

(ert-deftest qq-chat-renders-poke-as-a-distinct-gray-tip-row ()
  (let ((message
         '((server-id . "poke-1")
           (time . 1710000001)
           (sender-id . "10001")
           (sender-name . "Alice")
           (target-id . "10002")
           (timeline-class . service)
           (segments
            . (((type . "gray-tip")
                (data .
                      ((kind . "poke")
                       (actor-name . "Alice")
                       (target-name . "Bob")
                       (image-url . "https://example.com/poke.png")
                       (action . "喷了喷")
                       (detail . "的加分喷雾，分数++")))))))))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (fill-column 80))
        (cl-letf (((symbol-function 'qq-media-url-one-line-preview-display-string)
                   (lambda (&rest _args) "✦")))
          (qq-chat--insert-poke-message message nil)
          (should (string-match-p
                   (regexp-quote
                    "( Alice ✦ 喷了喷 Bob 的加分喷雾，分数++ )")
                   (buffer-string)))
          (should (string-match-p "00:00" (buffer-string)))
          (should (= (count-lines (point-min) (point-max)) 1))
          (should (string-match-p "的加分喷雾，分数++" (buffer-string)))
          (should-not (string-match-p "@ Alice" (buffer-string)))
          (goto-char (point-min))
          (search-forward "00:00")
          (should (equal (get-text-property (- (match-beginning 0) 1)
                                            'display)
                         '(space :align-to 75)))
          (should (eq (get-text-property 2 'face) 'qq-msg-poke)))))))

(ert-deftest qq-chat-self-poke-renders-you-and-self ()
  "A self-initiated self-targeted poke reads as `你 … 自己' like the mobile
client, never as a doubled display name."
  (let ((message
         '((server-id . "poke-2")
           (time . 1710000002)
           (sender-id . "90001")
           (sender-name . "Self")
           (target-id . "90001")
           (timeline-class . service)
           (segments
            . (((type . "gray-tip")
                (data .
                      ((kind . "poke")
                       (actor-id . "90001")
                       (target-id . "90001")
                       (actor-name . "Self")
                       (target-name . "Self")
                       (image-url . "https://example.com/poke.png")
                       (action . "捏了捏")
                       (detail)))))))))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (fill-column 80)
            (qq-state--self-info
             '((user_id . "90001") (nickname . "Self"))))
        (cl-letf (((symbol-function 'qq-media-url-one-line-preview-display-string)
                   (lambda (&rest _args) "✦")))
          (qq-chat--insert-poke-message message nil)
          (should (string-match-p
                   (regexp-quote "( 你 ✦ 捏了捏 自己 )")
                   (buffer-string))))))))

(ert-deftest qq-chat-poke-targeting-self-renders-you ()
  "A poke aimed at the current account names the target `你'."
  (let ((message
         '((server-id . "poke-3")
           (time . 1710000003)
           (sender-id . "10001")
           (sender-name . "Alice")
           (target-id . "90001")
           (timeline-class . service)
           (segments
            . (((type . "gray-tip")
                (data .
                      ((kind . "poke")
                       (actor-id . "10001")
                       (target-id . "90001")
                       (actor-name . "Alice")
                       (target-name . "Self")
                       (image-url . "https://example.com/poke.png")
                       (action . "捏了捏")
                       (detail)))))))))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (fill-column 80)
            (qq-state--self-info
             '((user_id . "90001") (nickname . "Self"))))
        (cl-letf (((symbol-function 'qq-media-url-one-line-preview-display-string)
                   (lambda (&rest _args) "✦")))
          (qq-chat--insert-poke-message message nil)
          (should (string-match-p
                   (regexp-quote "( Alice ✦ 捏了捏 你 )")
                   (buffer-string))))))))

(ert-deftest qq-chat-renders-json-gray-tip-as-system-divider ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:987654321"
    '((type . group) (title . "Test Group") (target-id . "987654321"))
    nil)
   (qq-chat-test--apply-gray-tip-message
    "group:987654321" "9007199254750003456" 1710000000
    '((kind . "general")
      (text . "新进群账号疑似来自非大陆地区，请谨慎核实对方身份。查看异常>")
      (business-type . "1")
      (business-id . "19366")
      (parts
       . (((type . "text")
           (text . "新进群账号疑似来自非大陆地区，请谨慎核实对方身份。查看异常>"))))))
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:987654321")
     (qq-chat-test--window "9007199254750003456" nil)
     (qq-chat-render)
     (should (equal (appkit-chat-timeline-keys)
                    '("9007199254750003456")))
     (should (string-match-p
              "新进群账号疑似来自非大陆地区，请谨慎核实对方身份。查看异常>"
              (buffer-substring-no-properties (point-min) (point-max))))
     (should-not qq-chat--message-selection)
     (should-not
      (string-match-p
       "@ QQ"
       (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest qq-chat-renders-member-add-user-as-avatar-profile-button ()
  (qq-chat-test-with-reset
   (let* ((message
           (qq-chat-test--apply-gray-tip-message
            "group:20001" "9007199254750003460" 1710000001
            '((kind . "group_members_joined")
              (text . "新同学加入群聊")
              (parts
               . (((type . "user")
                   (role . "member")
                   (user-id . "10002")
                   (name . "新同学"))
                  ((type . "text") (text . "加入群聊")))))))
          opened-user)
     (should (member "avatar:10002"
                     (qq-chat--message-media-cache-keys message)))
     (with-temp-buffer
       (let ((inhibit-read-only t)
             (fill-column 80))
         (cl-letf (((symbol-function 'qq-media-avatar-display-string)
                    (lambda (user-id)
                      (should (equal user-id "10002"))
                      "AV"))
                   ((symbol-function 'qq-user-open)
                    (lambda (user-id) (setq opened-user user-id))))
           (qq-chat--insert-gray-tip-message message nil)
           (should (string-match-p
                    (regexp-quote "( AV 新同学加入群聊 )")
                    (buffer-string)))
           (goto-char (point-min))
           (search-forward "新同学")
           (backward-char)
           (let ((button (button-at (point))))
             (should button)
             (should (equal (button-get button 'qq-chat-gray-tip-user-id)
                            "10002"))
             (should (equal
                      (buffer-substring-no-properties
                       (button-start button) (button-end button))
                      "AV 新同学"))
             (push-button button)
             (should (equal opened-user "10002")))))))))

(ert-deftest qq-chat-renders-uid-only-gray-tip-user-without-fake-profile ()
  (qq-chat-test-with-reset
   (let ((message
          (qq-chat-test--apply-gray-tip-message
           "group:20001" "9007199254750003461" 1710000002
           '((kind . "group_member_removed")
             (text . "u_operator 将 u_member 移出群聊")
             (parts
              . (((type . "user")
                  (role . "operator")
                  (identity . ((kind . "uid") (uid . "u_operator")))
                  (user-uid . "u_operator")
                  (name . "u_operator"))
                 ((type . "text") (text . " 将 "))
                 ((type . "user")
                  (role . "member")
                  (identity . ((kind . "uid") (uid . "u_member")))
                  (user-uid . "u_member")
                  (name . "u_member"))
                 ((type . "text") (text . " 移出群聊"))))))))
     (should-not
      (seq-some
       (lambda (key) (string-prefix-p "avatar:" key))
       (qq-chat--message-media-cache-keys message)))
     (with-temp-buffer
       (let ((inhibit-read-only t)
             (fill-column 80))
         (cl-letf (((symbol-function 'qq-media-avatar-display-string)
                    (lambda (&rest _)
                      (ert-fail "UID-only identity requested a fake avatar")))
                   ((symbol-function 'qq-user-open)
                    (lambda (&rest _)
                      (ert-fail "UID-only identity opened a fake profile"))))
           (qq-chat--insert-gray-tip-message message nil)
           (should (string-match-p
                    "u_operator 将 u_member 移出群聊"
                    (buffer-string)))
           (goto-char (point-min))
           (search-forward "u_member")
           (backward-char)
           (should-not (button-at (point)))
           (should-not (get-text-property
                        (point) 'qq-chat-gray-tip-user-id))))))))

(ert-deftest qq-chat-history-header-right-aligns-time-through-appkit ()
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (fill-column 50)
          (message '((server-id . "m1")
                     (time . 1710000001)
                     (sender-id . "10001")
                     (sender-name . "Alice")
                     (segments . (((type . "text")
                                   (data . ((text . "hello")))))))))
      (cl-letf (((symbol-function 'qq-chat--message-avatar-prefixes)
                 (lambda (&rest _args)
                   '(:header "AVA " :first-body "    " :rest-body "    "))))
        (qq-chat--render-message message nil)
        (goto-char (point-min))
        (search-forward (qq-chat--format-time 1710000001))
        (should (equal (get-text-property (- (match-beginning 0) 1)
                                          'display)
                       `(space :align-to
                         ,(- 50
                             (string-width
                              (qq-chat--format-time 1710000001))))))))))

(ert-deftest qq-chat-avatar-spans-heading-and-first-body-line ()
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (fill-column 80)
          (message '((server-id . "m1")
                     (time . 1710000001)
                     (sender-id . "10001")
                     (sender-name . "Alice")
                     (segments . (((type . "text")
                                   (data . ((text . "hello\nworld")))))))))
      (cl-letf (((symbol-function 'qq-chat--message-avatar-prefixes)
                 (lambda (&rest _args)
                   '(:header "TOP "
                     :first-body "BOTTOM "
                     :rest-body "       "))))
        (qq-chat--render-message message nil)
        (goto-char (point-min))
        (should (equal (get-text-property (point) 'line-prefix) "TOP "))
        (forward-line 1)
        (should (equal (get-text-property (point) 'line-prefix) "BOTTOM "))
        (forward-line 1)
        (should (equal (get-text-property (point) 'line-prefix) "       "))))))

(ert-deftest qq-chat-avatar-prefixes-use-shared-two-line-renderer ()
  (let (avatar-user-id shared-args)
    (cl-letf (((symbol-function 'qq-media-avatar-image)
               (lambda (user-id)
                 (setq avatar-user-id user-id)
                 'avatar-image))
              ((symbol-function 'appkit-chat-avatar-two-line-pixel-size)
               (lambda () 42))
              ((symbol-function 'appkit-chat-avatar-prefixes)
               (lambda (image fallback &rest args)
                 (setq shared-args (list image fallback args))
                 '(:header "TOP "
                   :first-body "BOTTOM "
                   :rest-body "       "))))
      (let ((prefixes
             (qq-chat--message-avatar-prefixes
              '((sender-id . "10001") (sender-name . "Alice")))))
        (should (equal avatar-user-id "10001"))
        (should (equal shared-args
                       '(avatar-image "@" (:pixel-size 42 :resize t))))
        (should (eq (get-text-property 0 'mouse-face
                                       (plist-get prefixes :header))
                    'highlight))
        (should-not (get-text-property 0 'mouse-face
                                       (plist-get prefixes :rest-body)))))))

(ert-deftest qq-chat-avatar-prefixes-pass-the-whole-message-to-media ()
  (let (delivered)
    (cl-letf (((symbol-function 'qq-media-message-avatar-image)
               (lambda (message)
                 (setq delivered message)
                 'guild-avatar))
              ((symbol-function 'appkit-chat-avatar-two-line-pixel-size)
               (lambda () 42))
              ((symbol-function 'appkit-chat-avatar-prefixes)
               (lambda (&rest _args)
                 '(:header "TOP " :first-body "BOTTOM " :rest-body "  "))))
      (let ((message
             '((session-key
                . "guild:9007199254740993:channel:9007199254741999")
               (guild-id . "9007199254740993")
               (sender-native-id . "144115219000000001")
               (sender-id . "144115219000000001"))))
        (qq-chat--message-avatar-prefixes message)
        (should (eq delivered message))))))

(ert-deftest qq-chat-message-hover-is-limited-to-interactive-children ()
  (let ((properties
         (qq-chat--message-line-properties
          '((server-id . "m1") (local-id . nil)) "m1")))
    ;; Like telega's outer `telega-msg' button: retain message identity and
    ;; read-only behavior without a blanket mouse face.  Interactive children
    ;; (avatar, sender, media, links, reactions) install their own hover face.
    (should (equal (plist-get properties 'qq-chat-message-anchor) "m1"))
    (should (plist-get properties 'read-only))
    (should-not (plist-member properties 'mouse-face))))

(ert-deftest qq-chat-auto-loads-newer-only-near-partial-window-footer ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--ensure-view)
     (qq-chat-test--window "m10" "m20")
     (let ((qq-chat-history-auto-load-threshold 50)
           calls)
       (cl-letf (((symbol-function 'appkit-chat-timeline-footer-start-position)
                  (lambda () 1000))
                 ((symbol-function 'appkit-chatbuf-composer-idle-p)
                  (lambda () t))
                 ((symbol-function 'qq-chat-load-newer-messages)
                  (lambda (&optional quiet) (push quiet calls))))
         (qq-chat--maybe-auto-load-newer 800)
         (should-not calls)
         (qq-chat--maybe-auto-load-newer 975)

         (should (equal calls '(t))))))))

(ert-deftest qq-chat-scroll-observer-loads-newer-from-selected-viewport-edge ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((view (qq-chat--ensure-view))
            (observer (appkit-chat-timeline-scroll-observer)))
       (qq-chat-test--window "m10" "m20")
       (should (appkit-scroll-observer-p observer))
       (should (appkit-scroll-observer-active-p observer))
       (should (eq view (appkit-scroll-observer-owner observer)))
       (let ((window (selected-window))
             (qq-chat-history-auto-load-threshold 50)
             calls
             checks)
         (cl-letf (((symbol-function
                     'appkit-chat-timeline-footer-start-position)
                    (lambda () 1000))
                   ((symbol-function 'appkit-chatbuf-composer-idle-p)
                    (lambda () t))
                   ((symbol-function 'qq-chat-load-newer-messages)
                    (lambda (&optional quiet) (push quiet calls)))
                   ((symbol-function 'qq-chat--manage-read-position)
                    (lambda (&rest _args)
                      (ert-fail
                       "selected scroll must not duplicate read handling")))
                   ((symbol-function 'appkit-scroll-observer-check)
                    (lambda (candidate &optional _window)
                      (should (eq candidate observer))
                      (setq checks (1+ (or checks 0))))))
           (funcall (appkit-scroll-observer-end-function observer)
                    window 975 1000)

           (should (equal calls '(t)))
           (should (= 1 checks))))))))

(ert-deftest qq-chat-scroll-observer-loads-newer-and-reads-inactive-window ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--ensure-view)
     (qq-chat-test--window "m10" "m20")
     (save-window-excursion
       (let ((observer (appkit-chat-timeline-scroll-observer))
             (window (split-window))
             (qq-chat-history-auto-load-threshold 50)
             calls
             read-position
             checks)
         (set-window-buffer window (current-buffer))
         (let ((viewport-point (window-point window)))
           (cl-letf (((symbol-function 'appkit-chat-timeline-footer-start-position)
                      (lambda () 1000))
                     ((symbol-function 'appkit-chatbuf-composer-idle-p)
                      (lambda () t))
                     ((symbol-function 'qq-chat-load-newer-messages)
                      (lambda (&optional quiet) (push quiet calls)))
                     ((symbol-function 'qq-chat--manage-read-position)
                      (lambda (position &optional read-window)
                        (should (eq read-window window))
                        (setq read-position position)))
                     ((symbol-function 'appkit-scroll-observer-check)
                      (lambda (candidate &optional _window)
                        (should (eq candidate observer))
                        (setq checks (1+ (or checks 0))))))
             (funcall (appkit-scroll-observer-end-function observer)
                      window 975 1000)
             (should (= read-position viewport-point))
             (should (equal calls '(t)))
             (should (= 1 checks)))))))))

(ert-deftest qq-chat-scroll-observer-auto-loads-older-near-top ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--ensure-view)
     (qq-chat-test--window "m10" nil)
     (qq-chat-test--model-put :has-older t)
     (let ((inhibit-read-only t))
       (insert (make-string 3000 ?x)))
     (goto-char (point-min))
     (let ((qq-chat-history-auto-load-threshold 2000)
           called)
       (cl-letf (((symbol-function 'appkit-chatbuf-point-in-input-p)
                  (lambda (&optional _position) nil))
                 ((symbol-function 'qq-chat-load-older-messages)
                  (lambda (&optional quiet) (setq called quiet))))
         (qq-chat--maybe-auto-load-older)

         (should (eq called t)))))))

(ert-deftest qq-chat-read-position-follows-cursor-without-regressing ()
  (qq-chat-test-with-reset
   (let ((first "9007199254741004645")
         (second "9007199254741004646")
         (third "9007199254741004647")
         (appkit-chat-show-unread-divider t)
         calls)
     (qq-state-upsert-session
      "group:20001"
      `((title . "Group")
        (target-id . "20001")
        (type . group)
        (unread-message-count . 2)
        (first-unread-message-id . ,second)
        ;; Native read advancement requires exact sequence evidence.
        (first-unread-message-seq . "2")
        (read-latest-message-id . ,third))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,first) (canonical-row-key . "41")
         (session-key . "group:20001")
         (message-seq . "1") (group-id . "20001")
         (gateway-account-id . "slot-a")
         (sender-name . "Alice")
         (time . 1) (raw-message . "first"))
        ((server-id . ,second) (canonical-row-key . "42")
         (session-key . "group:20001")
         (message-seq . "2") (group-id . "20001")
         (gateway-account-id . "slot-a")
         (sender-name . "Alice")
         (time . 2) (raw-message . "second"))
        ((server-id . ,third) (canonical-row-key . "43")
         (session-key . "group:20001")
         (message-seq . "3") (group-id . "20001")
         (gateway-account-id . "slot-a")
         (sender-name . "Alice")
         (time . 3) (raw-message . "third")))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat-test--window first nil)
       (qq-chat-render)
       (cl-letf (((symbol-function 'qq-core-message-read-capable-p)
                  (lambda (_message) t))
                 ((symbol-function 'qq-runtime-request)
                  (lambda (request &rest _)
                    (let ((message (cadr request)))
                      (setq calls
                            (append calls
                                    (list (list (alist-get 'session-key message)
                                                (alist-get 'server-id message)))))
                      ;; The App commits the report's row as the read position.
                      (qq-operation-test--seed
                       (list 'read-message (alist-get 'session-key message))
                       :value `((account_id . "slot-a")
                                (row_key . ,(alist-get 'canonical-row-key message))))))))
         ;; An already-read row does not move the native boundary backward.
         (goto-char (point-min))
         (search-forward "first")
         (qq-chat--manage-read-position)
         (should-not calls)
         ;; The separator is not the row it precedes, even with row anchoring.
         (goto-char (text-property-any
                     (point-min) (point-max) 'appkit-chat-read-divider t))
         (qq-chat--manage-read-position)
         (should-not calls)
         ;; Point on the timeline reads exactly that row.
         (search-forward "second")
         (qq-chat--manage-read-position)
         (search-forward "third")
         (qq-chat--manage-read-position)
         ;; Moving backward cannot submit an older target.
         (goto-char (point-min))
         (search-forward "second")
         (qq-chat--manage-read-position)
         ;; Point in the composer represents the newest loaded row and dedupes.
         (goto-char (point-max))
         (qq-chat--manage-read-position)
         (should
          (equal calls
                 `(("group:20001" ,second)
                   ("group:20001" ,third)))))))))

(ert-deftest qq-chat-read-position-keeps-loaded-scope ()
  (qq-chat-test-with-reset
   (let* ((session-key "group:20001")
          (first (qq-chat-test--canonical-message "100" 100 "first"))
          (second (qq-chat-test--canonical-message "200" 101 "second"))
          (pending (qq-chat-test--canonical-message "250" 102 "not canonical"))
          (latest (qq-chat-test--canonical-message "300" 103 "not loaded"))
          target)
     (dolist (message (list first second latest))
       (nconc message
              (list (cons 'canonical-row-key (alist-get 'server-id message))
                    (cons 'gateway-account-id "slot-a"))))
     (qq-state-upsert-session
      session-key
      '((title . "Group") (type . group) (target-id . "20001")) nil)
     (puthash session-key (list first second pending latest)
              qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key)
       (qq-chat-test--window "100" "250")
       (qq-chat-render)
       (cl-letf (((symbol-function 'qq-core-message-read-capable-p)
                  #'qq-message-read-capable-p)
                 ((symbol-function 'qq-runtime-request)
                  (lambda (request &rest _)
                    (setq target (alist-get 'server-id (cadr request))))))
         ;; Only canonical rows inside the loaded window can be read.
         (goto-char (point-max))
         (qq-chat--manage-read-position)
         (should (equal target "200"))
         (should (equal (cdar (plist-get (qq-chat--model) :rows)) "100"))
         (should (equal (cdar (last (plist-get (qq-chat--model) :rows))) "250")))))))

(ert-deftest qq-chat-reopen-preserves-pending-history-and-reading-position ()
  (qq-chat-test-with-history-metadata
   (save-window-excursion
     (let ((session-key "group:20001")
           (qq-account--accounts (make-hash-table :test #'equal))
           (fetch-count 0)
           (display-buffer-overriding-action '(display-buffer-same-window))
           buffer)
       (puthash "slot-a" '((account_id . "slot-a")) qq-account--accounts)
       (qq-state-upsert-session
        session-key '((type . group) (target-id . "20001")) nil)
       (unwind-protect
           (cl-letf (((symbol-function 'qq-runtime--rpc-fetch)
                      (lambda (method &rest _)
                        (when (equal method "timeline.open") (cl-incf fetch-count))
                        nil)))
             (qq-chat-open session-key)
         (qq-chat-test--drain-history)
             (setq buffer (current-buffer))
             (let ((owner (qq-chat--loading)))
               (appkit-chatbuf-input-set-text "draft text")
               (goto-char (+ 2 (appkit-chatbuf-input-start-position)))
               (qq-chat-open session-key)
         (qq-chat-test--drain-history)
               (should (eq buffer (current-buffer)))
               (should (eq owner (qq-chat--loading)))
               (should (= fetch-count 1))
               (should (= 2 (- (point) (appkit-chatbuf-input-start-position)))))
             ;; The transport publishes its canonical batch before completion.
             (let ((messages
                    (cl-loop for index from 1 to 80
                             collect
                             (qq-chat-test--canonical-message
                              (number-to-string index) index
                              (format "message %d" index)))))
               (puthash session-key messages qq-state--messages-by-session)
               (qq-chat-test--deliver-timeline
                (mapcar (lambda (message)
                          (cons (alist-get 'id message) (qq-state-message-anchor message)))
                        messages)))
             (should (qq-chat--history-known-p))
             (should-not (qq-chat--loading))
             (goto-char (+ 2 (appkit-chat-timeline-key-position "40")))
             (set-window-start (selected-window)
                               (appkit-chat-timeline-key-position "38"))
             (let ((saved-point (point))
                   (saved-start (window-start)))
               (with-temp-buffer
                 (switch-to-buffer (current-buffer))
                 (qq-chat-open session-key)
         (qq-chat-test--drain-history)
                 (should (eq buffer (current-buffer)))
                 (should (= saved-point (point)))
                 (should (= saved-start (window-start)))
                 (should (equal "40" (appkit-chat-timeline-key-at-point)))
                 (should (equal "draft text" (appkit-chatbuf-input-string)))
                 (should (= fetch-count 1)))))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest qq-chat-dataline-initial-window-uses-unified-durable-history ()
  (qq-chat-test-with-history-metadata
   (dolist (variant '("desktop" "mobile"))
     (let* ((peer "u_Wcc5rknRRqRO8y5gxMD6sA")
            (session-key (format "dataline:%s:%s" variant peer))
            (message-id "7348923749823749823")
            history-call)
       (qq-state-upsert-session
        session-key '((type . dataline) (title . "My device")) nil)
       (with-temp-buffer
         (qq-chat-mode)
         (setq qq-chat--session-key session-key)
         (cl-letf (((symbol-function 'qq-core-fetch-history-page)
                    (lambda (session cursor direction callback
                                     &optional _errback count
                                     lifecycle-owner)
                      (setq history-call
                            (list session cursor direction count))
                      (puthash
                       session-key
                       `(((server-id . ,message-id)
                          (session-key . ,session-key)
                          (time . 1784700000)
                          (raw-message . "durable DataLine text")))
                       qq-state--messages-by-session)
                      (funcall
                       callback
                       (qq-chat-test--history-meta
                        session-key
                        :history-has-older-p nil
                        :history-has-newer-materialized-p nil
                        :batch-message-ids (list message-id)
                        :message-count 1 :added-count 1))
                      (let ((request (qq-request-create)))
                        (qq-request-finish request)
                        request)))
                   ((symbol-function 'qq-chat--update-frame) #'ignore)
                   ((symbol-function 'qq-chat-render) #'ignore))
           (qq-chat--load-initial-history (current-buffer) session-key)
         (qq-chat-test--drain-history)

           (should
            (equal history-call
                   (list session-key nil 'older qq-history-fetch-count)))
           (should (eq (plist-get (qq-chat--model) :timeline) 'dataline))
           (should (equal (mapcar #'qq-state-message-anchor (qq-chat--timeline-messages))
                          (list message-id)))
           (should-not (plist-get (qq-chat--model) :has-older))))))))

(ert-deftest qq-chat-forward-segment-uses-dedicated-block-renderer ()
  (let* ((segment '((type . "forward")
                    (data
                     . ((content
                         . ((kind . "remote")
                            (reference
                             . ((kind . "message")
                                (message_id . "9007199254743009336")
                                (chat . ((kind . "group")
                                         (group_id . "20001")))))))))))
         (message `((server-id . "9007199254743009336")
                    (segments . (,segment))))
         called)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-forward-insert-segment)
                   (lambda (candidate _prefix _properties)
                     (setq called candidate)
                     (insert "FORWARD-CARD\n"))))
          (should (qq-chat--message-has-block-segments-p message))
          (qq-chat--insert-message-body message nil nil)
          (should (equal called segment))
          (should (equal (buffer-string) "FORWARD-CARD\n")))))))

(ert-deftest qq-chat-forward-targets-only-include-private-and-group-sessions ()
  (qq-chat-test-with-reset
   (qq-state-apply-friend-categories
    '(((category_id . 0) (sort_id . 0) (name . "好友")
       (online_count . 0)
       (friends . (((user_id . "10001")
                    (nickname . "Alice") (remark . "A")))))))
   (qq-state-apply-groups
    '(((group_id . "20001") (group_name . "Group A"))))
   (qq-state-upsert-session
    "dataline:mobile:dev:a"
    '((title . "My phone") (target-id . "dev:a") (type . dataline)) nil)
   (qq-state-upsert-session
    "service:u:mail:x"
    '((title . "Mail") (target-id . "u:mail:x") (type . service)) nil)
   (let* ((targets (qq-chat--forwardable-target-sessions))
          (by-key
           (mapcar (lambda (target)
                     (cons (alist-get 'key target) target))
                   targets)))
     (should (assoc "private:10001" by-key))
     (should (assoc "group:20001" by-key))
     (should-not (assoc "dataline:mobile:dev:a" by-key))
     (should-not (assoc "service:u:mail:x" by-key)))))

(ert-deftest qq-chat-forward-source-capability-is-a-closed-session-allowlist ()
  (dolist (session-key '("private:10001" "group:20001"))
    (should (qq-chat--forward-source-supported-p nil session-key))
    (should (qq-chat--forward-source-supported-p 'merged session-key))
    (should
     (qq-chat--forward-source-supported-p 'individual session-key)))
  (dolist (session-key '("service:u:mail:x"
                         "dataline:desktop:dev:a"
                         "dataline:mobile:dev:a"
                         "guild:server:channel"
                         "unknown:session"))
    (dolist (style '(nil individual merged))
      (should-not (qq-chat--forward-source-supported-p style session-key)))))

(ert-deftest qq-chat-direct-selection-rejects-unsupported-source-unchanged ()
  (dolist (session-key '("dataline:mobile:dev:a" "guild:server:channel"))
    (with-temp-buffer
      (qq-chat-mode)
      (let ((selection
             (qq-chat-test--selection "9007199254743009336"))
            message-read-p)
        (setq qq-chat--session-key session-key
              qq-chat--message-selection selection)
        (cl-letf (((symbol-function 'qq-chat--message-at-point)
                   (lambda (&optional _position)
                     (setq message-read-p t)
                     '((server-id . "9007199254743009444")))))
          (should-error
           (qq-chat-toggle-message-selection)
           :type 'user-error))
        (should (eq qq-chat--message-selection selection))
        (should-not message-read-p)))))

(ert-deftest qq-chat-message-selection-advances-to-next-message ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group)) nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001") (sender-name . "Alice")
       (time . 100) (raw-message . "first"))
      ((server-id . "9007199254743009444")
       (sender-id . "10002") (sender-name . "Bob")
       (time . 101) (raw-message . "second")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-test--window "9007199254743009336" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-message-selection)
     (should
      (equal (qq-chat--message-selection-anchors)
             '("9007199254743009336")))
     (should
      (equal (alist-get 'server-id (qq-chat--message-at-point))
             "9007199254743009444")))))

(defconst qq-chat-test--forward-ids
  '("9007199254743009336" "9007199254743009444" "9007199254743009555"))

(defmacro qq-chat-test--with-forward-selection (&rest body)
  "Run BODY in a group chat with every forward fixture message selected.
Messages are selected newest first; RPC calls collect in `calls'."
  (declare (indent 0) (debug t))
  `(qq-chat-test-with-reset
    (qq-state-upsert-session
     "group:20001" '((title . "Source") (target-id . "20001") (type . group)) nil)
    (puthash "group:20001"
             (seq-map-indexed
              (lambda (id index)
                `((server-id . ,id) (sender-id . "10001") (sender-name . "Alice")
                  (time . ,(+ 100 index)) (raw-message . ,(format "message-%d" index))))
              qq-chat-test--forward-ids)
             qq-state--messages-by-session)
    (with-temp-buffer
      (qq-chat-mode)
      (setq qq-chat--session-key "group:20001")
      (qq-chat-test--window (car qq-chat-test--forward-ids) nil)
      (qq-chat-render)
      (dolist (index '(2 1 0))
        (goto-char (point-min))
        (search-forward (format "message-%d" index))
        (qq-chat-toggle-message-selection))
      (let (calls)
        (cl-letf (((symbol-function 'qq-server-gateway-instance-id) (lambda () "gateway-a"))
                  ((symbol-function 'qq-server-cancel) #'ignore)
                  ((symbol-function 'qq-rpc-call)
                   (lambda (method params &rest options)
                     (setq calls (append calls (list (list method params options))))
                     (make-symbol "transport"))))
          ,@body)))))

(defun qq-chat-test--settle-forward (call &optional reason)
  "Settle forward CALL successfully, or fail it with REASON."
  (let ((options (nth 2 call)))
    (if reason
        (funcall (plist-get options :errback) nil reason)
      (funcall (plist-get options :callback)
               (if (equal (car call) "message.send_merged_forward")
                   '((account_id . "slot-a") (resource_id . "resource") (sent_at . 1)
                     (server_sequence . "1") (client_sequence . "1") (random . 1))
                 '((account_id . "slot-a")))))
    (qq-chat-test--drain-history)))

(ert-deftest qq-chat-merged-forward-sends-timeline-order-and-clears ()
  (qq-chat-test--with-forward-selection
    (qq-chat-forward-merged (qq-chat--current-forward-plan) "group:30001")
    (qq-chat-test--drain-history)
    (should (= 1 (length calls)))
    (should (equal (alist-get 'message_ids (nth 1 (car calls)))
                   qq-chat-test--forward-ids))
    (should (= 3 (length qq-chat--message-selection)))
    (qq-chat-test--settle-forward (car calls))
    (should-not qq-chat--message-selection)))

(ert-deftest qq-chat-individual-forward-settles-each-message-in-turn ()
  (qq-chat-test--with-forward-selection
    (qq-chat-forward-individually (qq-chat--current-forward-plan) "group:30001")
    (qq-chat-test--drain-history)
    (should (equal (mapcar (lambda (call) (alist-get 'message_id (nth 1 call))) calls)
                   (list (car qq-chat-test--forward-ids))))
    (qq-chat-test--settle-forward (car calls))
    (should (equal (alist-get 'message_id (nth 1 (nth 1 calls)))
                   (nth 1 qq-chat-test--forward-ids)))
    ;; A failure stops the run; only the forwarded message leaves the selection.
    (qq-chat-test--settle-forward (nth 1 calls) "network failure")
    (should (= 2 (length calls)))
    (should (equal (sort (mapcar #'qq-chat-message-selection-anchor qq-chat--message-selection)
                         #'string<)
                   (cdr qq-chat-test--forward-ids)))))

(ert-deftest qq-chat-forward-target-quit-keeps-selection-without-request ()
  (qq-chat-test--with-forward-selection
    (cl-letf (((symbol-function 'qq-chat--read-forward-target)
               (lambda (&rest _) (signal 'quit nil))))
      (should (eq 'quit (condition-case nil
                            (qq-chat-forward-merged (qq-chat--current-forward-plan))
                          (quit 'quit)))))
    (qq-chat-test--drain-history)
    (should-not calls)
    (should (= 3 (length qq-chat--message-selection)))))

(ert-deftest qq-chat-stable-forward-order-uses-one-rule-per-time-bucket ()
  (let ((with-high-sequence
         '((server-id . "9007199254743009336")
           (message-seq . "300") (time . 100) (order . 3)))
        (without-sequence
         '((server-id . "9007199254743009444")
           (time . 100) (order . 1)))
        (with-low-sequence
         '((server-id . "9007199254743009555")
           (message-seq . "100") (time . 100) (order . 2))))
    ;; Mixing sequence comparison with local-order comparison pair by pair
    ;; produces a non-transitive comparator.  One incomplete timestamp bucket
    ;; therefore uses local insertion order for every member.
    (should
     (equal
      (mapcar #'qq-chat--message-anchor
              (qq-chat--stable-message-order
               (list with-high-sequence without-sequence with-low-sequence)))
      '("9007199254743009444"
        "9007199254743009555"
        "9007199254743009336")))))

(ert-deftest qq-chat-forward-revalidates-recall-after-target-prompt ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((message-id "9007199254743009336")
            (plan
             (qq-chat-test--forward-plan
              (current-buffer) message-id qq-chat--session-key))
            dispatch-called)
       (cl-letf (((symbol-function 'qq-chat--read-forward-target)
                  (lambda (_style _count)
                    (qq-state-apply-recall qq-chat--session-key message-id)
                    "group:30001"))
                 ((symbol-function 'qq-runtime-request)
                  (lambda (&rest _arguments) (setq dispatch-called t))))
         (should-error
          (qq-chat-forward-merged plan)
          :type 'user-error))
       (should-not dispatch-called)))))

(ert-deftest qq-chat-failed-send-survives-replacement-first-render ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001") (type . private)) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-test--model-put :timeline "test" :rows nil)
     (qq-chat-render)
     (qq-chat--insert-input-segment-object
      '((type . "at")
        (data . ((qq . "10001") (name . "Alice Card")))))
     (qq-chat--set-reply-message
      '((server-id . "9007199254742007094")
        (session-key . "private:10001")
        (sender-name . "Alice")
        (raw-message . "source")))
     ;; Match the send boundary, which normalizes live editable properties into
     ;; the canonical snapshot before the destructive clear.
     (qq-chat--sync-draft-from-buffer)
     (let (failure old-view replacement-view draft-state aux-state segments
                   undo-state)
       (setq draft-state (appkit-chatbuf-input-state)
             aux-state (copy-tree (appkit-chatbuf-aux-state))
             segments (qq-chat--current-input-segments))
       (buffer-enable-undo)
       (setq buffer-undo-list nil)
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session _segments &optional _raw _success errback)
                    (setq failure errback)
                    'send-token))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (funcall failure nil "network failed"))
       (should (equal-including-properties
                (appkit-chatbuf-input-state) draft-state))
       (setq undo-state (copy-tree buffer-undo-list)
             old-view (appkit-current-surface))
       (appkit-surface-stop old-view)
       (setq replacement-view (qq-chat--ensure-view))
       (should-not (eq old-view replacement-view))
       (qq-chat-render)
       (should-not qq-chat--send-sync-request)
       (should (appkit-chatbuf-string-has-objects-p
                (appkit-chatbuf-input-state)))
       (should (equal (qq-chat--current-input-segments) segments))
       (should (equal (substring-no-properties
                       (appkit-chatbuf-input-string))
                      (substring-no-properties draft-state)))
       (should (appkit-chatbuf-input-has-objects-p))
       (should (equal (appkit-chatbuf-aux-state) aux-state))
       (should (equal buffer-undo-list undo-state))))))

(ert-deftest qq-chat-late-send-failure-materializes-current-replacement-view ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001") (type . private)) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-test--model-put :timeline "test" :rows nil)
     (qq-chat-render)
     (qq-chat--insert-input-segment-object
      '((type . "at")
        (data . ((qq . "10001") (name . "Alice Card")))))
     (qq-chat--set-reply-message
      '((server-id . "9007199254742007094")
        (session-key . "private:10001")
        (sender-name . "Alice")
        (raw-message . "source")))
     (qq-chat--sync-draft-from-buffer)
     (let (failure old-view replacement-view draft-state aux-state segments
                   sync-views)
       (setq draft-state (appkit-chatbuf-input-state)
             aux-state (copy-tree (appkit-chatbuf-aux-state))
             segments (qq-chat--current-input-segments))
       (cl-letf (((symbol-function 'qq-core-send-message)
                  (lambda (_session _segments &optional _raw _success errback)
                    (setq failure errback)
                    'send-token)))
         (qq-chat-send-message))
       (setq old-view (appkit-current-surface))
       (appkit-surface-stop old-view)
       (setq replacement-view (qq-chat--ensure-view))
       ;; This is the reverse ordering: the replacement has already projected
       ;; the successful-send shape before the old transport reports failure.
       (qq-chat-render)
       (should (equal (appkit-chatbuf-input-state) ""))
       (should (equal (appkit-chatbuf-input-string) ""))
       (let ((request-sync (symbol-function 'appkit-surface-send)))
         (cl-letf (((symbol-function 'appkit-surface-send)
                    (lambda (view &rest arguments)
                      (push view sync-views)
                      (apply request-sync view arguments)))
                   ((symbol-function 'qq-api--default-error) #'ignore))
           (funcall failure nil "network failed")))
       (should (equal sync-views (list replacement-view)))
       (should (equal-including-properties
                (appkit-chatbuf-input-state) draft-state))
       (should-not qq-chat--send-sync-request)
       (should (appkit-chatbuf-input-has-objects-p))
       (should (equal (qq-chat--current-input-segments) segments))
       (should (equal (appkit-chatbuf-aux-state) aux-state))
       ;; The first real edit must extend the materialized rich draft instead
       ;; of synchronizing the formerly empty replacement tail over it.
       (goto-char (appkit-chatbuf-input-logical-end-position))
       (insert " tail")
       (should (appkit-chatbuf-string-has-objects-p
                (appkit-chatbuf-input-state)))
       (should (equal (car (qq-chat--current-input-segments))
                      (car segments)))
       (should (string-suffix-p
                " tail" (appkit-chatbuf-input-string)))))))

(ert-deftest qq-chat-friend-pin-actions-use-authoritative-private-peer ()
  (qq-chat-test-with-reset
   (let (calls)
     (qq-state-apply-friend-categories
      '(((category_id . 0) (name . "Default")
         (friends . (((user_id . "9007199254740999")
                      (nickname . "Alice")))))))
     (qq-state-upsert-session
      "private:9007199254740999"
      '((title . "Alice") (target-id . "9007199254740999")
        (type . private)) nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:9007199254740999")
       (cl-letf (((symbol-function 'qq-runtime-request)
                  (lambda (request &rest _) (push (cdr request) calls))))
         (should (qq-chat--friend-pin-capable-p))
         (qq-chat-pin-friend)
         (qq-chat-unpin-friend))
       (should
        (equal (nreverse calls)
               '(("9007199254740999" t)
                 ("9007199254740999" nil))))))))

(ert-deftest qq-chat-friend-pin-rejects-groups-and-nonfriends ()
  (qq-chat-test-with-reset
   (progn
     (qq-state-upsert-session
      "private:10001"
      '((target-id . "10001") (type . private)) nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (should-not (qq-chat--friend-pin-capable-p))
       (should-error (qq-chat-pin-friend) :type 'user-error))
     (qq-state-upsert-session
      "group:20001"
      '((target-id . "20001") (type . group)) nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (should-not (qq-chat--friend-pin-capable-p))
       (should-error (qq-chat-unpin-friend) :type 'user-error)))))

(ert-deftest qq-chat-buffer-names-present-human-session-kind ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((type . private) (title . "Alice") (target-id . "10001"))
    nil)
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Emacs CN") (target-id . "20001"))
    nil)
   (should (equal "QQ{Alice}" (qq-chat--buffer-name "private:10001")))
   (should (equal "QQ[Emacs CN]" (qq-chat--buffer-name "group:20001")))))

(ert-deftest qq-chat-buffer-name-disambiguates-account-only-on-collision ()
  (let ((qq-runtime--accounts (make-hash-table :test #'equal))
        (qq-state--partitions (make-hash-table :test #'equal))
        (qq-state--active-account-id nil)
        first-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-runtime-account-display-name)
                   (lambda (account-id) account-id)))
          (qq-runtime-with-account "slot-a"
                                   (qq-state-upsert-session
                                    "private:10001"
                                    '((type . private) (title . "Alice") (target-id . "10001"))
                                    nil)
                                   (setq first-buffer (get-buffer-create "QQ{Alice}"))
                                   (with-current-buffer first-buffer
                                     (setq-local qq-runtime--account-id "slot-a")
                                     (setq-local qq-chat--session-key "private:10001")))
          (qq-runtime-with-account "slot-b"
                                   (qq-state-upsert-session
                                    "private:10001"
                                    '((type . private) (title . "Alice") (target-id . "10001"))
                                    nil)
                                   (should
                                    (equal "QQ{Alice}<slot-b>"
                                           (qq-chat--buffer-name "private:10001")))))
      (when (buffer-live-p first-buffer)
        (kill-buffer first-buffer))
      (qq-runtime-stop-account "slot-a" t)
      (qq-runtime-stop-account "slot-b" t))))

(ert-deftest qq-chat-same-session-key-stays-independent-in-two-accounts ()
  (let ((qq-runtime--accounts (make-hash-table :test #'equal))
        (qq-state--partitions (make-hash-table :test #'equal))
        (qq-state--active-account-id nil)
        (qq-state-change-hook nil)
        (qq-media-cache-update-hook nil)
        buffer-a buffer-b)
    (unwind-protect
        (progn
          (qq-runtime-with-account "slot-a"
                                   (qq-state-upsert-session
                                    "private:10001"
                                    '((type . private) (target-id . "10001") (title . "Alice A"))
                                    nil)
                                   (setq buffer-a (qq-chat--open-buffer "private:10001")))
          (qq-runtime-with-account "slot-b"
                                   (qq-state-upsert-session
                                    "private:10001"
                                    '((type . private) (target-id . "10001") (title . "Alice B"))
                                    nil)
                                   (setq buffer-b (qq-chat--open-buffer "private:10001")))
          (should (buffer-live-p buffer-a))
          (should (buffer-live-p buffer-b))
          (should-not (eq buffer-a buffer-b))
          (with-current-buffer buffer-a
            (should (equal qq-runtime--account-id "slot-a"))
            (should (equal qq-chat--session-key "private:10001"))
            (appkit-chatbuf-input-set-text "draft-a"))
          (with-current-buffer buffer-b
            (should (equal qq-runtime--account-id "slot-b"))
            (should (equal qq-chat--session-key "private:10001"))
            (appkit-chatbuf-input-set-text "draft-b"))
          (with-current-buffer buffer-a
            (should (equal (appkit-chatbuf-input-string) "draft-a"))
            (qq-runtime-with-account qq-runtime--account-id
                                     (should
                                      (equal (alist-get 'title
                                                        (qq-state-session "private:10001"))
                                             "Alice A"))))
          (with-current-buffer buffer-b
            (should (equal (appkit-chatbuf-input-string) "draft-b"))
            (qq-runtime-with-account qq-runtime--account-id
                                     (should
                                      (equal (alist-get 'title
                                                        (qq-state-session "private:10001"))
                                             "Alice B")))))
      (qq-runtime-stop-account "slot-a" t)
      (qq-runtime-stop-account "slot-b" t)
      (dolist (buffer (list buffer-a buffer-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest qq-chat-animated-face-is-a-block-segment ()
  (let ((segment '((type . "face")
                   (data . ((id . "478")
                            (raw . ((faceType . 3)
                                    (stickerId . "80"))))))))
    (should (qq-chat--animated-face-segment-p segment))
    (should (qq-chat--message-has-block-segments-p
             `((segments . (,segment)))))))

(defmacro qq-chat-test-with-translation-message (&rest body)
  "Run BODY in a real local chat containing one account-owned prose row."
  (declare (indent 0) (debug t))
  `(qq-chat-test-with-reset
    (qq-state-upsert-session
     "group:20001"
     '((type . group) (title . "Translation") (target-id . "20001")) nil)
    (let ((message (qq-chat-test--canonical-message
                    "9007199254743009336" 100 "Original prose")))
      (setf (alist-get 'gateway-account-id message) "slot-a")
      (puthash "group:20001" (list message) qq-state--messages-by-session)
      (with-temp-buffer
        (qq-chat-mode)
        (setq qq-chat--session-key "group:20001")
        (qq-chat-test--window "9007199254743009336" nil)
        (qq-chat-render)
        (goto-char (point-min))
        (search-forward "Original prose")
        ,@body))))

(ert-deftest qq-chat-translation-exports-only-loaded-inline-prose ()
  (qq-chat-test-with-translation-message
    (let* ((message (car (qq-state-session-messages qq-chat--session-key)))
           (segments
            '(((type . "reply") (data . ((text . "quoted secret"))))
              ((type . "text") (data . ((text . "Hello "))))
              ((type . "at") (data . ((qq . "10001") (name . "Alice"))))
              ((type . "text") (data . ((text . ", welcome"))))
              ((type . "image") (data . ((url . "https://private/image"))))
              ((type . "forward") (data . ((content . "forward secret"))))
              ((type . "mail") (data . ((text . "mail secret"))))
              ((type . "wallet") (data . ((text . "wallet secret"))))
              ((type . "json") (data . ((text . "card secret"))))
              ((type . "__unsupported") (data . ((text . "hidden metadata"))))
              ((type . "text") (data . ((text . "Next paragraph")))))))
      (setf (alist-get 'segments message) segments
            (alist-get 'preview message) "summary secret")
      (should
       (equal (plist-get (qq-chat--translation-source message t) :text)
              "Hello @Alice, welcome\nNext paragraph"))
      (dolist (body (list (list (nth 4 segments))
                         (list (nth 2 segments))
                         nil))
        (setf (alist-get 'segments message) body)
        (should-not (qq-chat--translation-source message t))
        (puthash qq-chat--session-key (list message) qq-state--messages-by-session)
        (let ((appkit-translate-backend-function
               (lambda () (ert-fail "Non-prose must not initialize a backend"))))
          (should-error (qq-chat-translate-message) :type 'user-error)))
      (setf (alist-get 'segments message) segments
            (alist-get 'status message) 'recalled)
      (should-not (qq-chat--translation-source message t))
      (setf (alist-get 'status message) 'received
            (alist-get 'gateway-account-id message) "slot-b")
      (should-not (qq-chat--translation-source message t)))))

(ert-deftest qq-chat-translation-renders-and-preserves-composer ()
  (qq-chat-test-with-translation-message
    (let* (complete
           (appkit-translate-backend-function
            (lambda ()
              (list :id 'fixture :label "Fixture"
                    :start (lambda (_source _language success _failure)
                             (setq complete success)
                             nil)))))
      (qq-chat--set-draft "Private unsent draft")
      (goto-char (point-min))
      (search-forward "Original prose")
      (qq-chat-toggle-message-selection)
      (goto-char (point-min))
      (search-forward "Original prose")
      (let ((selected (qq-chat--message-selection-anchors)))
        (qq-chat-translate-message)
        (funcall complete "Translated prose")
        (should (equal selected (qq-chat--message-selection-anchors))))
      (should (equal (qq-chat--current-draft-string) "Private unsent draft"))
      (should (equal (appkit-chatbuf-input-state) "Private unsent draft"))
      (goto-char (point-min))
      (search-forward "Original prose")
      (should (search-forward "Translated prose"
                              (appkit-chatbuf-input-start-position) t))
      (let* ((message (car (qq-state-session-messages qq-chat--session-key)))
             (source (qq-chat--translation-source message t)))
        (should (equal (plist-get source :text) "Original prose"))
        (appkit-translate-hide source)
        (should-not (string-match-p "Translated prose" (buffer-string)))
        (should (string-match-p "Show" (buffer-string)))
        (goto-char (point-min))
        (search-forward "Original prose")
        (qq-chat-translate-message)
        (should (string-match-p "Translated prose" (buffer-string)))))))

(ert-deftest qq-chat-translation-revokes-obsolete-row-and-surface-results ()
  (dolist (boundary '(edit recall delete rekey reopen account))
    (qq-chat-test-with-translation-message
      (let* (complete
             (appkit-translate-backend-function
              (lambda ()
                (list :id 'fixture :label "Fixture"
                      :start (lambda (_source _language success _failure)
                               (setq complete success)
                               nil)))))
        (when (eq boundary 'rekey)
          (let ((message (car (qq-state-session-messages qq-chat--session-key))))
            (setf (alist-get 'server-id message) nil
                  (alist-get 'id message) "local-1"
                  (alist-get 'local-id message) "local-1"
                  (alist-get 'status message) 'pending)
            (puthash qq-chat--session-key (list message)
                     qq-state--messages-by-session)
            (qq-chat-test--window "local-1" nil)
            (qq-chat-render)
            (goto-char (point-min))
            (search-forward "Original prose")))
        (qq-chat-translate-message)
        (let ((message (car (qq-state-session-messages qq-chat--session-key))))
          (pcase boundary
            ('edit
             (setf (alist-get 'segments message)
                   '(((type . "text") (data . ((text . "Edited prose")))))))
            ('recall (setf (alist-get 'status message) 'recalled))
            ('delete (setq message nil))
            ('rekey
             (setf (alist-get 'server-id message) "9007199254743009336"
                   (alist-get 'id message) "9007199254743009336"
                   (alist-get 'status message) 'sent))
            ('reopen (appkit-surface-stop (appkit-current-surface)))
            ('account
             (setf (alist-get 'gateway-account-id message) "slot-b")))
          (puthash qq-chat--session-key (and message (list message))
                   qq-state--messages-by-session)
          (when (memq boundary '(rekey reopen))
            (qq-chat-test--window "9007199254743009336" nil))
          (qq-chat-render)
          (funcall complete "OBSOLETE TRANSLATION")
          (should-not (string-match-p "OBSOLETE TRANSLATION" (buffer-string)))
          (should-not (string-match-p "Translation ·" (buffer-string)))
          (when (memq boundary '(rekey reopen account))
            (should (string-match-p "Original prose" (buffer-string))))
          (when (eq boundary 'edit)
            (should (string-match-p "Edited prose" (buffer-string))))
          (should (equal (qq-chat--current-draft-string) "")))))))

(ert-deftest qq-chat-translation-callback-keeps-account-partitions-isolated ()
  (qq-chat-test-with-translation-message
    (let* ((buffer-a (current-buffer))
           complete
           (appkit-translate-backend-function
            (lambda ()
              (list :id 'fixture :label "Fixture"
                    :start (lambda (_source _language success _failure)
                             (setq complete success)
                             nil)))))
      (qq-chat-translate-message)
      (unwind-protect
          (qq-runtime-with-account "slot-b"
            (qq-state-upsert-session
             "group:20001"
             '((type . group) (title . "Other account")
               (target-id . "20001")) nil)
            (let ((message (qq-chat-test--canonical-message
                            "9007199254743009336" 100 "Other account prose"))
                  (qq-chat-mode-hook nil))
              (setf (alist-get 'gateway-account-id message) "slot-b")
              (puthash "group:20001" (list message)
                       qq-state--messages-by-session)
              (with-temp-buffer
                (qq-chat-mode)
                (qq-runtime-bind-account "slot-b")
                (setq qq-chat--session-key "group:20001")
                (qq-chat-test--window "9007199254743009336" nil)
                (qq-chat-render)
                ;; Completion runs while another account's buffer and state
                ;; are current, as with an ordinary asynchronous backend.
                (funcall complete "Only account A translation")
                (should (string-match-p "Other account prose" (buffer-string)))
                (should-not (string-match-p "Only account A translation"
                                            (buffer-string)))
                (should (equal qq-state--active-account-id "slot-b"))
                (with-current-buffer buffer-a
                  (should (string-match-p "Only account A translation"
                                          (buffer-string)))))))
        (qq-runtime-stop-account "slot-b" t)))))




(provide (quote qq-chat-test))

;;; qq-chat-test.el ends here
