;;; qq-chat-test.el --- Tests for qq-chat -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-chat)
(require 'qq-forward)
(require 'qq-state)
(require 'qq-transient)
(require 'qq)

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

(defun qq-chat-test-sync-invalidations ()
  "Synchronously flush the current chat view's queued invalidations."
  (appkit-sync-invalidations (appkit-current-view)))

(defun qq-chat-test-sync-until-idle (&optional limit)
  "Flush callback-driven chat invalidations until idle, bounded by LIMIT."
  (let ((view (appkit-current-view))
        (remaining (or limit 32)))
    (while (and (> remaining 0)
                (appkit-view-live-p view)
                (appkit-invalidations-any-p
                 (appkit-view-invalidations view)))
      (cl-decf remaining)
      (appkit-sync-invalidations view))
    (when (and (appkit-view-live-p view)
               (appkit-invalidations-any-p
                (appkit-view-invalidations view)))
      (ert-fail "chat invalidations did not settle"))))

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
     (buffer-local-value 'qq-chat--forward-plan-owner buffer))))

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

(ert-deftest qq-chat-header-contains-state-not-a-key-cheat-sheet ()
  (with-temp-buffer
    (qq-chat-mode)
    (let ((header (qq-chat--header-text)))
      (should-not (string-match-p "M-<" header))
      (should-not (string-match-p "C-c" header)))))

(ert-deftest qq-chat-header-line-is-cached-between-redisplays ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001")
       (time . 100)
       (raw-message . "marked")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001"
           qq-chat--message-selection
           (qq-chat-test--selection "9007199254743009336"))
     (let ((original-getter (symbol-function 'qq-state-session-messages))
           (getter-calls 0))
       (cl-letf (((symbol-function 'qq-state-session-messages)
                  (lambda (session-key)
                    (cl-incf getter-calls)
                    (funcall original-getter session-key))))
         (qq-chat--header-line-update)
         (should (= getter-calls 1))
         (should (stringp header-line-format))
         (should (string-match-p "1 selected" header-line-format))
         (dotimes (_ 5)
           (format-mode-line header-line-format))
         (should (= getter-calls 1)))))))

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
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001"
           qq-chat--message-selection
           (qq-chat-test--selection "9007199254743009336"))
     (qq-chat--header-line-update)
     (should-not qq-chat--message-selection)
     (should-not (string-match-p "selected" header-line-format)))))

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
     (should-not (string-match-p "\\[ready\\]" (qq-chat--header-line)))
     (qq-state-set-connection-status 'reconnecting)
     (should (string-match-p "\\[reconnecting\\]"
                             (qq-chat--header-line))))))

(ert-deftest qq-chat-connection-state-change-refreshes-header-line ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let ((updates 0))
       (cl-letf (((symbol-function 'qq-chat--header-line-update)
                  (lambda () (cl-incf updates))))
         (qq-chat--handle-state-change
          '(:type connection :account-id "slot-a" :status ready))
         (qq-chat-test-sync-invalidations)
         (should (= updates 1)))))))

(ert-deftest qq-chat-frontier-observation-ignores-ordinary-message-updates ()
  (qq-chat-test-with-reset
   (let ((cached-tail "9007199254742007088")
         (remote-frontier "9007199254742007099"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,cached-tail) (time . 1)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (setq qq-chat--remote-latest-id remote-frontier)
       ;; Recall/reaction notices update an existing cached tail.  Being the
       ;; newest loaded row does not make it the remote live frontier.
       (qq-chat--observe-message-frontier
        `(:type message
          :session-key "group:20001"
          :mutation update
          :source notice
          :message-anchor ,cached-tail
          :message ((server-id . ,cached-tail) (time . 1))))
       (should (equal qq-chat--remote-latest-id
                      remote-frontier))))))

(ert-deftest qq-chat-frontier-observation-accepts-create-and-pending-promotion ()
  (qq-chat-test-with-reset
   (let ((live-id "9007199254742007090")
         (promoted-id "9007199254742007091"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       ;; A local pending row may sort after the newest server row.  It must
       ;; not hide a real live create from frontier observation.
       (puthash
        "group:20001"
        `(((server-id . ,live-id) (time . 2))
          ((local-id . "local-tail") (time . 3) (status . pending)))
        qq-state--messages-by-session)
       (qq-chat--observe-message-frontier
        `(:type message
          :session-key "group:20001"
          :mutation create
          :source event
          :message-anchor ,live-id
          :message ((server-id . ,live-id) (time . 2))))
       (should (equal qq-chat--remote-latest-id live-id))

       ;; A send response/event may introduce the same remote row by rekeying
       ;; its exact pending local anchor rather than by a create mutation.
       (puthash
        "group:20001"
        `(((server-id . ,promoted-id)
           (local-id . "local-promoted")
           (time . 4)))
        qq-state--messages-by-session)
       (setq qq-chat--remote-latest-id live-id)
       (qq-chat--observe-message-frontier
        `(:type message
          :session-key "group:20001"
          :mutation update
          :source response
          :previous-anchor "local-promoted"
          :message-anchor ,promoted-id
          :message ((server-id . ,promoted-id)
                    (local-id . "local-promoted")
                    (time . 4))))
       (should (equal qq-chat--remote-latest-id
                      promoted-id))))))

(ert-deftest qq-chat-input-region-uses-editing-keymap ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (save-window-excursion
     (let ((buffer (get-buffer-create " *qq-chat-input*")))
       (unwind-protect
           (progn
             (switch-to-buffer buffer)
             (qq-chat-mode)
             (should-not (derived-mode-p 'special-mode))
             (should (derived-mode-p 'appkit-chatbuf-mode))
             (setq qq-chat--session-key "private:10001")
             (qq-chat-render)
             (qq-chat-edit-draft)
             (appkit-chatbuf-update-context-mode)
             (should (appkit-chatbuf-point-in-input-p))
             (should (eq (key-binding (kbd "q") t)
                         'self-insert-command))
             (should (eq (key-binding (kbd "s") t)
                         'self-insert-command))
             (should (eq (key-binding (kbd "RET") t) 'qq-chat-return-dwim))
             (should (eq (key-binding (kbd "DEL") t)
                         'appkit-chatbuf-input-backward-delete))
             (should (eq (key-binding (kbd "C-d") t)
                         'appkit-chatbuf-input-forward-delete))
             (execute-kbd-macro "qs")
             (should (equal (qq-chat--current-draft-string) "qs"))
             (goto-char (point-min))
             (appkit-chatbuf-update-context-mode)
             (should qq-chat-timeline-mode)
             (should (eq (key-binding (kbd "q") t) 'quit-window))
             (should (eq (key-binding (kbd "r") t) 'qq-chat-reply-to-message))
             (should (eq (key-binding (kbd "d") t)
                         'qq-chat-delete-transient))
             (should-not (lookup-key qq-chat-timeline-mode-map (kbd "R")))
             (should (eq (key-binding (kbd "f") t)
                         'qq-chat-forward-transient))
             (should (eq (key-binding (kbd "m") t)
                         'qq-chat-toggle-message-selection))
             (should (eq (key-binding (kbd "U") t)
                         'qq-chat-clear-message-selection))
             (should (eq (key-binding (kbd "!") t)
                         'qq-chat-react-to-message))
             (should (eq (key-binding (kbd "P") t)
                         'qq-chat-poke-sender))
             (should (eq (key-binding (kbd "?") t) 'qq-chat-transient))
             (should (eq (key-binding (kbd "C-c RET") t)
                         'qq-chat-send-message))
             (should (eq (key-binding (kbd "C-c m") t) 'qq-chat-message-transient))
             (should (eq (key-binding (kbd "C-c ?") t) 'qq-chat-transient))
             (should (eq (key-binding (kbd "C-c P") t)
                         'qq-chat-send-poke))
             (should (eq (key-binding (kbd "C-c C-a") t) 'qq-chat-attach))
             (should (eq (key-binding (kbd "C-c C-v") t) 'qq-chat-attach-clipboard)))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

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
  (let ((qq-runtime--app (appkit-app-start 'qq :id 'chat-reset-test))
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
            (qq-chat--set-empty-history-window)
            (qq-chat-render)
            (let ((anchor "9007199254743009336"))
              (setq view (appkit-current-view)
                    plan (qq-chat-test--forward-plan buffer anchor)
                    qq-chat--message-selection
                    (qq-chat-test--selection anchor)
                    qq-chat--forward-request 'pre-reset-request
                    qq-chat--forward-request-owner
                    (list 'pre-reset-request-owner))))
          (should (appkit-view-live-p view))
          (qq-reset-session-state)
          (should-not (buffer-live-p buffer))
          (should-not (appkit-view-live-p view))
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
     (qq-chat--set-history-window "m1" nil)
     (should-error (qq-chat-render) :type 'error))))

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
     (qq-chat--set-history-window "m1" nil)
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
     (qq-chat--set-history-window "m1" nil)
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
     (qq-chat--set-history-window "m1" nil)
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
       (qq-chat--set-history-window "m1" nil)
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
       (qq-chat--set-history-window "m1" nil)
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

(ert-deftest qq-chat-state-events-require-the-exact-account-owner ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let ((view (qq-chat--ensure-view))
           calls)
       (cl-letf (((symbol-function 'appkit-request-sync)
                  (lambda (&rest arguments)
                    (push arguments calls))))
         (qq-chat--handle-state-change
          '(:type connection :status ready))
         (qq-chat--handle-state-change
          '(:type connection :account-id "slot-b" :status ready))
         (should-not calls)
         (should-not (appkit-view-pending-events-snapshot view))
         (qq-chat--handle-state-change
          '(:type connection :account-id "slot-a" :status ready))
         (should (= (length calls) 1))
         (should
          (equal (appkit-view-pending-events-snapshot view)
                 '((:type connection
                    :account-id "slot-a"
                    :status ready)))))))))

(ert-deftest qq-chat-state-callback-enqueues-before-one-atomic-sync-request ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let ((view (qq-chat--ensure-view))
           calls)
       (cl-letf (((symbol-function 'appkit-request-sync)
                  (lambda (candidate &rest options)
                    (should
                     (equal (appkit-view-pending-events-snapshot candidate)
                            '((:type connection
                               :account-id "slot-a"
                               :status ready))))
                    (push (cons candidate options) calls)))
                 ((symbol-function 'appkit-invalidate)
                  (lambda (&rest _)
                    (ert-fail "state callback split invalidation")))
                 ((symbol-function 'appkit-schedule-sync)
                  (lambda (&rest _)
                    (ert-fail "state callback used bare scheduling"))))
         (qq-chat--handle-state-change
          '(:type connection :account-id "slot-a" :status ready)))
       (should (equal calls (list (list view :part 'frame))))))))

(ert-deftest qq-chat-message-state-change-uses-one-projected-sync-path ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (sync-args render-called)
       (cl-letf (((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest args) (setq sync-args args)))
                 ((symbol-function 'qq-chat-render)
                  (lambda () (setq render-called t))))
         (qq-chat--handle-state-change
          '(:type message
            :account-id "slot-a"
            :session-key "private:10001"
            :mutation create
            :message-anchor "9007199254741004645"
            :message ((server-id . "9007199254741004645"))))
         (qq-chat-test-sync-invalidations)
         (should (equal (plist-get sync-args :changed-resources)
                        '((:message "9007199254741004645"))))
         (should-not render-called))))))

(ert-deftest qq-chat-session-state-change-updates-shared-frame ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (events)
       (cl-letf (((symbol-function 'qq-chat--header-line-update)
                  (lambda () (push 'header events)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (push 'frame events))))
         (qq-chat--handle-state-change
          '(:type session :account-id "slot-a"
            :session-key "private:10001" :mutation session))
         (qq-chat-test-sync-invalidations)
         (should (equal events '(frame header))))))))

(ert-deftest qq-chat-read-state-change-uses-projected-context-sync ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (called)
       (cl-letf (((symbol-function 'qq-chat--apply-read-state-change)
                  (lambda () (setq called t))))
         (qq-chat--handle-state-change
          '(:type session :account-id "slot-a"
            :session-key "private:10001" :mutation read))
         (qq-chat-test-sync-invalidations)
         (should called))))))



(ert-deftest qq-chat-friend-refresh-keeps-authored-timeline-names-stable ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (events)
       (cl-letf (((symbol-function 'qq-chat--header-line-update)
                  (lambda () (push 'header events)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (push 'frame events)))
                 ((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest args) (push (cons 'timeline args) events))))
         (qq-chat--handle-state-change
          '(:type friends-refreshed :account-id "slot-a" :count 1))
         (qq-chat-test-sync-invalidations)
         (should (equal events '(header))))))))

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
     (qq-chat--set-history-window nil nil)
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
           '((server-id . "m1")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "old body"))))))))
          (source-new
           '((server-id . "m1")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "new body"))))))))
          (reply
           '((server-id . "m2")
             (sender-id . "10002")
             (sender-name . "Bob")
             (time . 200)
             (segments . (((type . "reply")
                           (data . ((id . "m1"))))
                          ((type . "text")
                           (data . ((text . "answer"))))))))
          (initial (list source-old reply))
          (updated (list source-new reply)))
     (puthash "private:10001" initial qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat--set-history-window "m1" nil)
       (qq-chat-render)
       (qq-chat--set-reply-message source-old)
       (qq-chat--set-draft "draft stays")
       (qq-chat--update-frame)
       (puthash "private:10001" updated qq-state--messages-by-session)
       (qq-chat--apply-message-state-change
        (list :type 'message
              :session-key "private:10001"
              :message-anchor "m1"
              :message source-new))
       (should (equal "new body"
                      (qq-state-message-preview (qq-chat--reply-message))))
       (should (equal "draft stays" (qq-chat--current-draft-string)))
       (goto-char (point-min))
       (should (search-forward "↪ Alice: new body" nil t))))))

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
       (qq-chat--set-history-window "m1" nil)
       (qq-chat-render)
       (let ((recalled (qq-state--as-recalled-message source)))
         (puthash "private:10001" (list recalled reply)
                  qq-state--messages-by-session)
         (qq-chat--apply-message-state-change
          (list :type 'message :message-anchor "m1" :message recalled)))
       (should-not (appkit-chat-timeline-node "m1"))
       (should (appkit-chat-timeline-node "m2"))
       (goto-char (point-min))
       (should (search-forward "↪ Alice: [message recalled]" nil t))))))

(ert-deftest qq-chat-history-source-arrival-redisplays-reply-dependent ()
  "History source arrival resolves an already rendered reply preview."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((source
           '((server-id . "m1")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "source arrived"))))))))
          (reply
           '((server-id . "m2")
             (sender-id . "10002")
             (sender-name . "Bob")
             (time . 200)
             (segments . (((type . "reply")
                           (data . ((id . "m1"))))
                          ((type . "text")
                           (data . ((text . "answer")))))))))
     (puthash "private:10001" (list reply) qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat--set-history-window nil nil)
       (qq-chat-render)
       (goto-char (point-min))
       (should (search-forward "↪ id m1" nil t))
       (puthash "private:10001" (list source reply) qq-state--messages-by-session)
       (qq-chat--sync-timeline :changed-resources '((:message "m1")))
       (goto-char (point-min))
       (should (search-forward "↪ Alice: source arrived" nil t))))))

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
     (qq-chat--set-history-window nil nil)
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
     (qq-chat--set-history-window nil nil)
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
       (qq-chat--set-history-window "m1" nil)
       (qq-chat-render)
       (should (equal (appkit-chat-timeline-keys) '("m1")))
       (puthash "private:10001" nil qq-state--messages-by-session)
       (qq-chat--sync-timeline)
       (should (equal (appkit-chat-timeline-keys)
                      (list qq-chat--empty-placeholder)))
       (goto-char (point-min))
       (should (search-forward "No messages loaded yet." nil t))))))

(ert-deftest qq-chat-media-cache-update-requests-node-refresh ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (puthash "private:10001"
            '(((server-id . "m1") (sender-id . "10001")
               (sender-name . "Alice")
               (time . 1) (raw-message . "hello")))
            qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--set-history-window "m1" nil)
     (qq-chat-render)
     (let ((view (appkit-current-view)) calls)
       (cl-letf (((symbol-function 'appkit-request-sync)
                  (lambda (candidate &rest options)
                    (push (cons candidate options) calls)))
                 ((symbol-function 'appkit-invalidate)
                  (lambda (&rest _)
                    (ert-fail "media callback split invalidation")))
                 ((symbol-function 'appkit-schedule-sync)
                  (lambda (&rest _)
                    (ert-fail "media callback used bare scheduling")))
                 ((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest _)
                    (ert-fail "media callback projected rows"))))
         (qq-chat--rerender-open-chats)
         (should
          (equal calls
                 (list
                  (list view :part 'composer :entries '("m1"))))))))))

(ert-deftest qq-chat-avatar-cache-update-refreshes-composer-precisely ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (let ((view (appkit-current-view)) calls)
       (cl-letf (((symbol-function 'appkit-request-sync)
                  (lambda (candidate &rest options)
                    (push (cons candidate options) calls))))
         (qq-chat--rerender-open-chats "group-avatar:20001")
         (should
          (equal calls
                 (list
                  (list view
                        :part 'composer
                        :resource '(:media "group-avatar:20001"))))))))))

(ert-deftest qq-chat-reply-media-update-refreshes-composer-precisely ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((type . private) (title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((view (appkit-current-view))
           calls
           (message '((server-id . "m1"))))
       (cl-letf (((symbol-function 'qq-chat--reply-message)
                  (lambda () message))
                 ((symbol-function
                   'qq-media-message-one-line-preview-keys)
                  (lambda (candidate)
                    (should (eq candidate message))
                    '("preview:image")))
                 ((symbol-function 'appkit-request-sync)
                  (lambda (candidate &rest options)
                    (push (cons candidate options) calls))))
         (qq-chat--rerender-open-chats "preview:image")
         (should
          (equal calls
                 (list
                  (list view
                        :part 'composer
                        :resource '(:media "preview:image"))))))))))

(ert-deftest qq-chat-composer-invalidation-refreshes-only-prompt ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (let ((view (appkit-current-view))
           prompt-refreshed-p
           rendered-p)
       (cl-letf (((symbol-function 'qq-chat--refresh-prompt)
                  (lambda () (setq prompt-refreshed-p t)))
                 ((symbol-function 'qq-chat-render)
                  (lambda () (setq rendered-p t))))
         (appkit-request-sync view :part 'composer)
         (qq-chat-test-sync-invalidations)
         (should prompt-refreshed-p)
         (should-not rendered-p))))))

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

(ert-deftest qq-chat-compact-face-message-uses-image-display ()
  "Same-sender face continuations must not render plain [face:id] text."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((dir (make-temp-file "qq-default-emojis" t))
          (png (expand-file-name "178.png" dir))
          (qq-media-default-emoji-directory dir)
          (qq-media--face-names-table (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (puthash "178" "/斜眼笑" qq-media--face-names-table)
           (with-temp-file png
             (set-buffer-multibyte nil)
             (insert (unibyte-string
                      #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a
                      #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
                      #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x01
                      #x08 #x02 #x00 #x00 #x00 #x90 #x77 #x53
                      #xde #x00 #x00 #x00 #x0c #x49 #x44 #x41
                      #x54 #x08 #xd7 #x63 #xf8 #xcf #xc0 #x00
                      #x00 #x00 #x03 #x00 #x01 #x00 #x05 #xfe
                      #xd4 #xef #x00 #x00 #x00 #x00 #x49 #x45
                      #x4e #x44 #xae #x42 #x60 #x82)))
           (puthash
            "private:10001"
            '(((server-id . "m1")
               (sender-id . "10001")
               (sender-name . "Alice")
               (time . 100)
               (raw-message . "hello")
               (segments . (((type . "text")
                             (data . ((text . "hello")))))))
              ((server-id . "m2")
               (sender-id . "10001")
               (sender-name . "Alice")
               (time . 200)
               (raw-message . "[CQ:face,id=178]")
               (preview . "[face:178]")
               (segments . (((type . "face")
                             (data . ((id . "178"))))))))
            qq-state--messages-by-session)
           (with-temp-buffer
             (qq-chat-mode)
             (setq qq-chat--session-key "private:10001")
             (qq-chat--set-history-window "m1" nil)
             (qq-chat-render)
             (goto-char (point-min))
             (should (search-forward "hello" nil t))
             (let ((found nil)
                   (pos (point-min)))
               (while (and (< pos (point-max)) (not found))
                 (when-let* ((disp (get-text-property pos 'display)))
                   (when (and (consp disp) (eq (car disp) 'image))
                     (setq found t)))
                 (setq pos (1+ pos)))
               (should found))
             ;; Must not leave a bare placeholder without display as the only face token.
             (goto-char (point-min))
             (when (search-forward "[face:178]" nil t)
               (should (get-text-property (match-beginning 0) 'display)))))
       (when (file-directory-p dir)
         (delete-directory dir t))))))

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
     (qq-chat--set-history-window "9007199254741004001" nil)
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
     (qq-chat--set-history-window "9007199254741004001" nil)
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
           (qq-chat-react-to-message "178" message))
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

(ert-deftest qq-chat-cached-message-remains-actionable-after-same-slot-restart ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   ;; MESSAGE was cached before the native runtime restarted.  Its stable
   ;; account, conversation, and message identities remain authoritative.
   (let ((message '((server-id . "9007199254741004001")
                    (canonical-row-key . "42")
                    (session-key . "group:20001")
                    (message-seq . "9007199254740999")
                    (native-random . 7)
                    (gateway-account-id . "slot-a")
                    (self-p . t)
                    (time . 100)))
         calls)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-account-current-id)
                  (lambda () "slot-a"))
                 ((symbol-function 'qq-chat--message-at-point)
                  (lambda () message))
                 ((symbol-function 'qq-chat--set-pending-reply)
                  (lambda (selected)
                    (should (eq selected message))
                    (push 'reply calls)))
                 ((symbol-function 'qq-core-message-read-capable-p)
                  (lambda (selected) (eq selected message)))
                 ((symbol-function 'qq-core-mark-message-read)
                  (lambda (selected &optional callback _errback)
                    (should (eq selected message))
                    (push 'read calls)
                    (when callback
                      (funcall
                       callback
                       '((account_id . "slot-a")
                         (row_key . "42"))))))
                 ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                 ((symbol-function 'float-time) (lambda (&optional _) 200))
                 ((symbol-function 'qq-core-recall-message)
                  (lambda (selected &rest _)
                    (should (eq selected message))
                    (push 'recall calls)))
                 ((symbol-function 'qq-core-set-message-reaction)
                  (lambda (selected reaction set &rest _)
                    (should (eq selected message))
                    (should
                     (equal reaction
                            '((emoji-id . "178") (emoji-type . "1"))))
                    (should (eq set t))
                    (push 'reaction calls)))
                 ((symbol-function 'qq-core-set-message-essence)
                  (lambda (selected set &rest _)
                    (should (eq selected message))
                    (should (eq set t))
                    (push 'essence calls)))
                 ((symbol-function 'qq-core-set-message-todo)
                  (lambda (selected operation &rest _)
                    (should (eq selected message))
                    (should (eq operation 'set))
                    (push 'todo calls))))
         (qq-chat-reply-to-message)
         (qq-chat--mark-message-viewed message t)
         (qq-chat--recall-message-internal message)
         (qq-chat-react-to-message "178" message)
         (qq-chat-toggle-message-essence message)
         (qq-chat-set-message-todo message))
       (should (= (length calls) 6))
       (dolist (operation '(reply read recall reaction essence todo))
         (should (memq operation calls)))))))

(ert-deftest qq-chat-latest-idless-canonical-row-is-a-read-target ()
  (qq-chat-test-with-reset
   (let* ((older '((server-id . "9007199254741004001")
                   (canonical-row-key . "41")
                   (session-key . "group:20001")
                   (gateway-account-id . "slot-a")))
          (newest '((canonical-row-key . "42")
                    (session-key . "group:20001")
                    (gateway-account-id . "slot-a")))
          selected)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-chat--timeline-messages)
                  (lambda () (list older newest)))
                 ((symbol-function 'qq-core-message-read-capable-p)
                  (lambda (message)
                    (and (alist-get 'canonical-row-key message) t)))
                 ((symbol-function 'qq-core-mark-message-read)
                  (lambda (message &optional callback _errback)
                    (setq selected message)
                    (when callback
                      (funcall callback
                               '((account_id . "slot-a")
                                 (row_key . "42")))))))
         (qq-chat--mark-latest-window-read))
       (should (eq selected newest))
       (should (equal qq-chat--last-read-target-row-key "42"))))))


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

(ert-deftest qq-chat-delete-is-local-and-never-routes-to-recall ()
  (let ((message '((canonical-row-key . "42")
                   (session-key . "group:20001")))
        deleted recalled)
    (cl-letf (((symbol-function 'qq-chat--message-at-point)
               (lambda () message))
              ((symbol-function 'qq-message-delete-local-capable-p)
               (lambda (value) (eq value message)))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'qq-core-delete-message-local)
               (lambda (value &rest _) (setq deleted value)))
              ((symbol-function 'qq-core-recall-message)
               (lambda (&rest _) (setq recalled t))))
      (qq-chat-delete-message))
    (should (eq deleted message))
    (should-not recalled)))

(ert-deftest qq-chat-routes-group-poke-recall-through-dedicated-operation ()
  (let* ((qq-chat--session-key "group:20001")
         (message
          '((server-id . "9007199254741004001")
            (session-key . "group:20001")
            (gateway-account-id . "slot-a")
            (timeline-class . service)
            (self-p . t)
            (segments
             . (((type . "gray-tip")
                 (data . ((kind . "poke"))))))))
         recalled-message
         ordinary-recall-called)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'qq-message-poke-recall-capable-p)
               (lambda (value) (eq value message)))
              ((symbol-function 'qq-core-recall-poke)
               (lambda (value &rest _)
                 (setq recalled-message value)))
              ((symbol-function 'qq-core-recall-message)
               (lambda (&rest _)
                 (setq ordinary-recall-called t))))
      (qq-chat--recall-message-internal message))
    (should (eq recalled-message message))
    (should-not ordinary-recall-called)))

(ert-deftest qq-chat-recalls-ordinary-message-with-closed-reference ()
  (let ((qq-chat--session-key "group:20001")
        (message '((server-id . "9007199254741004001")
                   (session-key . "group:20001")
                   (gateway-account-id . "slot-a")
                   (self-p . t)
                   (time . 100)
                   (segments . (((type . "text"))))))
        called)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'float-time) (lambda (&optional _) 200))
              ((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-core-recall-message)
               (lambda (selected &rest _) (setq called selected))))
      (qq-chat--recall-message-internal message))
    (should (eq called message))))

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

(ert-deftest qq-chat-poke-recall-does-not-invent-a-client-expiry ()
  (let ((message
         '((server-id . "9007199254741004001")
           (session-key . "group:20001")
           (gateway-account-id . "slot-a")
           (self-p . t)
           (timeline-class . service)
           (segments
            . (((type . "gray-tip")
                (data . ((kind . "poke"))))))))
        prompted
        recalled)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (setq prompted t)
                 t))
              ((symbol-function 'qq-message-poke-recall-capable-p)
               (lambda (value) (eq value message)))
              ((symbol-function 'qq-core-recall-poke)
               (lambda (value &rest _)
                 (setq recalled value))))
      (qq-chat--recall-message-internal message))
    (should prompted)
    (should (eq recalled message))))

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

(ert-deftest qq-chat-media-cache-update-targets-affected-message-anchors ()
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
       (segments . (((type . "face")
                     (data . ((id . "88")))))))
      ((server-id . "m2")
       (sender-id . "10002")
       (sender-name . "Bob")
       (time . 200)
       (raw-message . "plain")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let (requested frame-updated)
       (cl-letf (((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest args)
                    (setq requested
                          (plist-get args :changed-resources))))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (setq frame-updated t))))
         (qq-chat--rerender-open-chats "face:88")
         (qq-chat-test-sync-invalidations)
         (should (equal '((:media "face:88")) requested))
         (should-not frame-updated))))))

(ert-deftest qq-chat-sessions-refresh-does-not-rebuild-composer ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (header-updated frame-updated)
       (cl-letf (((symbol-function 'qq-chat--header-line-update)
                  (lambda () (setq header-updated t)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (setq frame-updated t))))
         (qq-chat--handle-state-change
          '(:type sessions-refreshed :account-id "slot-a" :count 1))
         (qq-chat-test-sync-invalidations)
         (should header-updated)
         (should-not frame-updated))))))

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
       (qq-chat--set-history-window "m1" nil)
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

(ert-deftest qq-chat-first-unread-anchor-does-not-guess-from-count ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001")
      (unread-message-count . 2))
    nil)
   (let ((messages
          '(((server-id . "m1") (self-p . nil) (time . 1))
            ((server-id . "m2") (self-p . nil) (time . 2))
            ((server-id . "m3") (self-p . nil) (time . 3))
            ((server-id . "m-self") (self-p . t) (time . 4)))))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (let ((qq-chat-show-unread-divider t))
         (should-not (qq-chat--first-unread-anchor messages)))))))

(ert-deftest qq-chat-first-unread-anchor-prefers-kernel-position ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001")
      (unread-message-count . 2)
      (first-unread-message-id . "m3"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let ((qq-chat-show-unread-divider t))
       (should (equal "m3"
                      (qq-chat--first-unread-anchor
                       '(((server-id . "m1") (self-p . nil) (time . 1))
                         ((server-id . "m2") (self-p . nil) (time . 2))
                         ((server-id . "m3") (self-p . nil) (time . 3))))))))))


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
     (cl-letf (((symbol-function 'gui-get-selection)
                (lambda (_selection &optional type)
                  (pcase type
                    ('TARGETS ["TARGETS" "image/png" "TIMESTAMP"])
                    ('image/png qq-chat-test--1x1-png)
                    (_ nil)))))
       (qq-chat-attach-clipboard)
       (should (appkit-chatbuf-input-has-objects-p))
       (let ((segments (qq-chat--current-input-segments)))
         (should (= 1 (length segments)))
         (should (equal "image" (alist-get 'type (car segments))))
         (should (file-readable-p
                  (alist-get 'file (alist-get 'data (car segments))))))))))

(ert-deftest qq-chat-image-object-label-uses-one-line-preview-and-file-metadata ()
  (let ((path (make-temp-file "qq-composer-preview" nil ".png")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "123456"))
          (cl-letf (((symbol-function 'qq-media-composer-image-preview)
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

(ert-deftest qq-chat-attach-custom-face-inserts-durable-favorite-segment ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((favorite-id
           "10001_0_0_0_DEADBEEFDEADBEEFDEADBEEFDEADBEEF_0_0")
          (face `((favorite_emoji_id . ,favorite-id)
                  (md5 . "deadbeefdeadbeefdeadbeefdeadbeef")
                  (url . "https://example.invalid/favorite.png"))))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (cl-letf (((symbol-function 'qq-media-ensure-custom-faces)
                  (lambda (callback &optional _errback _force)
                    (funcall callback (list face))))
                 ((symbol-function 'completing-read)
                  (lambda (_prompt table &rest _)
                    (car (all-completions "" table)))))
         (qq-chat-attach-custom-face)
         ;; Media completion queues insertion on the captured view.
         (should-not (qq-chat--current-input-segments))
         (qq-chat-test-sync-until-idle)
         (should
          (equal
           (qq-chat--current-input-segments)
           `(((type . "favorite_emoji")
              (data . ((favorite_emoji_id . ,favorite-id))))))))))))

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
             (cl-letf (((symbol-function 'gui-get-selection)
                        (lambda (_selection &optional type)
                          (pcase type
                            ('TARGETS ["TARGETS" "text/uri-list" "text/plain"])
                            ('text/uri-list (concat "file://" path "\n"))
                            (_ nil)))))
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
     (cl-letf (((symbol-function 'gui-get-selection)
                (lambda (_selection &optional type)
                  (pcase type
                    ('TARGETS ["TARGETS" "image/png"])
                    ('image/png qq-chat-test--1x1-png)
                    (_ nil)))))
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
         (funcall error-fn nil "network failed")
         ;; Failure restores canonical draft state in the callback, but the
         ;; composer projection is owned by the captured-view transaction.
         (should-not (appkit-chatbuf-input-has-objects-p))
         (qq-chat-test-sync-until-idle))
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
       (qq-chat--set-history-window "100" nil)
       (should (equal "100" (qq-chat--message-reply-id reply)))
       (should (equal "42108" (qq-chat--message-reply-sequence reply)))
       (qq-chat-render)
       (goto-char (point-min))
       (should (search-forward "Alice: quoted text" nil t))
       (goto-char (qq-chat--message-position "200"))
       (qq-chat-goto-reply)
       (should
        (equal "100"
               (get-text-property (point) 'qq-chat-message-anchor)))))))

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
       (qq-chat--set-history-window history-anchor "200")
       (qq-chat-render)
       (goto-char (qq-chat--message-position "200"))
       (qq-chat-goto-reply)
       (should
        (equal history-anchor
               (get-text-property (point) 'qq-chat-message-anchor)))))))

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
              #'qq-chat-cancel-dwim)))))))

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


(ert-deftest qq-chat-jump-reports-missing-around-target-without-retry ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001"
           qq-chat--pending-jump-id "100")
     (let ((requests 0)
           failure)
       (cl-letf (((symbol-function 'qq-core-fetch-history-around)
                  (lambda (_session _target callback
                                    &optional _errback _count _sequence
                                    _lifecycle-owner)
                    (cl-incf requests)
                    (funcall callback
                             (qq-chat-test--history-meta
                              "private:10001" :message-count 0))))
                 ((symbol-function 'qq-chat--note-history-window) #'ignore)
                 ((symbol-function 'qq-chat--finish-jump-if-loaded)
                  (lambda (_target &optional _sequence) nil))
                 ((symbol-function 'qq-chat--jump-fail)
                  (lambda (target reason)
                    (setq failure (list target reason)))))
         (qq-chat--seek-history-for-jump
          "private:10001" "100" (current-buffer))
         (should (= requests 1))
         (should-not failure)
         (qq-chat-test-sync-until-idle)
         (should
          (equal failure
                 '("100" "around window omitted target"))))))))


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
                   (lambda (_segment) 'qq-test-image))
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
                   (lambda (_segment &optional _capabilities)
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

(ert-deftest qq-chat-media-card-captures-exact-account-owner ()
  "Closing a view keeps app ownership; a same-id app cannot take it over."
  (let* ((old-app (appkit-app-start 'qq :id 'default :shutdown #'ignore))
         (qq-runtime--app old-app)
         (segment '((type . "video")
                    (data . ((name . "late.mp4")
                             (url . "https://example.com/late.mp4")
                             (remote_status . "available")))))
         replacement view context calls)
    (unwind-protect
        (with-temp-buffer
          (qq-chat-mode)
          (setq qq-chat--session-key "group:20001")
          (setq view
                (appkit-attach-view
                 :app old-app :id (qq-chat--view-id)
                 :state qq-chat--session-key :mode 'qq-chat-mode))
          (setq context (qq-chat--segment-media-card-context segment))
          (should (functionp (plist-get context :open-action)))
          (appkit-kill-view view)
          (cl-letf (((symbol-function 'qq-media-segment-open)
                     (lambda (called-segment &rest keys)
                       (push (list called-segment (plist-get keys :owner))
                             calls))))
            ;; The player owner is the account app, so merely closing this chat
            ;; view does not revoke the captured action.
            (funcall (plist-get context :open-action))
            (should (appkit-app-live-p old-app))
            (should (eq (cadar calls) old-app))
            (should-not (eq (cadar calls) view))

            ;; Replacing the runtime with an equal kind/id app must not alter an
            ;; action rendered by the preceding account app instance.
            (appkit-app-close old-app)
            (setq replacement
                  (appkit-app-start 'qq :id 'default :shutdown #'ignore)
                  qq-runtime--app replacement)
            (funcall (plist-get context :open-action))
            (should (= (length calls) 2))
            (dolist (call calls)
              (should (equal (car call) segment))
              (should (eq (cadr call) old-app))
              (should-not (eq (cadr call) replacement)))))
      (when (appkit-app-live-p old-app)
        (appkit-app-close old-app))
      (when (appkit-app-live-p replacement)
        (appkit-app-close replacement)))))

(ert-deftest qq-chat-media-card-captures-owner-in-shared-nonchat-view ()
  "Shared message renderers bind video actions outside `qq-chat-mode'."
  (let* ((app (appkit-app-start 'qq :id 'shared-render :shutdown #'ignore))
         (segment '((type . "video")
                    (data . ((name . "shared.mp4")
                             (url . "https://example.com/shared.mp4")
                             (remote_status . "available")))))
         context called-owner view)
    (unwind-protect
        (with-temp-buffer
          ;; Forward and Guild forum buffers reuse this renderer but have
          ;; their own modes and view ids.  Only the exact Appkit view is a
          ;; valid source of account ownership here.
          (setq view
                (appkit-attach-view
                 :app app :id '(forward "shared") :mode major-mode))
          (setq context (qq-chat--segment-media-card-context segment))
          (cl-letf (((symbol-function 'qq-media-segment-open)
                     (lambda (_segment &rest keys)
                       (setq called-owner (plist-get keys :owner)))))
            (funcall (plist-get context :open-action))
            (should (eq called-owner app))))
      (when (appkit-view-live-p view)
        (appkit-kill-view view))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

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
                   (lambda (_segment) 'qq-video-preview))
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
     (qq-chat--set-history-window "9007199254750003456" nil)
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

(ert-deftest qq-chat-appkit-responsive-geometry-refreshes-layout ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (should (= line-spacing 0))
     (setq qq-chat--session-key "private:10001"
           fill-column 70)
     (let ((qq-chat-auto-fill-margin-columns 0)
           (width 70)
           (real-request-sync (symbol-function 'appkit-request-sync))
           view calls
           (timeline-syncs 0)
           (frame-syncs 0))
       (cl-letf (((symbol-function 'appkit-geometry-display-window)
                  (lambda (&optional _buffer) (selected-window)))
                 ((symbol-function 'appkit-geometry-window-width)
                  (lambda (candidate &optional margin)
                    (should (eq candidate (selected-window)))
                    (should-not margin)
                    width))
                 ((symbol-function 'appkit-request-sync)
                  (lambda (candidate &rest options)
                    (push (cons candidate options) calls)
                    (apply real-request-sync candidate
                           (append options '(:delay 60)))))
                 ((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest _) (cl-incf timeline-syncs)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (cl-incf frame-syncs))))
         (setq view (qq-chat--ensure-view))
         (should
          (memq #'appkit-view-refresh-responsive-geometry
                window-state-change-functions))
         (setq width 90)
         (run-hook-with-args
          'window-state-change-functions (selected-window))
         (should
          (equal calls (list (list view :part 'geometry :position t))))
         (qq-chat-test-sync-invalidations)
         (should (= fill-column 90))
         (should (= timeline-syncs 1))
         (should (= frame-syncs 1))
         (setq calls nil)
         (run-hook-with-args
          'window-state-change-functions (selected-window))
         (should-not calls)
         (run-hooks 'text-scale-mode-hook)
         (should
          (equal calls (list (list view :part 'geometry :position t))))
         (qq-chat-test-sync-invalidations)
         (should (= timeline-syncs 2))
         (should (= frame-syncs 2)))))))

(ert-deftest qq-chat-history-window-slice-honors-exact-first-and-last ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let ((messages '(((server-id . "m1") (time . 1))
                      ((server-id . "m2") (time . 2))
                      ((server-id . "m3") (time . 3))
                      ((server-id . "m8") (time . 8))
                      ((server-id . "m9") (time . 9)))))
      (qq-chat--set-history-window "m2" "m3")
      (should
       (equal (mapcar #'qq-chat--message-anchor
                      (qq-chat--timeline-messages messages))
              '("m2" "m3")))

      ;; If either exact boundary is missing, continuity with any other cache
      ;; island is unknown; render no substitute slice from that island.
      (qq-chat--set-history-window "missing-first" "m8")
      (should-not (qq-chat--timeline-messages messages))
      (qq-chat--set-history-window "m2" "missing-last")
      (should-not (qq-chat--timeline-messages messages)))))

(ert-deftest qq-chat-partial-window-footer-has-delimiter-not-gap-controls ()
  (let ((app
         (appkit-app-start
          'qq :id (make-symbol "history-footer") :shutdown #'ignore)))
    (unwind-protect
        (with-temp-buffer
          (qq-chat-mode)
          (setq qq-chat--session-key "group:20001"
                fill-column 24)
          (qq-chat--set-history-window "m10" "m20")
          (let ((footer (qq-chat--footer-text)))
            (should (string-match-p "····" footer))
            (should-not
             (string-match-p
              "\\(?:newer messages are not loaded\\|Load newer\\|Latest\\)"
              footer))
            (with-temp-buffer
              (let ((inhibit-read-only t))
                (insert footer))
              (should-not (next-button (point-min)))))
          (let* ((view
                  (appkit-attach-view
                   :app app :id '(chat "group:20001")
                   :state "group:20001" :mode major-mode))
                 (owner
                  (appkit-chat-history-request-start view 'newer)))
            (unwind-protect
                (let ((footer (qq-chat--footer-text)))
                  (should (string-match-p "加载中…" footer))
                  (should-not (string-match-p "loading" footer))
                  (with-temp-buffer
                    (let ((inhibit-read-only t))
                      (insert footer))
                    (should-not (next-button (point-min)))))
              (appkit-chat-history-request-end owner))))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

(ert-deftest qq-chat-history-batch-bounds-require-canonical-batch-members ()
  (qq-chat-test-with-reset
   (puthash
    "group:20001"
    '(((server-id . "m10") (time . 10))
      ((server-id . "m20") (time . 20))
      ((server-id . "m30") (time . 30)))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     ;; Batch wire order carries no chronology; canonical timeline order does.
     (should
      (equal (qq-chat--history-batch-bounds
              '(:batch-message-ids ("m30" "m10")))
             '("m10" . "m30")))
     ;; An empty batch has no boundary, irrespective of cached session rows.
     (should
      (equal (qq-chat--history-batch-bounds
              '(:batch-message-ids nil))
             (cons nil nil))))))

(ert-deftest qq-chat-unified-history-metadata-is-account-conversation-scoped ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (should-error
      (qq-chat--record-gateway-history-range
       (list :history-port-version qq-core-history-port-version
             :history-account-id "slot-b"
             :history-session-key "group:20001"))
      :type 'error)
     (should-error
      (qq-chat--record-gateway-history-range
       (list :history-port-version qq-core-history-port-version
             :history-account-id "slot-a"
             :history-session-key "group:20002"))
      :type 'error))))







(ert-deftest qq-chat-around-success-clears-loading-and-owner ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let (loading-during-request)
       (cl-letf (((symbol-function 'qq-core-fetch-history-around)
                  (lambda (session-key target callback
                                       &optional _errback _count _sequence
                                       _lifecycle-owner)
                    (should (equal session-key "group:20001"))
                    (should (equal target "m20"))
                    (should (appkit-chat-history-request-owner))
                    (setq loading-during-request
                          (appkit-chat-history-loading))
                    ;; Real history callbacks run after the transport has
                    ;; merged the batch into canonical session order.
                    (puthash
                     "group:20001"
                     '(((server-id . "m19") (time . 19))
                       ((server-id . "m20") (time . 20)))
                     qq-state--messages-by-session)
                    (funcall callback
                             (qq-chat-test--history-meta
                              "group:20001"
                              :added-count 2
                              :message-count 2
                              :batch-message-ids '("m19" "m20")))
                    'around-request))
                 ((symbol-function 'qq-chat--finish-jump-if-loaded)
                  (lambda (_target) t))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat--seek-history-for-jump
          "group:20001" "m20" (current-buffer))
         (should (eq loading-during-request 'around))
         (should-not (appkit-chat-history-loading-p))
         (should-not (appkit-chat-history-request-owner))
         (should (equal (appkit-chat-history-window-first-key) "m19"))
         (should-not (appkit-chat-history-window-last-key)))))))

(ert-deftest qq-chat-around-failure-clears-loading-and-owner ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let (loading-during-request failure)
       (cl-letf (((symbol-function 'qq-core-fetch-history-around)
                  (lambda (session-key target _callback
                                       &optional errback _count _sequence
                                       _lifecycle-owner)
                    (should (equal session-key "group:20001"))
                    (should (equal target "m20"))
                    (should (appkit-chat-history-request-owner))
                    (setq loading-during-request
                          (appkit-chat-history-loading))
                    (funcall errback nil "network failure")
                    'around-request))
                 ((symbol-function 'qq-chat--jump-fail)
                  (lambda (target reason)
                    (setq failure (list target reason)))))
         (qq-chat--seek-history-for-jump
          "group:20001" "m20" (current-buffer))
         (should (eq loading-during-request 'around))
         (should-not failure)
         (qq-chat-test-sync-until-idle)
         (should (equal failure '("m20" "network failure")))
         (should-not (appkit-chat-history-loading-p))
         (should-not (appkit-chat-history-request-owner)))))))

(ert-deftest qq-chat-around-batch-advances-captured-frontier-to-normalized-newest ()
  (qq-chat-test-with-reset
   (let ((first "9007199254742007088")
         (captured-latest "9007199254742007089")
         (normalized-newest "9007199254742007090"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,first) (time . 1))
        ((server-id . ,captured-latest) (time . 2))
        ((server-id . ,normalized-newest) (time . 3)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--prepare-around-history-window captured-latest)
       (cl-letf (((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         ;; Wire order and fallback newest are deliberately misleading;
         ;; normalized session order makes NORMALIZED-NEWEST authoritative.
         (qq-chat--note-history-window
          (qq-chat-test--history-meta
           "group:20001"
           :history-has-newer-materialized-p nil
           :added-count 3
           :message-count 3
           :batch-message-ids
           (list normalized-newest captured-latest first)
           :batch-oldest-message-id first
           :batch-newest-message-id captured-latest))
         (should (equal qq-chat--remote-latest-id normalized-newest))
         (should (equal (appkit-chat-history-window-first-key) first))
         (should-not (appkit-chat-history-window-last-key))
         (should (qq-chat--history-window-known-p)))))))

(ert-deftest qq-chat-around-batch-discards-a-proven-stale-remote-frontier ()
  (qq-chat-test-with-reset
   (let ((stale-frontier "9007199254742007080")
         (first "9007199254742007090")
         (newest "9007199254742007092"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,stale-frontier) (time . 1))
        ((server-id . ,first) (time . 2))
        ((server-id . ,newest) (time . 3)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--prepare-around-history-window stale-frontier)
       (cl-letf (((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat--note-history-window
          (qq-chat-test--history-meta
           "group:20001"
           :history-has-newer-materialized-p t
           :added-count 2
           :message-count 2
           :batch-message-ids (list first newest)
           :batch-oldest-message-id first
           :batch-newest-message-id newest))
         ;; Canonical order, rather than snowflake arithmetic, proves that
         ;; the disconnected frontier predates this around window.  Unknown
         ;; is safer and lets a no-progress newer page attach at real latest.
         (should-not qq-chat--remote-latest-id)
         (should (equal (appkit-chat-history-window-first-key) first))
         (should (equal (appkit-chat-history-window-last-key) newest)))))))

(ert-deftest qq-chat-auto-loads-newer-only-near-partial-window-footer ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--ensure-view)
     (qq-chat--set-history-window "m10" "m20")
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
         (should-not calls)
         (qq-chat-test-sync-until-idle)
         (should (equal calls '(t)))

         ;; No-progress against a known different remote frontier suppresses
         ;; automatic retries until the window cursor changes.
         (setq calls nil)
         (appkit-chat-history-newer-stalled-set "m20")
         (qq-chat--maybe-auto-load-newer 975)
         (qq-chat-test-sync-until-idle)
         (should-not calls))))))

(ert-deftest qq-chat-auto-polls-attached-unified-history-tail-with-a-rate-limit ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--gateway-history-newer-cursor
           (qq-chat-test--history-cursor
            "group:20001" 'newer
            '((kind . "timeline_row") (row_key . "105544"))))
     (qq-chat--ensure-view)
     (qq-chat--set-history-window "m10" nil)
     (let ((qq-chat-history-auto-load-threshold 50)
           (qq-chat-history-tail-poll-interval 5)
           (now 100.0)
           calls)
       (cl-letf (((symbol-function 'appkit-chat-timeline-footer-start-position)
                  (lambda () 1000))
                 ((symbol-function 'appkit-chatbuf-composer-idle-p)
                  (lambda () t))
                 ((symbol-function 'float-time)
                  (lambda (&optional _time) now))
                 ((symbol-function 'qq-chat-load-newer-messages)
                  (lambda (&optional quiet) (push quiet calls))))
         (qq-chat--maybe-auto-load-newer 800)
         (qq-chat-test-sync-until-idle)
         (should-not calls)

         (qq-chat--maybe-auto-load-newer 975)
         (qq-chat-test-sync-until-idle)
         (should (equal calls '(t)))

         (qq-chat--maybe-auto-load-newer 975)
         (qq-chat-test-sync-until-idle)
         (should (equal calls '(t)))

         (setq now 105.0)
         (qq-chat--maybe-auto-load-newer 975)
         (qq-chat-test-sync-until-idle)
         (should (equal calls '(t t))))))))

(ert-deftest qq-chat-scroll-observer-loads-newer-from-selected-viewport-edge ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((view (qq-chat--ensure-view))
            (observer qq-chat--scroll-observer))
       (qq-chat--set-history-window "m10" "m20")
       (should (appkit-scroll-observer-p observer))
       (should (appkit-scroll-observer-active-p observer))
       (should (eq view (appkit-scroll-observer-owner observer)))
       (let ((window 'test-window)
             (qq-chat-history-auto-load-threshold 50)
             calls
             checks)
         (cl-letf (((symbol-function 'selected-window) (lambda () window))
                   ((symbol-function
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
           (should-not calls)
           (qq-chat-test-sync-until-idle)
           (should (equal calls '(t)))
           (should (= 1 checks))))))))

(ert-deftest qq-chat-scroll-observer-loads-newer-and-reads-inactive-window ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--ensure-view)
     (qq-chat--set-history-window "m10" "m20")
     (let ((observer qq-chat--scroll-observer)
           (window 'inactive-window)
           (qq-chat-history-auto-load-threshold 50)
           calls
           read-position
           checks)
       (cl-letf (((symbol-function 'selected-window)
                  (lambda () 'selected-window))
                 ((symbol-function 'window-point) (lambda (_window) 700))
                 ((symbol-function 'appkit-chat-timeline-footer-start-position)
                  (lambda () 1000))
                 ((symbol-function 'appkit-chatbuf-composer-idle-p)
                  (lambda () t))
                 ((symbol-function 'qq-chat-load-newer-messages)
                  (lambda (&optional quiet) (push quiet calls)))
                 ((symbol-function 'qq-chat--manage-read-position)
                  (lambda (position) (setq read-position position)))
                 ((symbol-function 'appkit-scroll-observer-check)
                  (lambda (candidate &optional _window)
                    (should (eq candidate observer))
                    (setq checks (1+ (or checks 0))))))
         (funcall (appkit-scroll-observer-end-function observer)
                  window 975 1000)
         (should (= read-position 700))
         (should-not calls)
         (qq-chat-test-sync-until-idle)
         (should (equal calls '(t)))
         (should (= 1 checks)))))))

(ert-deftest qq-chat-scroll-observer-auto-loads-older-near-top ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--ensure-view)
     (qq-chat--set-history-window "m10" nil)
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
         (should-not called)
         (qq-chat-test-sync-until-idle)
         (should (eq called t)))))))

(ert-deftest qq-chat-read-position-follows-cursor-without-regressing ()
  (qq-chat-test-with-reset
   (let ((first "9007199254741004645")
         (second "9007199254741004646")
         (third "9007199254741004647")
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
       (qq-chat--set-history-window first nil)
       (qq-chat-render)
       (cl-letf (((symbol-function 'qq-core-message-read-capable-p)
                  (lambda (_message) t))
                 ((symbol-function 'qq-core-mark-message-read)
                  (lambda (message &optional callback _errback)
                    (setq calls
                          (append
                           calls
                           (list (list (alist-get 'session-key message)
                                       (alist-get 'server-id message)))))
                    (when callback
                      (funcall
                       callback
                       `((account_id . "slot-a")
                         (row_key
                          . ,(alist-get 'canonical-row-key message)))))
                    "read-request")))
         ;; An already-read row does not move the native boundary backward.
         (goto-char (point-min))
         (search-forward "first")
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

(ert-deftest qq-chat-unified-initial-group-history-records-exact-range ()
  (qq-chat-test-with-reset
   (let* ((session-key "group:20001")
          (oldest "7348923749823749801")
          (latest "7348923749823749820")
          (older-cursor
           (qq-chat-test--history-cursor
            session-key 'older
            '((kind . "timeline_row") (row_key . "81"))))
          (newer-cursor
           (qq-chat-test--history-cursor
            session-key 'newer
            '((kind . "timeline_row") (row_key . "100")))))
     (qq-state-upsert-session
      session-key '((type . group) (target-id . "20001")) nil)
     (puthash
      session-key
      (list (qq-chat-test--gateway-message
             session-key oldest "81" 81 "older")
            (qq-chat-test--gateway-message
             session-key latest "100" 100 "latest"))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key)
       (cl-letf (((symbol-function 'qq-core-fetch-history-page)
                  (lambda (_session-key cursor direction callback
                                        &optional _errback _count
                                        lifecycle-owner)
                    (should-not cursor)
                    (should (eq direction 'older))
                    (funcall
                     callback
                     (qq-chat-test--history-meta
                      session-key
                      :history-older-cursor older-cursor
                      :history-newer-cursor newer-cursor
                      :history-has-older-p t
                      :history-has-newer-materialized-p nil
                      :batch-message-ids (list oldest latest)
                      :message-count 2 :added-count 2))
                    nil))
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore)
                 ((symbol-function 'qq-chat--update-frame) #'ignore))
         (qq-chat--load-initial-history (current-buffer) session-key)
         (should (equal qq-chat--gateway-history-older-cursor older-cursor))
         (should (equal qq-chat--gateway-history-newer-cursor newer-cursor))
         (should (equal qq-chat--remote-latest-id latest))
         (should (equal (appkit-chat-history-window-first-key) oldest))
         (should-not (appkit-chat-history-window-last-key))
         (should-not (appkit-chat-history-loading-p)))))))

(ert-deftest qq-chat-dataline-initial-window-uses-unified-durable-history ()
  (qq-chat-test-with-reset
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
                      nil))
                   ((symbol-function 'qq-chat--update-frame) #'ignore)
                   ((symbol-function 'qq-chat-render) #'ignore))
           (qq-chat--load-initial-history (current-buffer) session-key)
           (qq-chat-test-sync-until-idle)
           (should
            (equal history-call
                   (list session-key nil 'older qq-history-fetch-count)))
           (should (equal qq-chat--remote-latest-id message-id))
           (should
            (equal (appkit-chat-history-window-first-key) message-id))
           (should-not (appkit-chat-history-window-last-key))
           (should (appkit-chat-history-older-loaded-p))))))))

(ert-deftest qq-chat-unified-private-history-pages-by-opaque-cursor ()
  (qq-chat-test-with-reset
   (let* ((session-key "private:10001")
          (older-id "7348923749823749822")
          (latest-id "7348923749823749823")
          (first-history-cursor
           (qq-chat-test--history-cursor
            session-key 'older
            '((kind . "timeline_row") (row_key . "500"))))
          older-call)
     (qq-state-upsert-session
      session-key '((type . private) (target-id . "10001")) nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key)
       (cl-letf (((symbol-function 'qq-core-fetch-history-page)
                  (lambda (_session cursor direction callback
                                    &optional _errback count lifecycle-owner)
                    (should (eq direction 'older))
                    (if cursor
                        (progn
                          (setq older-call (list cursor count))
                          (puthash
                           session-key
                           (list (qq-chat-test--gateway-message
                                  session-key older-id "499" 499 "older")
                                 (qq-chat-test--gateway-message
                                  session-key latest-id "500" 500 "latest"))
                           qq-state--messages-by-session)
                          (funcall
                           callback
                           (qq-chat-test--history-meta
                            session-key
                            :history-has-older-p nil
                            :history-has-newer-materialized-p t
                            :batch-message-ids (list older-id)
                            :message-count 1 :added-count 1)))
                      (puthash
                       session-key
                       (list (qq-chat-test--gateway-message
                              session-key latest-id "500" 500 "latest"))
                       qq-state--messages-by-session)
                      (funcall
                       callback
                       (qq-chat-test--history-meta
                        session-key
                        :history-older-cursor first-history-cursor
                        :history-has-older-p t
                        :history-has-newer-materialized-p nil
                        :batch-message-ids (list latest-id)
                        :message-count 1 :added-count 1)))
                    nil))
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore)
                 ((symbol-function 'qq-chat--update-frame) #'ignore))
         (qq-chat--load-initial-history (current-buffer) session-key)
         (should
          (equal qq-chat--gateway-history-older-cursor
                 first-history-cursor))
         (should-not (appkit-chat-history-older-loaded-p))
         (should
          (equal (appkit-chat-history-window-first-key) latest-id))

         (qq-chat-load-older-messages t)
         (should
          (equal older-call
                 (list first-history-cursor qq-history-fetch-count)))
         (should-not qq-chat--gateway-history-older-cursor)
         (should (appkit-chat-history-older-loaded-p))
         (should
          (equal (appkit-chat-history-window-first-key) older-id)))))))

(ert-deftest qq-chat-unified-older-history-extends-one-materialized-page ()
  (qq-chat-test-with-reset
   (let* ((session-key "group:20001")
          (older-id "7348923749823749880")
          (current-id "7348923749823749900")
          (older-cursor
           (qq-chat-test--history-cursor
            session-key 'older
            '((kind . "timeline_row") (row_key . "100"))))
          (next-older-cursor
           (qq-chat-test--history-cursor
            session-key 'older
            '((kind . "timeline_row") (row_key . "80"))))
          (newer-cursor
           (qq-chat-test--history-cursor
            session-key 'newer
            '((kind . "timeline_row") (row_key . "119"))))
          call)
     (qq-state-upsert-session
      session-key '((type . group) (target-id . "20001")) nil)
     (puthash
      session-key
      (list (qq-chat-test--gateway-message
             session-key current-id "100" 100))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key
             qq-chat--gateway-history-older-cursor older-cursor
             qq-chat--gateway-history-newer-cursor newer-cursor)
       (qq-chat--set-history-window current-id nil)
       (appkit-chat-history-older-loaded-set nil)
       (cl-letf (((symbol-function 'qq-core-fetch-history-page)
                  (lambda (_session cursor direction callback
                                    &optional _errback _count
                                    _lifecycle-owner)
                    (setq call (list cursor direction))
                    (puthash
                     session-key
                     (list (qq-chat-test--gateway-message
                            session-key older-id "80" 80)
                           (qq-chat-test--gateway-message
                            session-key current-id "100" 100))
                     qq-state--messages-by-session)
                    (funcall
                     callback
                     (qq-chat-test--history-meta
                      session-key
                      :history-older-cursor next-older-cursor
                      :history-has-older-p t
                      :batch-message-ids (list older-id)
                      :message-count 1 :added-count 1))
                    nil)))
         (qq-chat-load-older-messages t)
         (should (equal call (list older-cursor 'older)))
         (should
          (equal qq-chat--gateway-history-older-cursor next-older-cursor))
         ;; Extending the lower edge must retain the newer cursor.  Replacing
         ;; both edges here made a following forward page restart at 100.
         (should (equal qq-chat--gateway-history-newer-cursor newer-cursor))
         (should (equal (appkit-chat-history-window-first-key) older-id))
         (should-not (appkit-chat-history-older-loaded-p)))))))

(ert-deftest qq-chat-unified-newer-history-uses-opaque-continuation ()
  (qq-chat-test-with-reset
   (let* ((session-key "group:20001")
          (current-id "7348923749823749900")
          (latest-id "7348923749823749905")
          (older-cursor
           (qq-chat-test--history-cursor
            session-key 'older
            '((kind . "timeline_row") (row_key . "81"))))
          (newer-cursor
           (qq-chat-test--history-cursor
            session-key 'newer
            '((kind . "timeline_row") (row_key . "100"))))
          (next-newer-cursor
           (qq-chat-test--history-cursor
            session-key 'newer
            '((kind . "timeline_row") (row_key . "105"))))
          call)
     (qq-state-upsert-session
      session-key '((type . group) (target-id . "20001")) nil)
     (puthash
      session-key
      (list (qq-chat-test--gateway-message
             session-key current-id "100" 100))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key
             qq-chat--gateway-history-older-cursor older-cursor
             qq-chat--gateway-history-newer-cursor newer-cursor)
       ;; An attached window still polls after its exact native sequence;
       ;; `last-key=nil' means no known gap, not "never ask the server again".
       (qq-chat--set-history-window current-id nil)
       (cl-letf (((symbol-function 'qq-core-fetch-history-page)
                  (lambda (_session cursor direction callback
                                    &optional _errback count _lifecycle-owner)
                    (setq call (list cursor direction count))
                    (puthash
                     session-key
                     (list (qq-chat-test--gateway-message
                            session-key current-id "100" 100)
                           (qq-chat-test--gateway-message
                            session-key latest-id "105" 105))
                     qq-state--messages-by-session)
                    (funcall
                     callback
                     (qq-chat-test--history-meta
                      session-key
                      :history-newer-cursor next-newer-cursor
                      :history-has-newer-materialized-p nil
                      :batch-message-ids (list latest-id)
                      :message-count 1 :added-count 1))
                    nil)))
         (qq-chat-load-newer-messages t)
         (should
          (equal call
                 (list newer-cursor 'newer qq-history-fetch-count)))
         ;; Extending the upper edge must retain the older cursor.  Replacing
         ;; both edges here made a following backward page overlap 81..100.
         (should (equal qq-chat--gateway-history-older-cursor older-cursor))
         (should
          (equal qq-chat--gateway-history-newer-cursor next-newer-cursor))
         (should-not (appkit-chat-history-window-last-key))
         (should (equal qq-chat--remote-latest-id latest-id)))))))

(ert-deftest qq-chat-private-window-pages-newer-with-opaque-history-cursor ()
  "Chat forwards the newer cursor without choosing a native driver."
  (qq-chat-test-with-reset
   (let* ((session-key "private:10001")
          (current-id "7348923749823749900")
          (latest-id "7348923749823749905")
          (older-cursor
           (qq-chat-test--history-cursor
            session-key 'older
            '((kind . "timeline_row") (row_key . "81"))))
          (newer-cursor
           (qq-chat-test--history-cursor
            session-key 'newer
            '((kind . "timeline_row") (row_key . "100"))))
          (next-newer-cursor
           (qq-chat-test--history-cursor
            session-key 'newer
            '((kind . "timeline_row") (row_key . "105"))))
          call)
     (qq-state-upsert-session
      session-key '((type . private) (target-id . "10001")) nil)
     (puthash
      session-key
      (list (qq-chat-test--gateway-message
             session-key current-id "100" 100))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key session-key
             qq-chat--gateway-history-older-cursor older-cursor
             qq-chat--gateway-history-newer-cursor newer-cursor)
       (qq-chat--set-history-window current-id current-id)
       (cl-letf (((symbol-function 'qq-core-fetch-history-page)
                  (lambda (_session cursor direction callback
                                    &optional _errback _count
                                    _lifecycle-owner)
                    (setq call (list cursor direction))
                    (puthash
                     session-key
                     (list (qq-chat-test--gateway-message
                            session-key current-id "100" 100)
                           (qq-chat-test--gateway-message
                            session-key latest-id "105" 105))
                     qq-state--messages-by-session)
                    (funcall
                     callback
                     (qq-chat-test--history-meta
                      session-key
                      :history-newer-cursor next-newer-cursor
                      :history-has-newer-materialized-p nil
                      :batch-message-ids (list latest-id)
                      :message-count 1 :added-count 1))
                    nil)))
         (qq-chat-load-newer-messages t)
         (should (equal call (list newer-cursor 'newer)))
         (should (equal qq-chat--gateway-history-older-cursor older-cursor))
         (should
          (equal qq-chat--gateway-history-newer-cursor next-newer-cursor))
         (should-not (appkit-chat-history-window-last-key)))))))

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

(ert-deftest qq-chat-forward-plan-at-point-keeps-snowflake-string ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "9007199254743009336" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "hello")
     (let ((plan (qq-chat--current-forward-plan))
           call)
       (should
        (equal (qq-chat-forward-plan-anchors plan)
               '("9007199254743009336")))
       (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (source target ids callback &optional _errback)
                    (setq call (list source target ids))
                    (funcall callback '((kind . "individual")))
                    'already-settled)))
         (qq-chat-forward-merged plan "private:10002")
         (should
          (equal call
                 '("group:20001" "private:10002"
                   ("9007199254743009336"))))
         (should (seq-every-p #'stringp (nth 2 call)))
         ;; A synchronous callback must not resurrect a stale request token.
         (should-not qq-chat--forward-request)
         (should-not qq-chat--forward-request-owner))))))

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
    (should-not
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

(ert-deftest qq-chat-message-selection-walks-forward-and-renders-as-a-mark ()
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
     (qq-chat--set-history-window "9007199254743009336" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-message-selection)
     (should
      (equal (qq-chat--message-selection-anchors)
             '("9007199254743009336")))
     (should
      (equal (alist-get 'server-id (qq-chat--message-at-point))
             "9007199254743009444"))
     (qq-chat-test-sync-invalidations)
     (goto-char (point-min))
     (search-forward "first")
     (should (eq (get-text-property (point) 'qq-chat-message-selected) t))
     (let* ((message (qq-chat--message-at-point))
            (layout (qq-chat-message-layout message :selected-p t)))
       (should (string-prefix-p "▌ " (plist-get layout :header-prefix)))
       (should
        (string-prefix-p
         "▌ "
         (appkit-ui-prefix-state-current
          (plist-get layout :body-prefix-state))))))))

(ert-deftest qq-chat-merged-forward-preserves-timeline-order-and-clears ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group))
    nil)
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
     (qq-chat--set-history-window "9007199254743009336" nil)
     (qq-chat-render)
     ;; Select newest first to prove send order follows the timeline, not clicks.
     (goto-char (point-min))
     (search-forward "second")
     (qq-chat-toggle-message-selection)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-message-selection)
     (should (= 2 (length (qq-chat-selected-messages))))
     (let (source target captured-ids)
       (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (source-session-key target-session-key ids callback
                                              &optional _errback)
                    (setq source source-session-key
                          target target-session-key
                          captured-ids ids)
                    (funcall
                     callback
                     '((kind . "merged")
                       (message_id . "9007199254743010000")
                       (resource_id . "synthetic-resource"))))))
         (qq-chat-forward-merged
          (qq-chat--current-forward-plan) "group:30001")
         (should (equal source "group:20001"))
         (should (equal target "group:30001"))
         (should
          (equal captured-ids
                 '("9007199254743009336" "9007199254743009444")))
         (should (seq-every-p #'stringp captured-ids))
         (should-not qq-chat--message-selection))))))

(ert-deftest qq-chat-forward-success-callback-requests-exact-appkit-rows ()
  (qq-chat-test-with-reset
   (let ((message-id "9007199254743009336"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Source") (target-id . "20001") (type . group)) nil)
     (puthash
      "group:20001"
      `(((server-id . ,message-id)
         (sender-id . "10001") (sender-name . "Alice")
         (time . 100) (raw-message . "first")))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--set-history-window message-id nil)
       (qq-chat-render)
       (goto-char (appkit-chat-timeline-key-position message-id))
       (qq-chat-toggle-message-selection)
       (let ((plan (qq-chat--current-forward-plan)) success-callback)
         (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                    (lambda (_source _target _ids callback &optional _errback)
                      (setq success-callback callback)
                      'forward-token)))
           (qq-chat-forward-merged plan "group:30001"))
         (let ((view (appkit-current-view))
               (real-invalidate (symbol-function 'appkit-invalidate))
               calls snapshot)
           (should (string-match-p "forwarding" header-line-format))
           (should
            (get-text-property
             (appkit-chat-timeline-key-position message-id)
             'qq-chat-message-selected))
           (cl-letf (((symbol-function 'appkit-request-sync)
                      (lambda (candidate &rest options)
                        (push (cons candidate options) calls)
                        (apply real-invalidate candidate options)))
                     ((symbol-function 'appkit-invalidate)
                      (lambda (&rest _)
                        (ert-fail "forward callback split invalidation")))
                     ((symbol-function 'appkit-schedule-sync)
                      (lambda (&rest _)
                        (ert-fail "forward callback used bare scheduling")))
                     ((symbol-function 'appkit-sync-invalidations)
                      (lambda (&rest _)
                        (ert-fail "forward callback flushed Appkit directly")))
                     ((symbol-function 'appkit-chat-timeline-invalidate)
                      (lambda (&rest _)
                        (ert-fail "forward callback mutated timeline")))
                     ((symbol-function 'qq-chat-render)
                      (lambda () (ert-fail "forward callback rendered")))
                     ((symbol-function 'qq-chat--sync-timeline)
                      (lambda (&rest _)
                        (ert-fail "forward callback projected rows")))
                     ((symbol-function 'qq-chat--header-line-update)
                      (lambda ()
                        (ert-fail "forward callback mutated its header"))))
             (funcall success-callback '((kind . "individual"))))
           (should-not qq-chat--message-selection)
           ;; Generated content and header remain untouched until the view sync.
           (should
            (get-text-property
             (appkit-chat-timeline-key-position message-id)
             'qq-chat-message-selected))
           (should (string-match-p "forwarding" header-line-format))
           (should
            (equal calls
                   (list (list view :part 'frame :entries (list message-id)))))
           (setq snapshot
                 (appkit-invalidations-take
                  (appkit-view-invalidations view)))
           (cl-letf (((symbol-function 'qq-chat-render)
                      (lambda ()
                        (ert-fail "forward sync widened to full render"))))
             (qq-chat--sync-invalidations view snapshot nil))
           (should-not
            (get-text-property
             (appkit-chat-timeline-key-position message-id)
             'qq-chat-message-selected))
           (should-not (string-match-p "forwarding" header-line-format))))))))

(ert-deftest qq-chat-forward-callback-preserves-selection-added-in-flight ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001") (sender-name . "Alice")
       (time . 100) (raw-message . "first"))
      ((server-id . "9007199254743009444")
       (sender-id . "10002") (sender-name . "Bob")
       (time . 101) (raw-message . "second"))
      ((server-id . "9007199254743009555")
       (sender-id . "10003") (sender-name . "Carol")
       (time . 102) (raw-message . "third")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "9007199254743009336" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-message-selection)
     (goto-char (point-min))
     (search-forward "second")
     (qq-chat-toggle-message-selection)
     (let ((plan (qq-chat--current-forward-plan))
           success-callback)
       (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (_source _target _ids callback &optional _errback)
                    (setq success-callback callback)
                    'forward-request)))
         (qq-chat-forward-merged plan "group:30001")
         (should (eq qq-chat--forward-request 'forward-request))
         (should qq-chat--forward-request-owner)
         (should-error
          (qq-chat-forward-merged plan "group:30001")
          :type 'user-error)
         (goto-char (point-min))
         (search-forward "third")
         (qq-chat-toggle-message-selection)
         (funcall success-callback '((kind . "individual")))
         (should-not qq-chat--forward-request)
         (should-not qq-chat--forward-request-owner)
         (should
          (equal (qq-chat--message-selection-anchors)
                 '("9007199254743009555")))
         (should (equal
                  (mapcar #'qq-chat--message-anchor
                          (qq-chat-selected-messages))
                  '("9007199254743009555"))))))))

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

(ert-deftest qq-chat-point-forward-does-not-remove-later-same-anchor-selection ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group)) nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001") (sender-name . "Alice")
       (time . 100) (raw-message . "first")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "9007199254743009336" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "first")
     (let ((plan (qq-chat--current-forward-plan)) success-callback)
       (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (_source _target _ids callback &optional _errback)
                    (setq success-callback callback)
                    'forward-request)))
         (qq-chat-forward-merged plan "group:30001")
         (qq-chat-toggle-message-selection)
         (funcall success-callback '((kind . "individual")))
         (should
          (equal (qq-chat--message-selection-anchors)
                 '("9007199254743009336"))))))))

(ert-deftest qq-chat-forward-callback-keeps-reselected-same-anchor-membership ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group)) nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001") (sender-name . "Alice")
       (time . 100) (raw-message . "first")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "9007199254743009336" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-message-selection)
     (let* ((plan (qq-chat--current-forward-plan))
            (old-owner
             (qq-chat-message-selection-owner
              (car qq-chat--message-selection)))
            success-callback)
       (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (_source _target _ids callback &optional _errback)
                    (setq success-callback callback)
                    'forward-request)))
         (qq-chat-forward-merged plan "group:30001")
         (qq-chat-toggle-message-selection)
         (qq-chat-toggle-message-selection)
         (should-not
          (eq old-owner
              (qq-chat-message-selection-owner
               (car qq-chat--message-selection))))
         (funcall success-callback '((kind . "individual")))
         (should
          (equal (qq-chat--message-selection-anchors)
                 '("9007199254743009336"))))))))

(ert-deftest qq-chat-forward-dispatch-quit-cleans-owner-and-keeps-selection ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group)) nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001") (sender-name . "Alice")
       (time . 100) (raw-message . "first")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "9007199254743009336" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-message-selection)
     (let ((plan (qq-chat--current-forward-plan)))
       (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (&rest _arguments) (signal 'quit nil))))
         (let (quit-seen)
           (condition-case nil
               (qq-chat-forward-merged plan "group:30001")
             (quit (setq quit-seen t)))
           (should quit-seen))
         (should-not qq-chat--forward-request)
         (should-not qq-chat--forward-request-owner)
         (should
          (equal (qq-chat--message-selection-anchors)
                 '("9007199254743009336"))))))))

(ert-deftest qq-chat-forward-header-error-releases-owner-before-dispatch ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let ((plan (qq-chat-test--forward-plan (current-buffer)))
          dispatch-called)
      (cl-letf (((symbol-function 'qq-chat--header-line-update)
                 (lambda () (error "synthetic header failure")))
                ((symbol-function 'qq-message-send-merged-forward)
                 (lambda (&rest _arguments) (setq dispatch-called t))))
        (should-error
         (qq-chat-forward-merged plan "group:30001")))
      (should-not dispatch-called)
      (should-not qq-chat--forward-request)
      (should-not qq-chat--forward-request-owner))))

(ert-deftest qq-chat-forward-post-handoff-quit-retains-installed-owner ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let ((plan (qq-chat-test--forward-plan (current-buffer)))
           (quit-flag nil)
           (dispatch-count 0)
           success-callback
           quit-seen)
       (cl-letf (((symbol-function 'qq-chat--header-line-update) #'ignore)
                 ((symbol-function 'qq-message-send-merged-forward)
                  (lambda (_source _target _ids callback &optional _errback)
                    (cl-incf dispatch-count)
                    (setq success-callback callback)
                    ;; The transport has returned a live token.  Model C-g in
                    ;; the remaining API-to-chat handoff as Emacs does while
                    ;; `inhibit-quit' is non-nil: defer it through `quit-flag'.
                    (should inhibit-quit)
                    (setq quit-flag t)
                    'forward-request)))
         (condition-case nil
             (qq-chat-forward-merged plan "group:30001")
           (quit (setq quit-seen t)))
         (should quit-seen)
         (should-not quit-flag)
         (should (eq qq-chat--forward-request 'forward-request))
         (should qq-chat--forward-request-owner)
         (should-error
          (qq-chat-forward-merged plan "group:30001")
          :type 'user-error)
         (should (= dispatch-count 1))
         (funcall success-callback '((kind . "individual")))
         (should-not qq-chat--forward-request)
         (should-not qq-chat--forward-request-owner))))))

(ert-deftest qq-chat-forward-plan-revalidates-recall-before-dispatch ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((message-id "9007199254743009336")
            (plan
             (qq-chat-test--forward-plan
              (current-buffer) message-id qq-chat--session-key))
            dispatch-called)
       (qq-state-apply-recall qq-chat--session-key message-id)
       (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (&rest _arguments) (setq dispatch-called t))))
         (should-error
          (qq-chat-forward-merged plan "group:30001")
          :type 'user-error))
       (should-not dispatch-called)
       (should-not qq-chat--forward-request-owner)))))

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
                 ((symbol-function 'qq-message-send-merged-forward)
                  (lambda (&rest _arguments) (setq dispatch-called t))))
         (should-error
          (qq-chat-forward-merged plan)
          :type 'user-error))
       (should-not dispatch-called)
       (should-not qq-chat--forward-request-owner)))))

(ert-deftest qq-chat-forward-rechecks-request-owner-after-target-prompt ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let ((plan (qq-chat-test--forward-plan (current-buffer)))
          (recursive-owner (list 'recursive-forward-owner))
          dispatch-called)
      (unwind-protect
          (cl-letf (((symbol-function 'qq-chat--read-forward-target)
                     (lambda (_style _count)
                       (setq qq-chat--forward-request 'recursive-request
                             qq-chat--forward-request-owner recursive-owner)
                       "group:30001"))
                    ((symbol-function 'qq-message-send-merged-forward)
                     (lambda (&rest _arguments) (setq dispatch-called t))))
            (should-error
             (qq-chat-forward-merged plan)
             :type 'user-error)
            (should (eq qq-chat--forward-request 'recursive-request))
            (should (eq qq-chat--forward-request-owner recursive-owner))
            (should-not dispatch-called))
        (setq qq-chat--forward-request nil
              qq-chat--forward-request-owner nil)))))

(ert-deftest qq-chat-forward-failure-preserves-plan-selection ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group)) nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001") (sender-name . "Alice")
       (time . 100) (raw-message . "first")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "9007199254743009336" nil)
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-message-selection)
     (let ((plan (qq-chat--current-forward-plan)) error-callback)
       (cl-letf (((symbol-function 'qq-message-send-merged-forward)
                  (lambda (_source _target _ids _callback &optional errback)
                    (setq error-callback errback)
                    'forward-request))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-forward-merged plan "private:10002")
         (funcall error-callback '((retcode . 200)) "synthetic failure")
         (should-not qq-chat--forward-request)
         (should-not qq-chat--forward-request-owner)
         (should
          (equal (qq-chat--message-selection-anchors)
                 '("9007199254743009336"))))))))

(ert-deftest qq-chat-replacement-view-rejects-older-history-owner ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001") (type . private)) nil)
   (puthash
    "private:10001"
    '(((server-id . "200") (time . 200))
      ((server-id . "300") (time . 300)))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001"
           qq-chat--gateway-history-older-cursor
           (qq-chat-test--history-cursor
            "private:10001" 'older
            '((kind . "timeline_row") (row_key . "200"))))
     (qq-chat--set-history-window "200" "300")
     (let (callback request old-view replacement-view projection-calls)
       (cl-letf (((symbol-function 'qq-core-fetch-history-page)
                  (lambda (_session _cursor direction success
                                    &optional _failure _count lifecycle-owner)
                    (should (eq direction 'older))
                    (setq callback success
                          request
                          (qq-chat-test-native-request
                           "older-private" lifecycle-owner))
                    request)))
         (qq-chat-load-older-messages t))
       (setq old-view (appkit-current-view))
       (appkit-kill-view old-view)
       (should (eq (qq-request-state request) 'cancelled))
       (setq replacement-view (qq-chat--ensure-view))
       (should-not (eq old-view replacement-view))
       ;; The transport may merge canonical data before metadata publication.
       ;; The retired View owner must not move the replacement View's window.
       (puthash
        "private:10001"
        '(((server-id . "150") (time . 150))
          ((server-id . "200") (time . 200))
          ((server-id . "300") (time . 300)))
        qq-state--messages-by-session)
       (cl-letf (((symbol-function 'appkit-request-sync)
                  (lambda (&rest arguments)
                    (push arguments projection-calls))))
         (funcall callback
                  (qq-chat-test--history-meta
                   "private:10001"
                   :history-older-cursor
                   (qq-chat-test--history-cursor
                    "private:10001" 'older
                    '((kind . "timeline_row") (row_key . "150")))
                   :added-count 1
                   :message-count 2
                   :batch-message-ids '("150" "200"))))
       (should-not projection-calls)
       (should-not (appkit-chat-history-loading-p))
       (should-not (appkit-chat-history-request-owner))
       (should (equal (appkit-chat-history-window-first-key) "200"))
       (should (equal (appkit-chat-history-window-last-key) "300"))))))

(ert-deftest qq-chat-replacement-view-rejects-initial-history-owner ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Group") (target-id . "20001") (type . group)) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let (latest-callback initial-request old-view replacement-view
                           projection-calls)
       (cl-letf (((symbol-function 'qq-core-fetch-history-page)
                  (lambda (_session cursor direction success
                                    &optional _failure _count lifecycle-owner)
                    (should-not cursor)
                    (should (eq direction 'older))
                    (setq latest-callback success)
                    (qq-chat-test-native-request
                     "latest-token" lifecycle-owner))))
         (qq-chat--load-initial-history (current-buffer) "group:20001")
         (setq initial-request
               (appkit-handle-object
                (car
                 (appkit-view-operation-handles
                  qq-chat--initial-history-owner))))
         (should (equal (qq-request-token initial-request) "latest-token"))
         (setq old-view (appkit-current-view))
         (appkit-kill-view old-view)
         (should (eq (qq-request-state initial-request) 'cancelled))
         (setq replacement-view (qq-chat--ensure-view))
         (cl-letf (((symbol-function 'appkit-request-sync)
                    (lambda (&rest arguments)
                      (push arguments projection-calls))))
           (puthash
            "group:20001"
            '(((server-id . "300") (message-seq . "300") (time . 300)))
            qq-state--messages-by-session)
           (funcall latest-callback
                    (qq-chat-test--history-meta
                     "group:20001"
                     :history-has-newer-materialized-p nil
                     :added-count 1
                     :message-count 1
                     :batch-message-ids '("300")))))
       (should-not projection-calls)
       (should-not qq-chat--initial-history-owner)
       (should-not (appkit-chat-history-loading-p))
       (should-not (appkit-chat-history-window-first-key))
       (should (eq replacement-view (appkit-current-view)))))))

(ert-deftest qq-chat-replacement-view-settles-forward-owner ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Group") (target-id . "20001") (type . group)) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((anchor "9007199254743009336")
            (membership (car (qq-chat-test--selection anchor)))
            (old-view (qq-chat--ensure-view))
            (owner (list :view old-view))
            replacement-view projection-calls)
       (setq qq-chat--message-selection (list membership)
             qq-chat--forward-request 'forward-token
             qq-chat--forward-request-owner owner)
       (appkit-kill-view old-view)
       (setq replacement-view (qq-chat--ensure-view))
       (cl-letf (((symbol-function 'appkit-request-sync)
                  (lambda (&rest arguments)
                    (push arguments projection-calls))))
         (qq-chat--forward-succeeded
          (current-buffer) "group:20001" owner (list anchor)
          (list (cons anchor
                      (qq-chat-message-selection-owner membership)))
          'individual "private:10002" nil))
       (should-not projection-calls)
       (should-not qq-chat--forward-request)
       (should-not qq-chat--forward-request-owner)
       (should-not qq-chat--forward-sync-request)
       (should-not qq-chat--message-selection)
       (should (equal qq-chat--last-forward-target-key "private:10002"))
       (should (eq replacement-view (appkit-current-view)))))))

(ert-deftest qq-chat-failed-send-survives-replacement-first-render ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001") (type . private)) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--set-empty-history-window)
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
       (should (equal (appkit-chatbuf-input-string) ""))
       (should qq-chat--send-sync-request)
       (setq undo-state (copy-tree buffer-undo-list)
             old-view (appkit-current-view))
       (appkit-kill-view old-view)
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
     (qq-chat--set-empty-history-window)
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
       (setq old-view (appkit-current-view))
       (appkit-kill-view old-view)
       (setq replacement-view (qq-chat--ensure-view))
       ;; This is the reverse ordering: the replacement has already projected
       ;; the successful-send shape before the old transport reports failure.
       (qq-chat-render)
       (should (equal (appkit-chatbuf-input-state) ""))
       (should (equal (appkit-chatbuf-input-string) ""))
       (let ((request-sync (symbol-function 'appkit-request-sync)))
         (cl-letf (((symbol-function 'appkit-request-sync)
                    (lambda (view &rest arguments)
                      (push view sync-views)
                      (apply request-sync view arguments)))
                   ((symbol-function 'qq-api--default-error) #'ignore))
           (funcall failure nil "network failed")))
       (should (equal sync-views (list replacement-view)))
       (should (eq (plist-get qq-chat--send-sync-request :view)
                   replacement-view))
       (should (equal-including-properties
                (appkit-chatbuf-input-state) draft-state))
       (should (equal (appkit-chatbuf-input-string) ""))
       (qq-chat-test-sync-until-idle)
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
       (cl-letf (((symbol-function 'qq-core-set-friend-pinned)
                  (lambda (user-id pinned callback &optional _errback)
                    (push (list user-id pinned) calls)
                    (funcall callback
                             `((user_id . ,user-id)
                               (pinned . ,(if pinned t :false))))
                    'friend-pin-request)))
         (should (qq-chat--friend-pin-capable-p))
         (should (eq (qq-chat-pin-friend) 'friend-pin-request))
         (should (eq (qq-chat-unpin-friend) 'friend-pin-request)))
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

(provide (quote qq-chat-test))

;;; qq-chat-test.el ends here
