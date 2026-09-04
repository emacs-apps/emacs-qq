;;; qq-root-test.el --- Tests for qq-root -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-core)
(require 'qq-root)
(require 'qq-runtime)
(require 'qq-state)

(defmacro qq-root-test-with-reset (&rest body)
  "Run BODY with a clean in-memory qq-state store."
  `(let ((qq-state-change-hook nil)
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
         (qq-root--scope "slot-a")
         (qq-runtime--account-id "slot-a"))
     (qq-state-select-account "slot-a")
     (qq-state-reset)
     (unwind-protect
         (progn ,@body)
       (qq-state-reset))))

(defun qq-root-test-set-recent (&rest session-keys)
  "Set ordered recent membership to SESSION-KEYS in the current partition."
  (setq qq-state--recent-session-keys (copy-sequence session-keys))
  (clrhash qq-state--recent-session-key-set)
  (dolist (session-key session-keys)
    (puthash session-key t qq-state--recent-session-key-set)))

(defun qq-root-test-session-preview-text (session)
  "Flatten SESSION's projected one-line preview for assertions."
  (let* ((preview (qq-root--session-preview-model session))
         (label (or (appkit-ui-one-line-preview-label preview) ""))
         (separator (or (appkit-ui-one-line-preview-separator preview) ""))
         (text (or (appkit-ui-one-line-preview-text preview) "")))
    (appkit-ui-one-line-text
     (if (string-empty-p label)
         text
       (concat label separator
               (unless (string-empty-p text) (concat " " text)))))))

(defmacro qq-root-test-with-live-view (&rest body)
  "Run BODY in an isolated root Surface with lexical `app', `buffer', `view'."
  (declare (indent 0) (debug t))
  `(let* ((qq-root-buffer-name
           (generate-new-buffer-name "qq-root-test"))
          (qq-runtime--accounts (make-hash-table :test #'equal))
          (runtime (qq-runtime-ensure-account "slot-a"))
          (app (qq-runtime-account-app runtime))
          (buffer (generate-new-buffer (format "*qq-root:%s*" qq-root-buffer-name)))
          view)
     (unwind-protect
         (cl-letf (((symbol-function 'qq-runtime-account-display-name)
                    (lambda (&optional _account-id) qq-root-buffer-name)))
           (with-current-buffer buffer
             (qq-root-mode)
             (qq-runtime-bind-account "slot-a")
             (setq-local qq-root--scope "slot-a")
             (setq view
                   (qq-runtime-ensure-account-surface
                    :id 'root
                    :mode 'qq-root-mode
                    :render-function #'qq-root--render))
             ,@body))
       (when (appkit-app-live-p app)
         (appkit-app-close app))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(ert-deftest qq-root-refresh-uses-the-native-product-operation ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'qq-core-refresh)
               (lambda () (cl-incf calls) 'requests)))
      (should (equal (qq-root-refresh) 'requests))
      (should (= calls 1)))))

(ert-deftest qq-root-header-shows-gateway-online-account-before-self-info ()
  (let ((account
         '((account_id . "slot-work")
           (label . "Work")
           (phase . "online")
           (uin . "10001"))))
    (cl-letf (((symbol-function 'qq-state-self-info) #'ignore)
              ((symbol-function 'qq-state-connection-status)
               (lambda () 'ready))
              ((symbol-function 'qq-account-get)
               (lambda (_account-id) account))
              ((symbol-function 'qq-account-list)
               (lambda () (list account))))
      (let ((qq-root--scope "slot-work"))
        (should
         (equal (qq-root--header-line)
                " emacs-qq  [ready]  Work (10001) — online"))))))

(ert-deftest qq-root-header-rejects-stale-self-info-after-account-switch ()
  (let ((selected
         '((account_id . "slot-work")
           (label . "Work")
           (phase . "online")
           (uin . "10001")))
        (other
         '((account_id . "slot-personal")
           (label . "Personal")
           (phase . "stopped")
           (uin . "20002"))))
    (cl-letf (((symbol-function 'qq-state-self-info)
               (lambda ()
                 '((user_id . "20002") (nickname . "Old account"))))
              ((symbol-function 'qq-state-connection-status)
               (lambda () 'ready))
              ((symbol-function 'qq-account-get)
               (lambda (_account-id) selected))
              ((symbol-function 'qq-account-list)
               (lambda () (list selected other))))
      (let ((qq-root--scope "slot-work"))
        (should
         (equal (qq-root--header-line)
                " emacs-qq  [ready]  Work (10001) — online · 1/2 online"))))))

(ert-deftest qq-root-projects-only-authoritative-recent-membership-in-page-order ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "group:3"
    '((type . group) (title . "Directory only") (last-message-time . 30))
    nil)
   (qq-state-upsert-session
    "private:2"
    '((type . private) (title . "Second") (last-message-time . 20))
    nil)
   (qq-state-upsert-session
    "group:1"
    '((type . group) (title . "First") (last-message-time . 10))
    nil)
   (qq-root-test-set-recent "group:1" "private:2")
   (should
    (equal (mapcar (lambda (session) (alist-get 'key session))
                   (qq-root--recent-sessions))
           '("group:1" "private:2")))
   (cl-letf (((symbol-function 'qq-root--buffer-width) (lambda () 80)))
     (should
      (equal
       (seq-keep
        (lambda (entry)
          (and (eq (qq-root--entry-type entry) 'session)
               (alist-get 'key (qq-root--entry-session entry))))
        (qq-root--project-account-entries))
       '("group:1" "private:2"))))))

(ert-deftest qq-root-distinguishes-important-and-muted-unread-sessions ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "group:10001"
    '((unread-badge-count . 3) (muted-p . nil))
    nil)
   (qq-state-upsert-session
    "group:10002"
    '((unread-badge-count . 9) (muted-p . t))
    nil)
   (qq-root-test-set-recent "group:10001" "group:10002")
   (let ((metrics (qq-root--activity-metrics)))
     (should (= 2 (plist-get metrics :unread)))
     (should (= 1 (plist-get metrics :important)))
     (should (= 1 (plist-get metrics :muted))))))

(ert-deftest qq-root-aggregate-metrics-do-not-present-partial-badge-totals ()
  (let* ((sessions '(((type . group) (unread-badge-count . 3) (muted-p . nil))
                     ((type . private) (unread-badge-count . nil) (muted-p . nil))))
         (metrics (qq-root--activity-metrics sessions)))
    (should (= 2 (plist-get metrics :all)))
    (should-not (plist-get metrics :unread))
    (should-not (plist-get metrics :important))
    (should-not (plist-get metrics :muted))
    (should
     (string-match-p "Important:?" (qq-root--filters-line sessions)))))

(ert-deftest qq-root-badge-never-falls-back-to-message-count ()
  (let ((unknown-badge '((unread-message-count . 7)
                         (unread-badge-count . nil)
                         (muted-p . nil)))
        (exact-badge '((unread-message-count . 7)
                       (unread-badge-count . 9)
                       (muted-p . nil))))
    (should (equal "" (qq-root--session-unread-trail unknown-badge)))
    (should (string-match-p "9" (qq-root--session-unread-trail exact-badge)))
    (should-not
     (string-match-p "7" (qq-root--session-unread-trail exact-badge)))))

(ert-deftest qq-root-renders-capped-badges ()
  (should
   (equal "99+"
          (substring-no-properties
           (qq-root--session-unread-trail
            '((unread-badge-count . 218)))))))

(ert-deftest qq-root-projects-the-scannable-login-view ()
  (let ((model '(:account-id "slot-a"
                 :status "Waiting for mobile QQ confirmation…"
                 :display "[QR]\n")))
    (cl-letf (((symbol-function 'qq-login-view-model)
               (lambda () model)))
      (let* ((qq-root--scope 'gateway)
             (entries (qq-root--project-entries))
             (login
              (seq-find
               (lambda (entry)
                 (eq (qq-root--entry-type entry) 'login))
               entries)))
        (should login)
        (should (equal (qq-root--entry-text login) model))
        (with-temp-buffer
          (qq-root--entry-printer login)
          (should
           (string-prefix-p
            "Waiting for mobile QQ confirmation…\n[QR]\n"
            (buffer-string)))
          (should (string-match-p
                   "Scan with mobile QQ"
                   (buffer-string))))))))

(ert-deftest qq-root-renders-muted-unread-in-title-trail ()
  (let* ((session '((key . "group:muted")
                    (type . group)
                    (unread-badge-count . 9)
                    (muted-p . t)
                    (last-message-preview . "quiet message")))
         (row (qq-root--session-one-line-row session))
         (trail (appkit-presentation-one-line-row-context-trail row)))
    (should (equal "9" (substring-no-properties trail)))
    (should (eq 'qq-root-muted-count (get-text-property 0 'face trail)))
    (should (equal "quiet message"
                   (appkit-ui-one-line-preview-text (appkit-presentation-one-line-row-preview row))))
    (should-not (appkit-presentation-one-line-row-time-tail-face row))))

(ert-deftest qq-root-muted-session-without-unread-has-no-activity-trail ()
  (let ((session '((muted-p . t) (unread-badge-count . 0))))
    (should (equal "" (qq-root--session-unread-trail session)))
    (should (equal "" (qq-root-test-session-preview-text session)))))

(ert-deftest qq-root-session-preview-is-always-one-line ()
  (should (equal "first second third"
                 (qq-root-test-session-preview-text
                  '((last-message-preview . " first\nsecond\r\n  third "))))))

(ert-deftest qq-root-group-preview-shows-latest-sender ()
  (let* ((session '((type . group)
                    (last-message-sender-name . " Alice\n")
                    (last-message-preview . "first\nsecond")))
         (preview-model (qq-root--session-preview-model session))
         (row (qq-root--session-one-line-row session)))
    (should (equal "first second"
                   (appkit-ui-one-line-preview-text preview-model)))
    (should (equal "Alice"
                   (appkit-ui-one-line-preview-label preview-model)))
    (should (equal ":"
                   (appkit-ui-one-line-preview-separator preview-model)))
    (should
     (equal (list (appkit-name-color-face "Alice") 'qq-msg-user-title)
            (appkit-ui-one-line-preview-label-face preview-model)))
    (should (equal "Alice"
                   (appkit-ui-one-line-preview-label
                    (appkit-presentation-one-line-row-preview row))))
    (should
     (equal (list (appkit-name-color-face "Alice") 'qq-msg-user-title)
            (appkit-ui-one-line-preview-label-face
             (appkit-presentation-one-line-row-preview row))))))

(ert-deftest qq-root-preview-label-face-uses-summary-sender-identity ()
  (let ((session '((type . group)
                   (last-message-sender-id . "42")
                   (last-message-sender-name . "Alice")
                   (last-message-preview . "hello"))))
    (should
     (equal (list (appkit-name-color-face "42") 'qq-msg-user-title)
            (appkit-ui-one-line-preview-label-face
             (qq-root--session-preview-model session))))))

(ert-deftest qq-root-preview-label-face-prefers-cached-sender-identity ()
  (let ((session '((key . "group:42")
                   (type . group)
                   (last-message-id . "m1")
                   (last-message-sender-name . "Alice")
                   (last-message-preview . "hello")))
        (message '((server-id . "m1")
                   (sender-id . "42")
                   (sender-name . "Alice")
                   (self-p . nil))))
    (cl-letf (((symbol-function 'qq-state-session-messages)
               (lambda (_session-key) (list message))))
      (should
       (equal (qq-chat--message-title-face message)
              (appkit-ui-one-line-preview-label-face
               (qq-root--session-preview-model session)))))))

(ert-deftest qq-root-session-preview-projects-cached-message-media ()
  (let* ((qq-chat-show-peer-actions nil)
         (session
          '((key . "group:42")
            (type . group)
            (last-message-id . "m1")
            (last-message-sender-name . "Alice")
            (last-message-preview . "[image]")))
         (message
          '((server-id . "m1")
            (segments
             . (((type . "image")
                 (data . ((file . "cached.png"))))))))
         (image '(image :type png :data "bytes")))
    (cl-letf (((symbol-function 'qq-state-session-messages)
               (lambda (_session-key) (list message)))
              ((symbol-function
                'qq-media-segment-one-line-preview-image)
               (lambda (_segment) image)))
      (let ((preview (qq-root--session-preview-model session)))
        (should (equal "Alice"
                       (appkit-ui-one-line-preview-label preview)))
        (should (equal ":"
                       (appkit-ui-one-line-preview-separator preview)))
        (let ((display
               (get-text-property
                0 'display
                (appkit-ui-one-line-preview-visual preview))))
          (should (eq 'slice (caar display)))
          (should
           (equal "bytes"
                  (plist-get (cdr (cadr display)) :data))))
        (should
         (equal "[image]"
                (appkit-ui-one-line-preview-text preview)))
        (should
         (equal "Alice: [image]"
                (qq-root-test-session-preview-text session)))))))

(ert-deftest qq-root-private-preview-shows-sender-only-when-outgoing ()
  (let ((incoming '((type . private)
                    (last-message-sender-name . "Alice")
                    (last-message-self-p . nil)
                    (last-message-preview . "hello")))
        (outgoing '((type . private)
                    (last-message-sender-name . "Me")
                    (last-message-self-p . t)
                    (last-message-preview . "hello"))))
    (should (equal "hello" (qq-root-test-session-preview-text incoming)))
    (should (equal "Me: hello" (qq-root-test-session-preview-text outgoing)))
    (should
     (equal (list (appkit-name-color-face "Me") 'qq-msg-self-title)
            (appkit-ui-one-line-preview-label-face
             (qq-root--session-preview-model outgoing))))))

(ert-deftest qq-root-service-and-dataline-previews-omit-sender ()
  (dolist (type '(service dataline))
    (should
     (equal "Henrik: subject"
            (qq-root-test-session-preview-text
             `((type . ,type)
               (last-message-sender-name . "QQ Mail")
               (last-message-self-p . t)
               (last-message-preview . "Henrik: subject")))))))

(ert-deftest qq-root-does-not-invent-a-missing-message-preview ()
  (should (equal ""
                 (qq-root-test-session-preview-text
                  '((type . group)
                    (last-message-id . "9007199254741004991")
                    (last-message-sender-name . "Alice"))))))

(ert-deftest qq-root-mentions-stay-important-through-muted-groups ()
  (let ((session '((muted-p . t)
                   (unread-badge-count . 9)
                   (unread-at-me-message-seq . "10001")
                   (unread-at-all-message-seq . "10002"))))
    (should (qq-root--session-important-unread-p session))
    (let ((trail (qq-root--session-unread-trail session)))
      (should (equal "9 @ @all" (substring-no-properties trail)))
      (should (eq 'qq-root-muted-count
                  (get-text-property 0 'face trail)))
      (should (eq 'qq-root-mention-count
                  (get-text-property 2 'face trail)))
      (should (eq 'qq-root-mention-count
                  (get-text-property 4 'face trail))))))

(ert-deftest qq-root-inserts-badge-count-inside-title-brackets ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
               (lambda (_session) "#"))
              ((symbol-function 'qq-root--buffer-width) (lambda () 80)))
      (qq-root--insert-session-line
       '((key . "group:1")
         (type . group)
         (title . "Example Group")
         (unread-badge-count . 3)
         (muted-p . t)
         (last-message-preview . "[image]"))))
    (should (string-match-p
             "\\[Example Group +3\\] \\[image\\]"
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest qq-root-session-row-keeps-help-without-blanket-hover ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
               (lambda (_session) "#"))
              ((symbol-function 'qq-root--buffer-width) (lambda () 80)))
      (qq-root--insert-session-line
       '((key . "group:1")
         (type . group)
         (title . "Group")
         (last-message-preview . "hello"))))
    (should (equal "Open group:1"
                   (get-text-property (point-min) 'help-echo)))
    (should-not (text-property-not-all
                 (point-min) (point-max) 'mouse-face nil))))

(ert-deftest qq-root-background-sync-reuses-last-visible-width ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((qq-root--fill-column 88)
           (compute-calls 0))
       (cl-letf (((symbol-function 'qq-root--selected-window) (lambda () nil))
                 ((symbol-function 'qq-root--display-window) (lambda () nil))
                 ((symbol-function 'qq-root--compute-fill-column)
                  (lambda (&optional _window)
                    (cl-incf compute-calls)
                    42)))
         (should (= (qq-root--stable-fill-column) 88))
         (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
         (should (= qq-root--fill-column 88))
         (should (= compute-calls 0)))))))

(ert-deftest qq-root-mode-disables-undo-history ()
  (with-temp-buffer
    (qq-root-mode)
    (should (eq buffer-undo-list t))))

(ert-deftest qq-root-line-property-falls-back-on-the-probe-line ()
  (with-temp-buffer
    (insert "first row\nsecond row\n")
    (add-text-properties
     (point-min) (1+ (point-min))
     '(qq-root-session-key "first"))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (add-text-properties
       (point) (1+ (point))
       '(qq-root-session-key "second")))
    (goto-char (point-min))
    (forward-line 1)
    (should (equal "first" (qq-root--session-key-at-point 4)))
    (should (equal "second" (qq-root--session-key-at-point)))))

(ert-deftest qq-root-projects-navigation-without-a-key-cheat-sheet ()
  (qq-root-test-with-reset
   (with-temp-buffer
     (cl-letf (((symbol-function 'qq-root--buffer-width) (lambda () 80)))
       (let* ((entries (qq-root--project-entries))
              (texts (delq nil (mapcar #'qq-root--entry-text entries))))
         (should-not (seq-some (lambda (text)
                                 (string-match-p "g refresh\\|Press `g`" text))
                               texts)))))))

(ert-deftest qq-root-sync-preserves-nodes-and-never-erases-buffer ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1"
    '((type . private) (target-id . "1") (title . "One")
      (last-message-time . 2) (last-message-preview . "old"))
    nil)
   (qq-state-upsert-session
    "group:2"
    '((type . group) (target-id . "2") (title . "Two")
      (last-message-time . 1) (last-message-preview . "quiet"))
    nil)
   (qq-root-test-set-recent "private:1" "group:2")
   (qq-root-test-with-live-view
     (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                (lambda (_session) "#")))
       (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
       (let ((one-node (gethash '(session . "private:1") qq-root--node-table))
             (two-node (gethash '(session . "group:2") qq-root--node-table)))
         (should one-node)
         (should two-node)
         (qq-state-upsert-session
          "private:1" '((last-message-preview . "updated")) nil)
         (cl-letf (((symbol-function 'erase-buffer)
                    (lambda () (ert-fail "incremental sync erased the buffer"))))
           (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t))))
         (should (eq one-node
                     (gethash '(session . "private:1") qq-root--node-table)))
         (should (eq two-node
                     (gethash '(session . "group:2") qq-root--node-table)))
         (should (string-match-p "updated" (buffer-string)))
         (should-not (string-match-p "old" (buffer-string)))
         (should (eq buffer-undo-list t)))))))

(ert-deftest qq-root-selected-window-refreshes-cached-width ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((qq-root--fill-column 88))
       (cl-letf (((symbol-function 'qq-root--selected-window)
                  (lambda () 'root-window))
                 ((symbol-function 'qq-root--compute-fill-column)
                  (lambda (&optional _window) 104)))
         (should (qq-root--reflow-visible))
         (should (= qq-root--fill-column 104)))))))

(ert-deftest qq-root-media-update-targets-only-owning-session-node ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1" '((type . private) (target-id . "1") (title . "One")) nil)
   (qq-state-upsert-session
    "group:2" '((type . group) (target-id . "2") (title . "Two")) nil)
   (qq-root-test-set-recent "private:1" "group:2")
   (qq-root-test-with-live-view
     (let ((private-icon "old-private") (group-icon "old-group"))
       (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                  (lambda (session)
                    (if (eq (alist-get 'type session) 'group)
                        group-icon private-icon))))
         (appkit-surface-send
          view (list 'qq-render (appkit-projection-change-create :full-p t)))
         (setq private-icon "new-private" group-icon "new-group")
         (qq-root--handle-media-cache-update "avatar:1")
         (should (string-match-p "new-private" (buffer-string)))
         (should (string-match-p "old-group" (buffer-string)))
         (should-not (string-match-p "new-group" (buffer-string)))
         (qq-root--handle-media-cache-update "group-avatar:2")
         (should (string-match-p "new-group" (buffer-string)))
         (should-not (string-match-p "old-group" (buffer-string))))))))

(ert-deftest qq-root-state-events-require-the-exact-account-owner ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((before (buffer-string)))
       (qq-root--handle-state-change
        '(:type message :session-key "private:7"))
       (qq-root--handle-state-change
        '(:type message :account-id "slot-b" :session-key "private:7"))
       (should (equal before (buffer-string))))
     (qq-state-upsert-session
      "private:7" '((type . private) (target-id . "7") (title . "Exact owner")) nil)
     (qq-root-test-set-recent "private:7")
     (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                (lambda (_session) "#")))
       (qq-root--handle-state-change
        '(:type message :account-id "slot-a" :session-key "private:7")))
     (should (string-match-p "Exact owner" (buffer-string))))))

(ert-deftest qq-root-header-event-does-not-touch-ewoc-content ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (appkit-surface-send
      view (list 'qq-render (appkit-projection-change-create :full-p t)))
     (goto-char (point-max))
     (let ((before (buffer-string)) (position (point)))
       (qq-root--handle-state-change '(:type self-info :account-id "slot-a"))
       (should (equal before (buffer-string)))
       (should (= position (point)))))))

(ert-deftest qq-root-gateway-account-event-preserves-account-root-content ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (appkit-surface-send
      view (list 'qq-render (appkit-projection-change-create :full-p t)))
     (let ((before (buffer-string)))
       (qq-root--handle-gateway-account-change 'changed "slot-a")
       (should (equal before (buffer-string)))))))

(ert-deftest qq-root-structural-sync-preserves-semantic-point-after-reorder ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1"
    '((type . private) (target-id . "1") (title . "One")
      (last-message-time . 2) (last-message-preview . "first"))
    nil)
   (qq-state-upsert-session
    "group:2"
    '((type . group) (target-id . "2") (title . "Two")
      (last-message-time . 1) (last-message-preview . "second"))
    nil)
   (qq-root-test-set-recent "private:1" "group:2")
   (qq-root-test-with-live-view
     (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                (lambda (_session) "#")))
       (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
       (goto-char
        (ewoc-location
         (gethash '(session . "group:2") qq-root--node-table)))
       (should (equal "group:2" (qq-root--session-key-at-point)))
       (qq-state-upsert-session
        "group:2" '((last-message-time . 3)) nil)
      ;; Root order belongs to the recent projection, not to all-session
      ;; timestamp sorting.  Simulate the newer authoritative page order.
       (qq-root-test-set-recent "group:2" "private:1")
       (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t)))
       (should (equal "group:2" (qq-root--session-key-at-point)))
       (should
        (equal '(session . "group:2")
               (qq-root--entry-key
                (ewoc-data (ewoc-nth qq-root--ewoc 3)))))))))

(ert-deftest qq-root-dead-view-makes-all-external-callbacks-inert ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (appkit-surface-stop view)
     (cl-letf (((symbol-function 'appkit-surface-send)
                (lambda (&rest _args)
                  (ert-fail "dead root view requested a sync")))
               ((symbol-function 'qq-root--selected-window)
                (lambda () 'root-window))
               ((symbol-function 'qq-root--compute-fill-column)
                (lambda (&optional _window) 100)))
       (qq-root--handle-state-change
        '(:type connection :account-id "slot-a"))
       (qq-root--handle-state-change
        '(:type message :account-id "slot-a" :session-key "group:1"))
       (qq-root--handle-media-cache-update "avatar:1")
       (should-not (qq-root--reflow-visible))))))

(ert-deftest qq-root-hooks-do-not-create-a-runtime-app-when-closed ()
  (qq-root-test-with-reset
   (let ((qq-runtime--app nil))
     (cl-letf (((symbol-function 'qq-runtime-app)
                (lambda ()
                  (ert-fail "closed-root hook created a runtime app")))
               ((symbol-function 'appkit-surface-send)
                (lambda (&rest _args)
                  (ert-fail "closed-root hook requested a sync"))))
       (qq-root--handle-state-change
        '(:type connection :account-id "slot-a"))
       (qq-root--handle-state-change
        '(:type message :account-id "slot-a" :session-key "group:1"))
       (qq-root--handle-media-cache-update "avatar:1"))
     (should-not qq-runtime--app))))

(ert-deftest qq-root-open-reattaches-and-reuses-one-appkit-view ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((qq-runtime--app app)
           (dead view)
           reopened reused)
       (appkit-surface-stop dead)
       (save-window-excursion
         (setq reopened (qq-root-open)))
       (should (eq reopened buffer))
       (setq view (with-current-buffer buffer (appkit-current-surface)))
       (should (appkit-surface-live-p view))
       (should-not (eq dead view))
       (should (equal 'root (appkit-surface-identity view)))
       (should-not (string-empty-p
                    (with-current-buffer buffer (buffer-string))))
       (save-window-excursion
         (qq-root-open))
       (setq reused (with-current-buffer buffer (appkit-current-surface)))
       (should (eq view reused))))))

(ert-deftest qq-root-renamed-buffer-keeps-hooks-geometry-and-reopen-owned ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1" '((type . private) (target-id . "1") (title . "One")) nil)
   (qq-root-test-set-recent "private:1")
   (qq-root-test-with-live-view
     (let ((renamed (generate-new-buffer-name " *qq-root-renamed*"))
           (icon "old-icon") reopened)
       (rename-buffer renamed)
       (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                  (lambda (_session) icon))
                 ((symbol-function 'qq-root--selected-window)
                  (lambda () 'renamed-root-window))
                 ((symbol-function 'qq-root--compute-fill-column)
                  (lambda (&optional _window) 100)))
         (qq-root--handle-state-change
          '(:type message :account-id "slot-a" :session-key "private:1"))
         (should (string-match-p "old-icon" (buffer-string)))
         (setq icon "new-icon")
         (qq-root--handle-media-cache-update "avatar:1")
         (should (string-match-p "new-icon" (buffer-string)))
         (should (qq-root--reflow-visible t))
         (should (= qq-root--fill-column 100))
         (save-window-excursion
           (setq reopened (qq-root-open))))
       (should (eq buffer reopened))
       (should (equal renamed (buffer-name reopened)))
       (should (eq view (with-current-buffer reopened (appkit-current-surface))))))))

(ert-deftest qq-root-keeps-manager-and-two-account-roots-live-together ()
  (let ((qq-runtime--app nil)
        (qq-runtime--accounts (make-hash-table :test #'equal))
        (qq-state--partitions (make-hash-table :test #'equal))
        (qq-state--active-account-id nil)
        (qq-account--accounts (make-hash-table :test #'equal))
        (qq-account--account-order nil)
        (qq-account--current-account-id nil)
        (qq-account-registry-changed-hook nil)
        (qq-account-selection-changed-hook nil)
        (qq-state-change-hook nil)
        manager root-a root-b)
    (unwind-protect
        (progn
          (qq-account--replace-accounts
           '(((account_id . "slot-a") (label . "Work")
              (phase . "online") (uin . "10001"))
             ((account_id . "slot-b") (label . "Personal")
              (phase . "online") (uin . "20002")))
           'ready "gateway-test")
          (qq-runtime-with-account "slot-a"
            (qq-state-upsert-session
             "private:1"
             '((type . private) (target-id . "1") (title . "Alice A"))
             nil)
            (qq-root-test-set-recent "private:1"))
          (qq-runtime-with-account "slot-b"
            (qq-state-upsert-session
             "private:1"
             '((type . private) (target-id . "1") (title . "Alice B"))
             nil)
            (qq-root-test-set-recent "private:1"))
          (cl-letf
              (((symbol-function 'qq-login-view-model) #'ignore)
               ((symbol-function 'qq-server-state)
                (lambda () 'ready)))
            (save-window-excursion
              (setq manager (qq-root-open 'gateway)
                    root-a (qq-root-open "slot-a")
                    root-b (qq-root-open "slot-b"))))
          (should (buffer-live-p manager))
          (should (buffer-live-p root-a))
          (should (buffer-live-p root-b))
          (should-not (eq manager root-a))
          (should-not (eq root-a root-b))
          (with-current-buffer manager
            (should (eq qq-root--scope 'gateway))
            (should (string-match-p "Work" (buffer-string)))
            (should (string-match-p "Personal" (buffer-string))))
          (with-current-buffer root-a
            (should (equal qq-runtime--account-id "slot-a"))
            (should (string-match-p "Alice A" (buffer-string)))
            (should-not (string-match-p "Alice B" (buffer-string))))
          (with-current-buffer root-b
            (should (equal qq-runtime--account-id "slot-b"))
            (should (string-match-p "Alice B" (buffer-string)))
            (should-not (string-match-p "Alice A" (buffer-string)))))
      (qq-runtime-stop)
      (dolist (buffer (list manager root-a root-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'qq-root-test)

;;; qq-root-test.el ends here
