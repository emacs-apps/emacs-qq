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
         (qq-account--current-account-id "slot-a")
         (qq-login--current nil)
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
                    :render-function #'qq-root--render
                    :setup (apply-partially #'qq-root--setup-scope "slot-a")))
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
        (should (string-match-p "10001" (qq-root--header-line)))
        (should (string-match-p "online" (qq-root--header-line)))))))

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
      (let ((qq-root--scope "slot-work")
            (qq-multi-account nil))
        (let ((header (qq-root--header-line)))
          (should (string-match-p "10001" header))
          (should-not (string-match-p "20002\\|Old account\\|1/2" header)))
        (let ((qq-multi-account t))
          (should (string-match-p "1/2 online" (qq-root--header-line))))))))

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
          (and (eq (car-safe (appkit-directory-entry-key entry)) 'session)
               (alist-get 'key (appkit-directory-entry-payload entry))))
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

(ert-deftest qq-root-login-presentation-follows-owner-and-disappears-on-finish ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((model '(:account-id "slot-a" :status "Authorize A"
                    :error "Login failed A" :display "[QR-A]\n")))
       (cl-letf (((symbol-function 'qq-login-view-model) (lambda () model)))
         (qq-root--handle-login-change)
         (should (string-match-p "Authorize A" (buffer-string)))
         (should (string-match-p "Login failed A" (buffer-string)))
         (should (string-match-p "\\[QR-A\\]" (buffer-string)))
         (setq model '(:account-id "slot-b" :status "Authorize B"
                       :display "[QR-B]\n"))
         (qq-root--handle-login-change)
         (should-not (gethash 'login (appkit-directory-surface-node-table (appkit-directory-surface))))
         (should-not (string-match-p "Authorize\\|QR-" (buffer-string)))
         (setq model '(:account-id "slot-a" :status "Authorize again A"))
         (qq-root--handle-login-change)
         (should (string-match-p "Authorize again A" (buffer-string)))
         (setq model nil)
         (qq-root--handle-login-change)
         (should-not (gethash 'login (appkit-directory-surface-node-table (appkit-directory-surface))))
         (should-not (string-match-p "Authorize" (buffer-string))))))))

(ert-deftest qq-root-single-startup-does-not-expose-managed-accounts ()
  (let ((qq-multi-account nil)
        (qq-root--scope 'gateway)
        (model nil))
    (cl-letf (((symbol-function 'qq-login-view-model) (lambda () model))
              ((symbol-function 'qq-account-list)
               (lambda ()
                 (ert-fail "single startup accessed account manager registry"))))
      (let ((entries (qq-root--project-entries)))
        (should-not
         (seq-find (lambda (entry)
                     (eq (car-safe (appkit-directory-entry-key entry)) 'account))
                   entries))
        (with-temp-buffer
          (qq-root-mode)
          (appkit-directory-reconcile (appkit-directory-surface) entries)
          (should (string-match-p "qq-login" (buffer-string)))))
      (setq model '(:status "Choose login identity"))
      (let ((entries (qq-root--project-entries)))
        (should (seq-find (lambda (entry)
                           (eq (appkit-directory-entry-key entry) 'login))
                         entries)))
      (should-error (qq-root-open-gateway) :type 'user-error))))

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
    (should (equal ">"
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

(ert-deftest qq-root-preview-keeps-mention-faces ()
  (let* ((session '((key . "group:42")
                    (type . group)
                    (last-message-id . "m1")
                    (last-message-sender-name . "Alice")
                    (last-message-preview . "hi @Bob @me and /smile")))
         (message
          '((server-id . "m1")
            (sender-id . "42")
            (sender-name . "Alice")
            (segments
             . (((type . "text") (data . ((text . "hi "))))
                ((type . "at") (data . ((qq . "1001") (name . "Bob"))))
                ((type . "text") (data . ((text . " "))))
                ((type . "at") (data . ((qq . "7") (name . "me"))))
                ((type . "text") (data . ((text . " and"))))
                ((type . "face") (data . ((id . "14") (description . "/smile"))))))))
         (qq-media--resource-cache (make-hash-table :test #'equal))
         (qq-media--image-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'qq-state-session-messages)
               (lambda (_session-key) (list message)))
              ((symbol-function 'qq-state-self-user-id) (lambda () "7"))
              ((symbol-function 'qq-media-face-image) #'ignore))
      (let* ((preview (qq-root--session-preview-model session))
             (text (appkit-ui-one-line-preview-text preview))
             (rendered (appkit-ui-render-one-line-preview
                        preview 80 :face 'shadow)))
        (should (equal "hi @Bob @me and /smile" text))
        (should (equal (list (appkit-name-color-face "1001") 'qq-msg-mention)
                       (get-text-property (string-search "@Bob" text)
                                          'face text)))
        (should (eq 'qq-msg-mention-self
                    (get-text-property (string-search "@me" text)
                                       'face text)))
        (should-not (get-text-property 0 'face text))
        ;; Appkit's shared row styling appends, so the mention face leads.
        (should (eq 'qq-msg-mention-self
                    (car (get-text-property (string-search "@me" rendered)
                                            'face rendered))))))))

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
    (should (equal "Me> hello" (qq-root-test-session-preview-text outgoing)))
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
     (save-window-excursion
       (switch-to-buffer buffer)
       (appkit-surface-refresh-responsive-geometry view)
       (let ((width qq-root--fill-column))
         (switch-to-buffer (get-buffer-create " *qq-root-geometry-background*"))
         (with-current-buffer buffer
           (appkit-surface-send
            view (list 'qq-render (appkit-projection-change-create :full-p t)))
           (should (= (qq-root--stable-fill-column) width))
           (should (= qq-root--fill-column width))))))))

(ert-deftest qq-root-mode-disables-undo-history ()
  (with-temp-buffer
    (qq-root-mode)
    (should (eq buffer-undo-list t))))

(ert-deftest qq-root-item-at-point-uses-the-shared-directory-owner ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1" '((type . private) (title . "First")) nil)
   (qq-state-upsert-session
    "group:2" '((type . group) (title . "Second")) nil)
   (qq-root-test-set-recent "private:1" "group:2")
   (qq-root-test-with-live-view
     (appkit-surface-send
      view (list 'qq-render (appkit-projection-change-create :full-p t)))
     (let* ((nodes (appkit-directory-surface-node-table
                    (appkit-directory-surface)))
            (first (ewoc-location (gethash '(session . "private:1") nodes)))
            (second (ewoc-location (gethash '(session . "group:2") nodes))))
       (let ((inhibit-read-only t))
         (put-text-property first (1+ first) 'qq-root-session-key "stale"))
       (goto-char (+ second 2))
       (should (equal "private:1" (qq-root--session-key-at-point first)))
       (should (equal "group:2" (qq-root--session-key-at-point)))
       (should (derived-mode-p 'appkit-directory-mode))))))

(ert-deftest qq-root-projects-navigation-without-a-key-cheat-sheet ()
  (qq-root-test-with-reset
   (with-temp-buffer
     (cl-letf (((symbol-function 'qq-root--buffer-width) (lambda () 80)))
       (let* ((entries (qq-root--project-entries))
              (texts (delq nil (mapcar #'appkit-directory-entry-label entries))))
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
       (let ((one-node (gethash '(session . "private:1") (appkit-directory-surface-node-table (appkit-directory-surface))))
             (two-node (gethash '(session . "group:2") (appkit-directory-surface-node-table (appkit-directory-surface)))))
         (should one-node)
         (should two-node)
         (qq-state-upsert-session
          "private:1" '((last-message-preview . "updated")) nil)
         (cl-letf (((symbol-function 'erase-buffer)
                    (lambda () (ert-fail "incremental sync erased the buffer"))))
           (appkit-surface-send view (list 'qq-render (appkit-projection-change-create :full-p t))))
         (should (eq one-node
                     (gethash '(session . "private:1") (appkit-directory-surface-node-table (appkit-directory-surface)))))
         (should (eq two-node
                     (gethash '(session . "group:2") (appkit-directory-surface-node-table (appkit-directory-surface)))))
         (should (string-match-p "updated" (buffer-string)))
         (should-not (string-match-p "old" (buffer-string)))
         (should (eq buffer-undo-list t)))))))

(ert-deftest qq-root-selected-window-refreshes-cached-width ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let* ((frame (selected-frame))
            (original-width (frame-width frame)))
       (unwind-protect
           (save-window-excursion
             (set-frame-width frame 180)
             (delete-other-windows)
             (switch-to-buffer buffer)
             (let* ((narrow (selected-window))
                    (wide (split-window-right 60)))
               (set-window-buffer wide buffer)
               (run-hook-with-args 'window-state-change-functions frame)
               (let ((narrow-width qq-root--fill-column))
                 (should (= narrow-width
                            (max 60 (appkit-geometry-window-width
                                     narrow qq-root-auto-fill-margin-columns))))
                 (select-window wide)
                 (run-hook-with-args 'window-state-change-functions frame)
                 (should (= qq-root--fill-column
                            (max 60 (appkit-geometry-window-width
                                     wide qq-root-auto-fill-margin-columns))))
                 (should (> qq-root--fill-column narrow-width)))))
         (set-frame-width frame original-width))))))

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

(ert-deftest qq-root-global-media-update-repaints-coalesced-avatar-success ()
  (skip-unless (image-type-available-p 'svg))
  (qq-root-test-with-reset
    (let ((qq-media--resource-cache (make-hash-table :test #'equal))
          (qq-media--image-cache (make-hash-table :test #'equal))
          (qq-media--consumers (make-hash-table :test #'eq))
          (qq-media--pending-cache-updates (make-hash-table :test #'equal))
          (qq-media--cache-update-timer nil)
          (qq-media-cache-update-hook '(qq-root--handle-media-cache-update))
          (file (make-temp-file "qq-root-avatar-" nil ".svg")))
      (unwind-protect
          (cl-letf (((symbol-function 'image-size)
                     ;; Batch Emacs has no graphical image-sizing surface.
                     (lambda (_image &rest _arguments) '(16 . 16))))
            (with-temp-file file
              (insert "<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16'><circle cx='8' cy='8' r='8' fill='orange'/></svg>"))
            (qq-state-upsert-session
             "private:1" '((type . private) (target-id . "1") (title . "One")) nil)
            (qq-state-upsert-session
             "group:2" '((type . group) (target-id . "2") (title . "Two")) nil)
            (qq-root-test-set-recent "private:1" "group:2")
            (qq-root-test-with-live-view
              (cl-labels ((row-image (title)
                            (save-excursion
                              (goto-char (point-min))
                              (search-forward title)
                              (appkit-media--display-image-spec
                               (get-text-property (line-beginning-position) 'display)))))
                (appkit-surface-send
                 view (list 'qq-render (appkit-projection-change-create :full-p t)))
                (should-not (row-image "One"))
                (should-not (row-image "Two"))
                (dolist (key '("avatar:1" "group-avatar:2"))
                  (qq-media--cache-resource key `((file . ,file)))
                  (qq-media--cache-image key (create-image file 'svg nil :height 16)))
                (qq-media--note-cache-updated "avatar:1")
                (qq-media--note-cache-updated nil)
                (when (timerp qq-media--cache-update-timer)
                  (cancel-timer qq-media--cache-update-timer))
                (qq-media--flush-cache-updates)
                (should (eq (car (row-image "One")) 'image))
                (should (eq (car (row-image "Two")) 'image)))))
        (when (timerp qq-media--cache-update-timer)
          (cancel-timer qq-media--cache-update-timer))
        (delete-file file)))))

(ert-deftest qq-root-face-previews-refresh-every-occurrence-as-resources-arrive ()
  (qq-root-test-with-reset
   (let ((qq-media--resource-cache (make-hash-table :test #'equal))
         (qq-media--image-cache (make-hash-table :test #'equal))
         (file (make-temp-file "qq-root-face-" nil ".svg")))
     (unwind-protect
         (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () nil))
                   ((symbol-function 'qq-media-session-avatar-display-string)
                    (lambda (_session) "#")))
           (with-temp-file file
             (insert "<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16'><rect width='16' height='16' fill='orange'/></svg>"))
           (qq-state-insert-pending-message
            "group:1"
            '(((type . "text") (data . ((text . "before\n"))))
              ((type . "face") (data . ((id . "177") (description . "/喷血"))))
              ((type . "face") (data . ((id . "177") (description . "/喷血"))))
              ((type . "face") (data . ((id . "298") (description . "/元宝"))))
              ((type . "text") (data . ((text . "after"))))))
           (qq-root-test-set-recent "group:1")
           (qq-root-test-with-live-view
             (cl-labels
                 ((face-displays ()
                    (save-excursion
                      (goto-char (point-min))
                      (let (displays)
                        (while (re-search-forward "/喷血\\|/元宝" nil t)
                          (push (appkit-media--display-image-spec
                                 (get-text-property (match-beginning 0) 'display))
                                displays))
                        (nreverse displays))))
                  (ready (id)
                    (let ((key (format "face:%s" id)))
                      (qq-media--cache-resource key `((file . ,file)))
                      (qq-media--cache-image
                       key `(image :type svg :file ,file :height 18))
                      (qq-root--handle-media-cache-update key))))
               (appkit-surface-send
                view (list 'qq-render
                           (appkit-projection-change-create :full-p t)))
               (should (equal (face-displays) '(nil nil nil)))
               (ready "298")
               (should (equal (mapcar #'car-safe (face-displays))
                              '(nil nil image)))
               (ready "177")
               (should (equal (mapcar #'car-safe (face-displays))
                              '(image image image)))
               (dolist (display (face-displays))
                 (should (equal (plist-get (cdr display) :height) '(1 . ch))))
               (should (string-match-p
                        "before /喷血 /喷血 /元宝 after" (buffer-string))))))
       (delete-file file)))))

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
         (gethash '(session . "group:2") (appkit-directory-surface-node-table (appkit-directory-surface)))))
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
               (appkit-directory-entry-key
                (ewoc-data (ewoc-nth (appkit-directory-surface-ewoc (appkit-directory-surface)) 3)))))))))

(ert-deftest qq-root-dead-view-makes-all-external-callbacks-inert ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((geometry-hook appkit-surface--responsive-hook))
       (should (memq geometry-hook window-state-change-functions))
       (appkit-surface-stop view)
       (should-not (memq geometry-hook window-state-change-functions))
       (cl-letf (((symbol-function 'appkit-surface-send)
                  (lambda (&rest _args)
                    (ert-fail "dead root view requested a sync"))))
         (qq-root--handle-state-change
          '(:type connection :account-id "slot-a"))
         (qq-root--handle-state-change
          '(:type message :account-id "slot-a" :session-key "group:1"))
         (qq-root--handle-media-cache-update "avatar:1")
         (should-not (appkit-surface-refresh-responsive-geometry view)))))))

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
         (setq reopened (qq-root-open "slot-a")))
       (should (eq reopened buffer))
       (setq view (with-current-buffer buffer (appkit-current-surface)))
       (should (appkit-surface-live-p view))
       (should-not (eq dead view))
       (should (equal 'root (appkit-surface-identity view)))
       (save-window-excursion
         (qq-root-open "slot-a"))
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
                  (lambda (_session) icon)))
         (qq-root--handle-state-change
          '(:type message :account-id "slot-a" :session-key "private:1"))
         (should (string-match-p "old-icon" (buffer-string)))
         (setq icon "new-icon")
         (qq-root--handle-media-cache-update "avatar:1")
         (should (string-match-p "new-icon" (buffer-string)))
         (save-window-excursion
           (switch-to-buffer buffer)
           (appkit-surface-refresh-responsive-geometry view)
           (should (= qq-root--fill-column
                      (max 60 (appkit-geometry-window-width
                               (selected-window)
                               qq-root-auto-fill-margin-columns))))
           (setq reopened (qq-root-open "slot-a"))))
       (should (eq buffer reopened))
       (should (equal renamed (buffer-name reopened)))
       (should (eq view (with-current-buffer reopened (appkit-current-surface))))))))

(ert-deftest qq-root-keeps-manager-and-two-account-roots-live-together ()
  (let ((qq-runtime--app nil)
        (qq-multi-account t)
        (qq-login--current nil)
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
              (setq manager (qq-root-open 'gateway))
              (qq-runtime-with-account "slot-b"
                (setq root-a (qq-root-open "slot-a"))
                (should (equal qq-state--active-account-id "slot-b"))
                (should (equal (alist-get 'title (qq-state-session "private:1")) "Alice B")))
              (setq root-b (qq-root-open "slot-b"))))
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
            (should-not (string-match-p "Alice A" (buffer-string))))
          (save-window-excursion
            (with-current-buffer root-a
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt choices &rest _)
                           (car (rassoc "slot-b" choices)))))
                (should (eq (qq-root-switch-account) root-b)))))
          (save-window-excursion
            (with-current-buffer root-a
              (should (eq (qq-root-open) root-a))
              (let ((qq-multi-account nil))
                (should (eq (qq-root-open) root-b))
                (should (eq (qq-root-open "slot-a") root-a))))
            (let ((qq-multi-account nil))
              (qq-root-open 'gateway)))
          (with-current-buffer manager
            (should-not (string-match-p "Work\\|Personal\\|slot-a\\|slot-b"
                                        (buffer-string))))
          (should (qq-runtime-account "slot-a"))
          (should (qq-runtime-account "slot-b")))
      (qq-runtime-stop)
      (dolist (buffer (list manager root-a root-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest qq-root-mark-read-uses-current-native-conversation ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1"
    '((type . private) (target-id . "1") (title . "One")
      (unread-badge-count . 3)) nil)
   (qq-state-upsert-session
    "group:2"
    '((type . group) (target-id . "2") (title . "Two")
      (unread-badge-count . 5)) nil)
   (qq-root-test-set-recent "private:1" "group:2")
   (qq-root-test-with-live-view
     (let ((avatar-clicks 0)
           calls)
       (cl-letf
           (((symbol-function 'qq-media-session-avatar-display-string)
             (lambda (_session)
               (with-temp-buffer
                 (insert-text-button "#" 'action
                                     (lambda (_button) (cl-incf avatar-clicks)))
                 (buffer-string))))
            ((symbol-function 'qq-runtime-request)
             (lambda (request &rest _)
               (push (list (cadr request) (qq-runtime-current-account-id)) calls))))
         (appkit-surface-send
          view (list 'qq-render (appkit-projection-change-create :full-p t)))
         (let* ((directory (appkit-directory-surface))
                (nodes (appkit-directory-surface-node-table directory))
                (one (ewoc-location (gethash '(session . "private:1") nodes)))
                (two (ewoc-location (gethash '(session . "group:2") nodes))))
           (goto-char one)
           (should (button-at (point)))
           (push-button (point))
           (should (= avatar-clicks 1))
           (should (eq (key-binding (kbd "r")) #'appkit-directory-mark-read))
           (let ((transient-mark-mode t))
             (set-mark two)
             (setq mark-active t)
             (call-interactively (key-binding (kbd "r"))))
           (should (equal calls '(("private:1" "slot-a"))))
           (should (= 3 (alist-get 'unread-badge-count
                                   (qq-state-session "private:1"))))
           (should (appkit-directory-entry-unread-p
                    (appkit-directory-entry-for-key
                     directory '(session . "private:1"))))
           (should (= 3 (alist-get 'unread-badge-count
                                   (qq-state-session "private:1"))))
           ;; Only the authoritative native projection clears the badge.
           (qq-state-upsert-session
            "private:1" '((unread-badge-count . 0)) nil)
           (qq-root--handle-state-change
            '(:type session :account-id "slot-a" :session-key "private:1"))
           (should-not
            (appkit-directory-entry-unread-p
             (appkit-directory-entry-for-key
              directory '(session . "private:1"))))
           (should (= 5 (alist-get 'unread-badge-count
                                   (qq-state-session "group:2"))))))))))

(ert-deftest qq-root-account-items-share-activation-and-navigation ()
  (let ((qq-multi-account t)
        opened)
    (cl-letf (((symbol-function 'qq-account-list)
               (lambda ()
                 '(((account_id . "slot-a") (label . "Work") (phase . "online"))
                   ((account_id . "slot-b") (label . "Home") (phase . "offline")))))
              ((symbol-function 'qq-login-view-model) #'ignore)
              ((symbol-function 'qq-root-open-account)
               (lambda (account-id) (push account-id opened)))
              ((symbol-function 'qq-runtime-request)
               (lambda (&rest _arguments)
                 (ert-fail "account item dispatched a conversation read"))))
      (with-temp-buffer
        (qq-root-mode)
        (setq-local qq-root--scope 'gateway)
        (appkit-directory-reconcile
         (appkit-directory-surface) (qq-root--project-entries))
        (goto-char (point-min))
        (should-error (appkit-directory-mark-read) :type 'user-error)
        (qq-root-button-forward)
        (should (equal (qq-root--account-id-at-point) "slot-a"))
        (qq-root-open-at-point)
        (qq-root-button-forward)
        (should (equal (qq-root--account-id-at-point) "slot-b"))
        (qq-root-open-at-point)
        (qq-root-button-backward)
        (should (equal (qq-root--account-id-at-point) "slot-a"))
        (should-error (appkit-directory-mark-read) :type 'user-error)
        (should (equal opened '("slot-b" "slot-a")))))))

(ert-deftest qq-root-login-directory-rows-preserve-qr-buttons-and-line-breaks ()
  (let* ((qq-multi-account nil)
         (clicks 0)
         (display
          (with-temp-buffer
            (insert "QR: " (propertize "[QR]" 'display '(space :height 6)) "\n")
            (insert-text-button "Verify" 'action
                                (lambda (_button) (cl-incf clicks)))
            (insert "\n\n")
            (buffer-string)))
         (model (list :status "Authorize" :display display))
         (expected
          (with-temp-buffer
            (qq-login-insert-view model)
            (buffer-string))))
    (cl-letf (((symbol-function 'qq-login-view-model) (lambda () model)))
      (with-temp-buffer
        (qq-root-mode)
        (setq-local qq-root--scope 'gateway)
        (appkit-directory-reconcile
         (appkit-directory-surface) (qq-root--project-entries))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (concat "\n" (substring-no-properties expected))))
        (goto-char (point-min))
        (search-forward "[QR]")
        (should (equal (get-text-property (match-beginning 0) 'display)
                       '(space :height 6)))
        (search-forward "Verify")
        (goto-char (match-beginning 0))
        (should (button-at (point)))
        (push-button (point))
        (should (= clicks 1))
        (should (eq (key-binding (kbd "r")) #'appkit-directory-mark-read))
        (should-error (call-interactively (key-binding (kbd "r"))) :type 'user-error)
        ;; Identical QR text with a new display property must repaint.
        (let ((next-display (copy-sequence display)))
          (put-text-property 4 8 'display '(space :height 8) next-display)
          (setq model (list :status "Authorize" :display next-display)))
        (appkit-directory-reconcile
         (appkit-directory-surface) (qq-root--project-entries))
        (goto-char (point-min))
        (search-forward "[QR]")
        (should (equal (get-text-property (match-beginning 0) 'display)
                       '(space :height 8)))))))


(ert-deftest qq-root-pin-keys-toggle-row-directly ()
  (qq-root-test-with-reset
    (let (calls)
      (cl-letf (((symbol-function 'qq-root--session-key-at-point)
                 (lambda (&optional _) "group:20001"))
                ((symbol-function 'qq-runtime-request)
                 (lambda (request &rest _) (push request calls))))
        (with-temp-buffer
          (qq-root-mode)
          (qq-state-upsert-session "group:20001" '((pinned . :false)) nil)
          (call-interactively (lookup-key qq-root-mode-map (kbd "P")))
          (qq-state-upsert-session "group:20001" '((pinned . t)) nil)
          (call-interactively (lookup-key qq-root-mode-map (kbd "^")))))
      (should (equal (nreverse calls)
                     '((conversation-pinned "group:20001" t)
                       (conversation-pinned "group:20001" nil)))))))

(provide 'qq-root-test)

;;; qq-root-test.el ends here
