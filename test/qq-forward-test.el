;;; qq-forward-test.el --- Tests for native merged forwards -*- lexical-binding: t; -*-

(require 'qq-test-state)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-forward)
(require 'qq-operation-test)

(defun qq-forward-test--source (&optional resource-id scene)
  "Return one v2 long-message source for RESOURCE-ID and SCENE."
  `((kind . "resource")
    (resource_id . ,(or resource-id "resid-a"))
    (scene . ,(or scene "group"))))

(defun qq-forward-test--card (&optional resource-id scene)
  "Return one canonical merged-forward card."
  `((type . "card")
    (data . ((kind . "forward")
             (reference . ,(qq-forward-test--source resource-id scene))
             (presentation . ((source . "群聊的聊天记录")
                              (content . "Alice: hello")
                              (summary . "查看1条转发消息")
                              (prompt . "[聊天记录]")))))))

(cl-defun qq-forward-test--message
    (entry-id text &key message-id (sequence "100") sender origin segments)
  "Return one closed `message.get_forward' entry."
  `((entry_id . ,entry-id)
    ,@(when message-id `((message_id . ,message-id)))
    (sequence . ,sequence)
    (state . "live")
    (sent_at . 1710000000)
    (sender . ,(copy-tree
                (or sender
                    '((kind . "user")
                      (user_id . "10001")
                      (name . "Alice")
                      (avatar_url
                       . "https://q1.qlogo.cn/g?b=qq&nk=10001&s=100")))))
    (origin . ,(copy-tree
                (or origin
                    '((kind . "group") (group_uin . "20001")))))
    (segments . ,(copy-tree
                  (or segments
                      `(((kind . "text")
                         (payload . ((text . ,text))))))))))

(defun qq-forward-test--kill-viewers ()
  "Kill only forward buffers owned by the isolated test account Apps."
  (let ((apps (mapcar #'qq-runtime-account-app (qq-runtime-accounts))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* (((derived-mode-p 'qq-forward-mode))
                    (surface (appkit-current-surface))
                    ((memq (appkit-surface-app surface) apps)))
          (appkit-surface-stop surface)
          (kill-buffer buffer))))))

(defmacro qq-forward-test--with-clean-viewers (&rest body)
  "Run BODY with isolated account Apps and only their forward buffers."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--account-id
          (or (qq-state-store-account-id (qq-state-store-create)) (qq-runtime-current-account-id) "slot-a"))
         (qq-runtime--snapshots (qq-test--snapshots (qq-state-store-create))) (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal)) (qq-runtime-change-hook nil)
         (qq-media-cache-update-hook nil))
     (qq-runtime-with-account "slot-a"
                              (unwind-protect
                                  (progn ,@body)
                                (qq-forward-test--kill-viewers)
                                (qq-runtime-stop)))))

(ert-deftest qq-forward-native-link-preview-renders-and-opens-authored-url ()
  (let* ((url "https://example.org/original")
         (message
          (qq-forward-native-message-to-internal (qq-runtime-state)
                                                 (qq-forward-test--message
                                                  "0" url
                                                  :sender '((kind . "anonymous") (name . "Author"))
                                                  :segments
                                                  `(((kind . "text")
                                                     (payload . ((text . ,url)
                                                                 (link_preview
                                                                  . ((title . "Forwarded site")
                                                                     (description . "Native site description")
                                                                     (is_default_picture . t))))))))))
         opened
         (inhibit-read-only t))
    (with-temp-buffer
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (target &rest _) (setq opened target))))
        (qq-forward--insert-message message nil)
        (goto-char (point-min))
        (search-forward "Forwarded site")
        (should (button-at (1- (point))))
        (push-button (1- (point)))
        (should (equal opened url))
        (should (search-forward "Native site description" nil t))
        (should (search-forward url nil t))
        (should-not (search-forward url nil t))))
    (should (equal (alist-get 'preview message) url))))

(ert-deftest qq-forward-mode-has-special-navigation-bindings ()
  (with-temp-buffer
    (qq-forward-mode)
    (should (derived-mode-p 'special-mode))
    (should (eq (lookup-key qq-forward-mode-map (kbd "q")) #'quit-window))
    (should (eq (lookup-key qq-forward-mode-map (kbd "g"))
                #'qq-forward-refresh))
    (should (eq (lookup-key qq-forward-mode-map (kbd "n"))
                #'qq-forward-next-message))
    (should (eq (lookup-key qq-forward-mode-map (kbd "p"))
                #'qq-forward-previous-message))
    (should-not (lookup-key qq-forward-mode-map (kbd "RET")))
    (should-not (lookup-key qq-forward-mode-map (kbd "<return>")))))

(ert-deftest qq-forward-card-requires-a-scene-qualified-resource ()
  (let ((card (qq-forward-test--card)))
    (should (qq-forward-segment-p card))
    (should (equal (qq-forward-reference-id card) "resid-a"))
    (dolist (invalid
             (list
              '((type . "card")
                (data . ((kind . "forward")
                         (reference . ((kind . "resource")
                                       (resource_id . "resid-a")))
                         (presentation . nil))))
              '((type . "card")
                (data . ((kind . "forward")
                         (reference . ((kind . "resource")
                                       (resource_id . "resid-a")
                                       (scene . "unknown")))
                         (presentation . nil))))))
      (should-not (qq-forward-segment-p invalid))
      (should-error (qq-forward-open-segment invalid) :type 'user-error))))

(ert-deftest qq-forward-resource-canonicalization-includes-scene ()
  (let* ((source '((scene . "private")
                   (resource_id . "resid-a")
                   (kind . "resource")))
         (canonical (qq-forward--canonical-source source)))
    (should
     (equal canonical
            '((kind . "resource")
              (resource_id . "resid-a")
              (scene . "private"))))
    (should-not
     (equal (qq-forward--source-buffer-key canonical)
            (qq-forward--source-buffer-key
             (qq-forward-test--source "resid-a" "group"))))))

(ert-deftest qq-forward-message-mapper-keeps-entry-and-snowflake-distinct ()
  (let* ((without-id
          (qq-forward-native-message-to-internal (qq-runtime-state)
                                                 (qq-forward-test--message "1.2" "first")))
         (with-id
          (qq-forward-native-message-to-internal (qq-runtime-state)
                                                 (qq-forward-test--message
                                                  "1.3" "second"
                                                  :message-id "9007199254742007089"))))
    (should (equal (alist-get 'id without-id) "1.2"))
    (should-not (alist-get 'server-id without-id))
    (should (equal (alist-get 'id with-id) "1.3"))
    (should (equal (alist-get 'server-id with-id)
                   "9007199254742007089"))
    (should (equal (alist-get 'message-seq with-id) "100"))
    (should (equal (alist-get 'sender-name with-id) "Alice"))
    (should (equal (alist-get 'message-type with-id) "group"))))

(ert-deftest qq-forward-message-mapper-supports-private-and-anonymous-origin ()
  (let ((message
         (qq-forward-native-message-to-internal (qq-runtime-state)
                                                (qq-forward-test--message
                                                 "2" "hello"
                                                 :sender '((kind . "anonymous") (name . "Visitor"))
                                                 :origin '((kind . "private") (peer_uin . "10002"))))))
    (should-not (alist-get 'sender-id message))
    (should (equal (alist-get 'sender-name message) "Visitor"))
    (should (equal (alist-get 'origin message)
                   '((kind . "private") (peer_uin . "10002"))))))

(ert-deftest qq-forward-message-validator-rejects-open-wire-shapes ()
  (let ((message (qq-forward-test--message "1" "hello")))
    (should-error
     (qq-forward-native-message-to-internal (qq-runtime-state)
                                            (append message '((unexpected . t))))
     :type 'error)
    (setf (alist-get 'kind (alist-get 'sender message)) "mystery")
    (should-error
     (qq-forward-native-message-to-internal (qq-runtime-state) message)
     :type 'error)))

(ert-deftest qq-forward-segments-share-the-root-v2-normalizer ()
  (let* ((nested
          `((kind . "forward_card")
            (payload
             . ((reference . ,(qq-forward-test--source "resid-b" "private"))
                (presentation . ((prompt . "[聊天记录]")))))))
         (market
          '((kind . "market_face")
            (payload . ((emoji_id . "abcdef")
                        (package_id . 12)
                        (summary . "[商城表情]")
                        (url . "https://example.test/sticker")
                        (width . 120)
                        (height . 120)))))
         (image
          '((kind . "image")
            (payload . ((width . 640)
                        (height . 480)
                        (sub_type . 0)
                        (summary . "[图片]")
                        (media_id . "media-a")))))
         (app
          '((kind . "light_app")
            (payload . ((app . "com.example.card")
                        (source . "Example")
                        (summary . "Safe summary")
                        (preview . ["line one" "line two"])
                        (prompt . "[应用卡片]"))))))
    (should (qq-forward-segment-p
             (qq-forward-native-segment-to-internal nested)))
    (let ((segment (qq-forward-native-segment-to-internal market)))
      (should (equal (alist-get 'type segment) "mface"))
      (should (= (alist-get 'emoji_package_id
                            (alist-get 'data segment))
                 12)))
    (let ((segment (qq-forward-native-segment-to-internal image)))
      (should (equal (alist-get 'type segment) "image"))
      (should (equal (alist-get 'media_id (alist-get 'data segment))
                     "media-a")))
    (let* ((segment (qq-forward-native-segment-to-internal app))
           (data (alist-get 'data segment)))
      (should (equal (alist-get 'type segment) "card"))
      (should (equal (alist-get 'kind data) "app"))
      (should (equal (alist-get 'app data) "com.example.card"))
      (should (equal (alist-get 'title data) "[应用卡片]"))
      (should (equal (alist-get 'source data) "Example"))
      (should (equal (alist-get 'content data) "line one\nline two"))
      (should-not (assq 'url data)))))

(ert-deftest qq-forward-unsupported-segment-never-projects-unclosed-raw-data ()
  (let* ((segment
          (qq-forward-native-segment-to-internal
           '((kind . "unsupported")
             (payload . ((native_keys . ["elem.99"])
                         (summary . "elem.99")
                         (raw . ((fallback_text . "visible"))))))))
         (data (alist-get 'data segment)))
    (should (equal (alist-get 'type segment) "__unsupported"))
    (should (equal (alist-get 'fallback_text data) "visible"))
    (should-not (assq 'raw data))))

(ert-deftest qq-forward-native-reply-resolves-within-the-fetched-tree ()
  (let* ((original
          (qq-forward-native-message-to-internal (qq-runtime-state)
                                                 (qq-forward-test--message
                                                  "1" "original"
                                                  :sequence "4000000001")))
         (reply
          (qq-forward-native-message-to-internal (qq-runtime-state)
                                                 (qq-forward-test--message
                                                  "2" "answer"
                                                  :sequence "4000000002"
                                                  :segments
                                                  '(((kind . "reply")
                                                     (payload . ((target . ((kind . "native")
                                                                            (sequence . "4000000001")
                                                                            (sender_name . "Alice"))))))
                                                    ((kind . "text") (payload . ((text . "answer"))))))))
         (messages (list original reply))
         (model
          (qq-forward--reply-view-model
           reply
           (qq-forward--messages-by-sequence messages))))
    (should (equal (plist-get model :jump-entry-id) "1"))
    (with-temp-buffer
      (qq-forward--insert-reply-preview-line
       model (appkit-ui-make-prefix-state "" "") nil)
      (goto-char (point-min))
      (search-forward "Alice")
      (let ((faces (get-text-property (match-beginning 0) 'face)))
        (should (eq (car faces)
                    (appkit-name-color-face
                     (qq-chat--message-sender-color-key original))))
        (should (memq 'appkit-chat-reply-author faces))))))

(ert-deftest qq-forward-renders-native-portrait-and-absent-source-author ()
  (let* ((url "https://qh.qlogo.cn/g?b=oidb&ek=portrait&s=0")
         (target '((kind . "native")
                   (sequence . "77")
                   (sender . ((uin . "10002")))
                   (sender_name . "Original author")
                   (preview . "Historical quote")))
         (message
          (qq-forward-native-message-to-internal (qq-runtime-state)
                                                 (qq-forward-test--message
                                                  "0" "answer"
                                                  :sender `((kind . "user") (user_id . "10001")
                                                            (name . "Reply author") (avatar_url . ,url))
                                                  :segments `(((kind . "reply") (payload . ((target . ,target))))
                                                              ((kind . "text") (payload . ((text . "answer"))))))))
         (model (qq-forward--reply-view-model message (make-hash-table :test #'equal)))
         fetched-key fetched-url)
    (cl-letf (((symbol-function 'qq-media--ensure-resource-image)
               (lambda (key fetch _height &optional _factory)
                 (setq fetched-key key)
                 (funcall fetch
                          (lambda (resource) (setq fetched-url (alist-get 'url resource)))
                          #'ignore)
                 'native-avatar))
              ((symbol-function 'qq-media-avatar-image)
               (lambda (&rest _) (ert-fail "Native portrait was discarded"))))
      (should (eq (qq-media-message-avatar-image message) 'native-avatar))
      (should (equal fetched-url url))
      (should (equal fetched-key (concat "message-avatar-url:" url))))
    (should-not (plist-get model :jump-entry-id))
    (with-temp-buffer
      (qq-forward--insert-reply-preview-line
       model (appkit-ui-make-prefix-state "" "") nil)
      (should (string-match-p "Original author" (buffer-string)))
      (should (string-match-p "Historical quote" (buffer-string)))
      (should-not (string-match-p "10002\\|Reply author" (buffer-string))))))

(ert-deftest qq-forward-card-is-one-whole-clickable-action ()
  (let ((segment (qq-forward-test--card))
        opened)
    (with-temp-buffer
      (cl-letf (((symbol-function 'qq-forward-open-segment)
                 (lambda (clicked) (setq opened clicked))))
        (qq-forward-insert-segment segment nil nil)
        (goto-char (point-min))
        (should (button-at (point)))
        (push-button (point))
        (should (equal opened segment))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (let ((map (make-sparse-keymap)))
            (define-key map [return] #'ignore)
            (use-local-map map)
            (dolist (key '("RET" "<return>"))
              (setq opened nil)
              (goto-char (point-min))
              (execute-kbd-macro (kbd key))
              (should (equal opened segment)))))
        (goto-char (1- (point-max)))
        (should (get-text-property (point) 'qq-forward-segment))))))

(ert-deftest qq-forward-shared-remote-tree-preserves-scene-entry-ids-and-omissions ()
  (qq-operation-test-with-app
   (save-window-excursion
     (let* ((source (qq-forward-test--source "resid-private" "private"))
            (buffer (qq-forward-open source)))
       (unwind-protect
           (progn
             (qq-operation-test--drain)
             (should (equal (plist-get (car calls) :params)
                            '((account_id . "slot-a") (resource_id . "resid-private") (scene . "private"))))
             (qq-operation-test--resolve (car calls)
                                         `((account_id . "slot-a") (unsupported_message_count . 2)
                                           (messages . ,(list (qq-forward-test--message "1.2" "first" :message-id "9007199254742007031")
                                                              (qq-forward-test--message "9" "second" :message-id "9007199254742007031")))))
             (qq-operation-test--drain)
             (with-current-buffer buffer
               (should-not (qq-forward--data :error))
               (should (equal (appkit-chat-timeline-keys) '("1.2" "9" :qq-forward-status)))
               (should (string-match-p "2 unsupported forwarded messages omitted" (buffer-string))))
             (should-not (qq-state-session-messages (qq-runtime-state) "private:10001")))
         (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest qq-forward-refresh-revokes-old-tree-and-close-keeps-app-result ()
  (qq-operation-test-with-app
   (save-window-excursion
     (let* ((source (qq-forward-test--source)) (buffer (qq-forward-open source)))
       (qq-operation-test--drain)
       (let ((old (car calls)))
         (with-current-buffer buffer (qq-forward-refresh))
         (qq-operation-test--drain)
         (should (memq (plist-get old :token) cancelled))
         (kill-buffer buffer)
         (qq-operation-test--resolve (car calls)
                                     `((account_id . "slot-a") (unsupported_message_count . 0)
                                       (messages . ,(list (qq-forward-test--message "1" "fresh")))))
         (qq-operation-test--resolve old '((account_id . "slot-a") (unsupported_message_count . 0) (messages)))
         (qq-operation-test--drain)
         (should (= (length (plist-get (plist-get (qq-runtime-operation (list 'forward source)) :value) :messages)) 1)))))))

(provide 'qq-forward-test)
;;; qq-forward-test.el ends here
