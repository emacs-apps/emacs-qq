;;; qq-forward-test.el --- Tests for native merged forwards -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-forward)

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
  "Kill every forward viewer created by a test."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*qq-forward:" (buffer-name buffer))
      (kill-buffer buffer))))

(defmacro qq-forward-test--with-clean-viewers (&rest body)
  "Run BODY with an isolated account-scoped forward viewer set."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--context-account-id "slot-a"))
     (qq-forward-test--kill-viewers)
     (unwind-protect
         (progn ,@body)
       (qq-forward-test--kill-viewers)
       (qq-runtime-stop-account "slot-a" t))))

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
          (qq-forward-native-message-to-internal
           (qq-forward-test--message "1.2" "first")))
         (with-id
          (qq-forward-native-message-to-internal
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
         (qq-forward-native-message-to-internal
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
     (qq-forward-native-message-to-internal
      (append message '((unexpected . t))))
     :type 'error)
    (setf (alist-get 'kind (alist-get 'sender message)) "mystery")
    (should-error
     (qq-forward-native-message-to-internal message)
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
          (qq-forward-native-message-to-internal
           (qq-forward-test--message
            "1" "original" :sequence "4000000001")))
         (reply
          (qq-forward-native-message-to-internal
           (qq-forward-test--message
            "2" "answer" :sequence "4000000002"
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
           (qq-forward--messages-by-entry messages)
           (qq-forward--messages-by-sequence messages))))
    (should (equal (plist-get model :jump-entry-id) "1"))
    (should (string-match-p "Alice: original"
                            (plist-get model :body)))))

(ert-deftest qq-forward-remote-load-passes-resource-and-native-scene ()
  (qq-forward-test--with-clean-viewers
    (let (observed-resource observed-scene success)
      (cl-letf (((symbol-function 'qq-core-get-forward)
                 (lambda (resource scene callback &optional _errback)
                   (setq observed-resource resource
                         observed-scene scene
                         success callback)
                   'request-a)))
        (save-window-excursion
          (let ((buffer
                 (qq-forward-open
                  (qq-forward-test--source "resid-private" "private"))))
            (should (equal observed-resource "resid-private"))
            (should (equal observed-scene "private"))
            (with-current-buffer buffer
              (should
               (qq-forward--request-current-p
                (appkit-current-view) buffer qq-forward--source
                qq-forward--request-owner)))
            (let ((raw
                   (list
                    (qq-forward-test--message
                     "1" "loaded"
                     :origin
                     '((kind . "private") (peer_uin . "10002"))))))
              (should
               (equal
                (alist-get 'kind
                           (alist-get 'sender (car raw)))
                "user"))
              (should (= (length (qq-forward--normalize-messages raw)) 1))
              (funcall success
                       (list :messages raw
                             :unsupported-message-count 0)))
            (with-current-buffer buffer
              (appkit-sync-invalidations (appkit-current-view))
              (should-not qq-forward--error)
              (should qq-forward--loaded-p)
              (should-not qq-forward--loading)
              (should (equal (appkit-chat-timeline-keys) '("1")))
              (should (string-match-p "loaded" (buffer-string))))))))))

(ert-deftest qq-forward-refresh-revokes-the-owned-request ()
  (qq-forward-test--with-clean-viewers
    (let ((request-count 0)
          canceled)
      (cl-letf (((symbol-function 'qq-core-get-forward)
                 (lambda (_resource _scene _callback &optional _errback)
                   (intern (format "request-%d" (cl-incf request-count)))))
                ((symbol-function 'qq-request-cancel)
                 (lambda (request) (push request canceled))))
        (save-window-excursion
          (let ((buffer (qq-forward-open (qq-forward-test--source))))
            (with-current-buffer buffer
              (should (eq qq-forward--request 'request-1))
              (qq-forward-refresh)
              (should (eq qq-forward--request 'request-2))
              (should (equal canceled '(request-1))))))))))

(ert-deftest qq-forward-viewer-makes-omitted-entries-visible ()
  (qq-forward-test--with-clean-viewers
    (cl-letf (((symbol-function 'qq-core-get-forward)
               (lambda (_resource _scene callback &optional _errback)
                 (funcall
                  callback
                  (list
                   :messages
                   (list (qq-forward-test--message "1" "visible"))
                   :unsupported-message-count 2))
                 'settled-request)))
      (save-window-excursion
        (let ((buffer (qq-forward-open (qq-forward-test--source))))
          (with-current-buffer buffer
            (appkit-sync-invalidations (appkit-current-view))
            (should (= qq-forward--unsupported-message-count 2))
            (should
             (equal (appkit-chat-timeline-keys)
                    '("1" :qq-forward-status)))
            (should
             (string-match-p
              "2 unsupported forwarded messages omitted"
              (buffer-string)))))))))

(ert-deftest qq-forward-card-is-one-whole-clickable-action ()
  (let ((segment (qq-forward-test--card))
        opened)
    (with-temp-buffer
      (cl-letf (((symbol-function 'qq-forward-open-segment)
                 (lambda (clicked) (setq opened clicked))))
        (qq-forward-insert-segment segment nil nil)
        (should-not (string-match-p "\\[Open\\]" (buffer-string)))
        (goto-char (point-min))
        (should (button-at (point)))
        (push-button (point))
        (should (equal opened segment))
        (setq opened nil)
        (goto-char (point-min))
        (let ((command
               (lookup-key
                (get-text-property (point) 'keymap)
                (kbd "RET"))))
          (should (commandp command))
          (call-interactively command))
        (should (equal opened segment))
        (goto-char (1- (point-max)))
        (should (get-text-property (point) 'qq-forward-segment))))))

(ert-deftest qq-forward-timeline-uses-entry-id-not-optional-message-id ()
  (qq-forward-test--with-clean-viewers
    (let ((message-id "9007199254742007031"))
      (cl-letf (((symbol-function 'qq-core-get-forward)
                 (lambda (_resource _scene callback &optional _errback)
                   (funcall
                    callback
                    (list
                     :messages
                     (list
                      (qq-forward-test--message
                       "1.2" "first" :message-id message-id)
                      (qq-forward-test--message
                       "9" "second" :message-id message-id))
                     :unsupported-message-count 0)))))
        (save-window-excursion
          (let ((buffer (qq-forward-open (qq-forward-test--source))))
            (with-current-buffer buffer
              (appkit-sync-invalidations (appkit-current-view))
              (should-not qq-forward--error)
              (should (equal (appkit-chat-timeline-keys) '("1.2" "9")))
              (should-not
               (eq (appkit-chat-timeline-node "1.2")
                   (appkit-chat-timeline-node "9"))))))))))

(provide 'qq-forward-test)
;;; qq-forward-test.el ends here
