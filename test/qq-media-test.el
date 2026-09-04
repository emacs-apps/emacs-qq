;;; qq-media-test.el --- Tests for qq-media -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-media)
(require 'qq-runtime)

(defmacro qq-media-test-with-reset (&rest body)
  "Run BODY with clean qq-media caches and rerender hooks disabled."
  `(let ((qq-media-cache-update-hook nil))
     (qq-media-clear-cache)
     (unwind-protect
         (progn ,@body)
       (qq-media-clear-cache))))

(ert-deftest qq-media-session-avatar-projection-is-kind-exact ()
  (cl-letf (((symbol-function 'qq-account-get) (lambda (_account-id) t))
            ((symbol-function 'qq-media-avatar-display-string)
             (lambda (target-id) (format "user:%s" target-id)))
            ((symbol-function 'qq-media-group-avatar-display-string)
             (lambda (target-id) (format "group:%s" target-id)))
            ((symbol-function 'qq-media-avatar-cached-display-string)
             (lambda (target-id) (format "cached-user:%s" target-id)))
            ((symbol-function 'qq-media-group-avatar-cached-display-string)
             (lambda (target-id) (format "cached-group:%s" target-id))))
    (let ((private '((type . private) (target-id . "10001")))
          (group '((type . group) (target-id . "20001")))
          (dataline '((type . dataline) (target-id . "device")))
          (channel '((type . guild-channel) (target-id . "30001"))))
      (should (equal "avatar:10001"
                     (qq-media-session-avatar-cache-key private)))
      (should (equal "group-avatar:20001"
                     (qq-media-session-avatar-cache-key group)))
      (should-not (qq-media-session-avatar-cache-key dataline))
      (should-not (qq-media-session-avatar-cache-key channel))
      (should (equal "user:10001"
                     (qq-media-session-avatar-display-string private)))
      (should (equal "group:20001"
                     (qq-media-session-avatar-display-string group)))
      (should
       (equal "cached-user:10001"
              (qq-media-session-avatar-cached-display-string private)))
      (should
       (equal "cached-group:20001"
              (qq-media-session-avatar-cached-display-string group)))
      (should (equal "📱"
                     (qq-media-session-avatar-display-string dataline)))
      (should (equal "#"
                     (qq-media-session-avatar-display-string channel))))))

(ert-deftest qq-media-avatar-builder-prefers-appkit-circular-image ()
  (let (circle-arguments square-called-p)
    (cl-letf (((symbol-function 'appkit-media-circular-image-from-file)
               (lambda (&rest arguments)
                 (setq circle-arguments arguments)
                 'circular-avatar))
              ((symbol-function 'qq-media--image-from-file)
               (lambda (&rest _arguments)
                 (setq square-called-p t)
                 'square-avatar)))
      (should
       (eq 'circular-avatar
           (qq-media--avatar-image-from-file "/tmp/avatar.jpg" 20)))
      (should (equal circle-arguments '("/tmp/avatar.jpg" 20)))
      (should-not square-called-p))))

(ert-deftest qq-media-avatar-builder-keeps-square-capability-fallback ()
  (cl-letf (((symbol-function 'appkit-media-circular-image-from-file)
             (lambda (&rest _arguments) nil))
            ((symbol-function 'qq-media--image-from-file)
             (lambda (file pixel-size)
               (list 'square-avatar file pixel-size))))
    (should
     (equal '(square-avatar "/tmp/avatar.jpg" 20)
            (qq-media--avatar-image-from-file "/tmp/avatar.jpg" 20)))))


(ert-deftest qq-media-url-one-line-preview-reuses-resource-key ()
  (let ((key "poke-image-url:https://example.invalid/poke.png")
        (url "https://example.invalid/poke.png")
        captured)
    (cl-letf (((symbol-function 'qq-media--ensure-resource-image)
               (lambda (received-key fetcher spec builder)
                 (setq captured (list received-key spec builder))
                 (funcall fetcher #'ignore #'ignore)
                 'one-line-image)))
      (should (eq 'one-line-image
                  (qq-media-url-one-line-preview-image key url)))
      (should
       (equal
        (list key nil #'qq-media--one-line-preview-image-from-file)
        captured)))))

(ert-deftest qq-media-composer-preview-reuses-one-line-builder ()
  "Composer images should use the existing compact one-line builder."
  (let (captured)
    (cl-letf (((symbol-function 'qq-media--one-line-preview-image-from-file)
               (lambda (file spec)
                 (setq captured (list file spec))
                 'composer-image)))
      (should
       (eq 'composer-image
           (qq-media-composer-image-preview "/tmp/photo.png")))
      (should (equal captured '("/tmp/photo.png" nil))))))

(ert-deftest qq-media-ensure-resource-image-uses-existing-disk-cache ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-media-cache" t))
          (key "preview:test")
          (cache-file
           (format "%s.jpg" (qq-media--remote-image-cache-file-base key)))
          (fetch-called nil))
     (unwind-protect
         (progn
           (with-temp-file cache-file
             (insert "cached preview bytes"))
           (qq-media--cache-resource
            key '((url . "https://example.com/preview.jpg")))
           (should
            (equal
             (qq-media--ensure-resource-image
              key
              (lambda (_done _error)
                (setq fetch-called t))
              20
              (lambda (file spec)
                (list file spec)))
             (list cache-file 20)))
           (should-not fetch-called)
           (should (equal (alist-get 'file (qq-media--cached-resource key))
                          cache-file)))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-avatar-ignores-legacy-identity-only-disk-cache ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-media-cache" t))
          (key "avatar:10001")
          (resource '((url . "https://example.com/current-avatar.png")))
          (legacy-file
           (format "%s.jpg" (qq-media--remote-image-cache-file-base key)))
          started-key)
     (unwind-protect
         (progn
           (with-temp-file legacy-file
             (insert "stale identity-only avatar bytes"))
           (qq-media--cache-resource key resource)
           (cl-letf (((symbol-function 'qq-media--start-resource-image-download)
                      (lambda (download-key _resource _spec _builder)
                        (setq started-key download-key))))
             (should-not
              (qq-media--ensure-resource-image
               key
               (lambda (_done _error)
                 (ert-fail "fetcher should not run for cached avatar locator"))
               20
               (lambda (file spec) (list file spec))))
             (should (equal started-key key))))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-avatar-reuses-disk-cache-from-the-same-url ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-media-cache" t))
          (key "avatar:10001")
          (resource '((url . "https://example.com/current-avatar.png")))
          (cache-file
           (format "%s.png"
                   (qq-media--remote-image-cache-file-base key resource)))
          (fetch-called nil))
     (unwind-protect
         (progn
           (with-temp-file cache-file
             (insert "current avatar bytes"))
           (qq-media--cache-resource key resource)
           (should
            (equal
             (qq-media--ensure-resource-image
              key
              (lambda (_done _error) (setq fetch-called t))
              20
              (lambda (file spec) (list file spec)))
             (list cache-file 20)))
           (should-not fetch-called))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-clear-cache-removes-disk-cache-directory ()
  (let* ((qq-media-cache-directory (make-temp-file "qq-media-cache" t))
         (cache-file (expand-file-name "sample.jpg" qq-media-cache-directory)))
    (with-temp-file cache-file
      (insert "cached avatar bytes"))
    (should (file-exists-p cache-file))
    (qq-media-clear-cache)
    (should-not (file-exists-p cache-file))
    (should-not (file-directory-p qq-media-cache-directory))))

(ert-deftest qq-media-clear-cache-cancels-active-preview-downloads ()
  (let ((qq-media--fetching-cache (make-hash-table :test #'equal))
        canceled)
    (puthash "image:key" 'image-transfer qq-media--fetching-cache)
    (cl-letf (((symbol-function 'appkit-media-transfer-p)
               (lambda (object) (eq object 'image-transfer)))
              ((symbol-function 'appkit-media-cancel-transfer)
               (lambda (transfer) (push transfer canceled)))
              ((symbol-function 'appkit-media-cancel-video-preview) #'ignore))
      (qq-media-clear-cache)
      (should (equal '(image-transfer) canceled))
      (should-not (gethash "image:key" qq-media--fetching-cache)))))

(ert-deftest qq-media-clear-cache-cancels-shared-video-previews ()
  (let ((qq-media--resource-cache (make-hash-table :test #'equal))
        (qq-media--image-cache (make-hash-table :test #'equal))
        (qq-media--preview-missing-cache (make-hash-table :test #'equal))
        (qq-media--fetching-cache (make-hash-table :test #'equal))
        (qq-media--download-state-table (make-hash-table :test #'equal))
        (qq-media-cache-directory (make-temp-file "qq-clear-video" t))
        cancelled)
    (unwind-protect
        (progn
          (puthash "video:key" t qq-media--fetching-cache)
          (cl-letf (((symbol-function 'appkit-media-cancel-video-preview)
                     (lambda (key) (push key cancelled))))
            (qq-media-clear-cache))
          (should (equal cancelled '("qq:video:key")))
          (should-not (gethash "video:key" qq-media--fetching-cache)))
      (when (file-directory-p qq-media-cache-directory)
        (delete-directory qq-media-cache-directory t)))))

(ert-deftest qq-media-clear-cache-cancels-segment-download-transfers ()
  (let ((qq-media--resource-cache (make-hash-table :test #'equal))
        (qq-media--image-cache (make-hash-table :test #'equal))
        (qq-media--preview-missing-cache (make-hash-table :test #'equal))
        (qq-media--fetching-cache (make-hash-table :test #'equal))
        (qq-media--download-state-table (make-hash-table :test #'equal))
        (qq-media-cache-directory (make-temp-file "qq-clear-download" t))
        canceled)
    (puthash "download:key" '(:status downloading :transfer download-handle)
             qq-media--download-state-table)
    (cl-letf (((symbol-function 'appkit-media-transfer-p)
               (lambda (object) (eq object 'download-handle)))
              ((symbol-function 'appkit-media-cancel-transfer)
               (lambda (handle) (push handle canceled)))
              ((symbol-function 'appkit-media-cancel-video-preview) #'ignore))
      (qq-media-clear-cache)
      (should (equal '(download-handle) canceled))
      (should (= 0 (hash-table-count qq-media--download-state-table))))))

(ert-deftest qq-media-ensure-resource-image-starts-remote-download-for-url-only-resource ()
  (qq-media-test-with-reset
   (let (started-key started-resource started-spec)
     (cl-letf (((symbol-function 'qq-media--start-resource-image-download)
                (lambda (key resource spec _builder)
                  (setq started-key key)
                  (setq started-resource resource)
                  (setq started-spec spec))))
       (should-not
        (qq-media--ensure-resource-image
         "avatar:10001"
         (lambda (done _error)
           (funcall done '((url . "https://example.com/avatar.jpg"))))
         20
         (lambda (_file _spec)
           'image)))
       (should (equal started-key "avatar:10001"))
       (should (equal started-spec 20))
       (should (equal (alist-get 'url started-resource)
                      "https://example.com/avatar.jpg"))))))

(ert-deftest qq-media-resource-image-download-uses-shared-transfer-runtime ()
  (let* ((qq-media-cache-directory (make-temp-file "qq-media-transfer" t))
         (qq-media--fetching-cache (make-hash-table :test #'equal))
         (key "avatar:10001")
         (resource '((url . "https://example.com/avatar.jpg")
                     (name . "avatar.jpg")))
         captured-resource
         captured-base)
    (unwind-protect
        (progn
          (puthash key t qq-media--fetching-cache)
          (cl-letf (((symbol-function 'appkit-media-cache-image-resource-async)
                     (lambda (canonical cache-base _success _error
                                        &rest _arguments)
                       (setq captured-resource canonical
                             captured-base cache-base)
                       :image-transfer)))
            (qq-media--start-resource-image-download
             key resource 20 (lambda (_file _spec) :image)))
          (should (equal resource captured-resource))
          (should (equal (qq-media--remote-image-cache-file-base key resource)
                         captured-base))
          (should (eq :image-transfer
                      (gethash key qq-media--fetching-cache))))
      (delete-directory qq-media-cache-directory t))))

(ert-deftest qq-media-avatar-image-prefers-remote-refresh-over-stale-local-file ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-media-cache" t))
          (stale-file (make-temp-file "qq-stale-avatar" nil ".png"))
          (key "avatar:10001")
          started-key)
     (unwind-protect
         (progn
           (with-temp-file stale-file
             (insert "stale avatar bytes"))
           (qq-media--cache-resource
            key
            `((file . ,stale-file)
              (url . "https://example.com/avatar.jpg")))
           (cl-letf (((symbol-function 'qq-media--start-resource-image-download)
                      (lambda (download-key _resource _spec _builder)
                        (setq started-key download-key))))
             (should (equal (qq-media--ensure-resource-image
                             key
                             (lambda (_done _error)
                               (ert-fail "fetcher should not run for cached avatar resource"))
                             20
                             (lambda (file spec)
                               (list file spec)))
                            (list stale-file 20)))
             (should (equal started-key key))))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))
       (when (file-exists-p stale-file)
         (delete-file stale-file))))))

(ert-deftest qq-media-avatar-download-excludes-stale-local-transfer-source ()
  (let* ((qq-media-cache-directory (make-temp-file "qq-media-transfer" t))
         (qq-media--fetching-cache (make-hash-table :test #'equal))
         (stale-file (make-temp-file "qq-stale-avatar" nil ".png"))
         (key "avatar:10001")
         (resource
          `((file . ,stale-file)
            (url . "https://example.com/avatar.jpg")
            (name . "avatar.jpg")))
         captured-resource)
    (unwind-protect
        (progn
          (with-temp-file stale-file
            (insert "stale avatar bytes"))
          (puthash key t qq-media--fetching-cache)
          (cl-letf (((symbol-function 'appkit-media-cache-image-resource-async)
                     (lambda (canonical _cache-base _success _error
                                        &rest _arguments)
                       (setq captured-resource canonical)
                       :image-transfer)))
            (qq-media--start-resource-image-download
             key resource 20 (lambda (_file _spec) :image)))
          (should (equal "https://example.com/avatar.jpg"
                         (alist-get 'url captured-resource)))
          (should-not (alist-get 'file captured-resource))
          (should (equal stale-file (alist-get 'file resource)))
          (should (file-exists-p stale-file)))
      (when (file-directory-p qq-media-cache-directory)
        (delete-directory qq-media-cache-directory t))
      (when (file-exists-p stale-file)
        (delete-file stale-file)))))

(ert-deftest qq-media-non-avatar-image-keeps-local-file-preference ()
  (qq-media-test-with-reset
   (let ((local-file (make-temp-file "qq-local-image" nil ".png"))
         (started nil))
     (unwind-protect
         (progn
           (with-temp-file local-file
             (insert "local image bytes"))
           (qq-media--cache-resource
            "face:100"
            `((file . ,local-file)
              (url . "https://example.com/face.png")))
           (cl-letf (((symbol-function 'qq-media--start-resource-image-download)
                      (lambda (&rest _args)
                        (setq started t))))
             (should (equal (qq-media--ensure-resource-image
                             "face:100"
                             (lambda (_done _error)
                               (ert-fail "fetcher should not run for cached local resource"))
                             18
                             (lambda (file spec)
                               (list file spec)))
                            (list local-file 18)))
             (should-not started)))
       (when (file-exists-p local-file)
         (delete-file local-file))))))

(ert-deftest qq-media-ensure-resource-image-clears-fetching-on-error ()
  (qq-media-test-with-reset
   (should-not
    (qq-media--ensure-resource-image
     "avatar:10001"
     (lambda (_done error)
       (funcall error nil "boom"))
     20
     (lambda (_file _spec)
       'image)))
   (should-not (qq-media--resource-fetching-p "avatar:10001"))))

(ert-deftest qq-media-ensure-resource-image-contains-synchronous-fetch-failure ()
  (qq-media-test-with-reset
   (let (notified)
     (cl-letf (((symbol-function 'qq-media--note-cache-updated)
                (lambda (key) (setq notified key))))
       (should-not
        (qq-media--ensure-resource-image
         "avatar:10001"
         (lambda (_done _error)
           (error "account lifecycle unavailable"))
         20
         (lambda (_file _spec) 'image)))
       (should (equal "avatar:10001" notified))
       (should-not (qq-media--resource-fetching-p "avatar:10001"))))))

(ert-deftest qq-media-avatar-image-never-falls-back-to-onebot ()
  (qq-media-test-with-reset
   (let (legacy-called)
     (cl-letf (((symbol-function 'qq-api-get-avatar)
                (lambda (&rest _)
                  (setq legacy-called t))))
       (should-not (qq-media-avatar-image "10001"))
       (should-not legacy-called)
       (should-not (qq-media--resource-fetching-p "avatar:10001"))))))

(ert-deftest qq-media-native-avatar-prefers-friend-directory-url ()
  (let ((qq-state--friends-by-id (make-hash-table :test #'equal)))
    (puthash
     "10001"
     '((user_id . "10001")
       (avatar_url . "https://example.invalid/friend-avatar.png"))
     qq-state--friends-by-id)
    (should
     (equal
      (qq-media--native-user-avatar-resource "10001")
      '((url . "https://example.invalid/friend-avatar.png"))))))

(ert-deftest qq-media-user-avatar-locator-validates-owner-and-identity ()
  (let (sent-method sent-params resource)
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-account-get)
               (lambda (account-id)
                 (and (equal account-id "slot-a")
                      '((account_id . "slot-a")))))
              ((symbol-function 'qq-server-ready-p)
               (lambda () t))
              ((symbol-function 'qq-server-capabilities)
               (lambda () '("contact.get_user_avatar")))
              ((symbol-function 'qq-server-send)
               (lambda (method params callback _errback)
                 (setq sent-method method sent-params params)
                 (funcall
                  callback
                  '((account_id . "slot-a")
                    (user_uin . "9007199254740999")
                    (url
                     . "https://q.qlogo.cn/headimg_dl?dst_uin=9007199254740999&spec=640&img_type=jpg")))
                 "request-avatar")))
      (should
       (equal
        (qq-media--fetch-native-user-avatar-locator
         "9007199254740999"
         (lambda (value) (setq resource value)))
        "request-avatar"))
      (should (equal sent-method "contact.get_user_avatar"))
      (should
       (equal sent-params
              '((account_id . "slot-a")
                (user_uin . "9007199254740999"))))
      (should
       (equal
        resource
        '((url
           . "https://q.qlogo.cn/headimg_dl?dst_uin=9007199254740999&spec=640&img_type=jpg")))))))

(ert-deftest qq-media-group-avatar-locator-validates-owner-and-identity ()
  (let (sent-method sent-params resource)
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-account-get)
               (lambda (account-id)
                 (and (equal account-id "slot-a")
                      '((account_id . "slot-a")))))
              ((symbol-function 'qq-server-ready-p)
               (lambda () t))
              ((symbol-function 'qq-server-capabilities)
               (lambda () '("contact.get_group_avatar")))
              ((symbol-function 'qq-server-send)
               (lambda (method params callback _errback)
                 (setq sent-method method sent-params params)
                 (funcall
                  callback
                  '((account_id . "slot-a")
                    (group_uin . "8209413637")
                    (url
                     . "https://p.qlogo.cn/gh/8209413637/8209413637/640/")))
                 "request-group-avatar")))
      (should
       (equal
        (qq-media--fetch-native-group-avatar
         "8209413637"
         (lambda (value) (setq resource value)))
        "request-group-avatar"))
      (should (equal sent-method "contact.get_group_avatar"))
      (should
       (equal sent-params
              '((account_id . "slot-a")
                (group_uin . "8209413637"))))
      (should
       (equal
        resource
        '((url
           . "https://p.qlogo.cn/gh/8209413637/8209413637/640/")))))))

(ert-deftest qq-media-native-avatar-resolves-media-cache-miss ()
  (let ((qq-state--friends-by-id (make-hash-table :test #'equal))
        result call)
    (cl-letf (((symbol-function 'qq-account-current-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-server-capabilities)
               (lambda () '("contact.get_user_avatar")))
              ((symbol-function 'qq-media--fetch-native-user-avatar-locator)
               (lambda (user-id done &optional _error)
                 (setq call user-id)
                 (funcall
                  done
                  '((url . "https://example.invalid/resolved-avatar.png")))
                 "avatar-request")))
      (qq-media--fetch-native-user-avatar
       "10001" (lambda (resource) (setq result resource)) #'ignore)
      (should (equal call "10001"))
      (should
       (equal result
              '((url . "https://example.invalid/resolved-avatar.png")))))))

(ert-deftest qq-media-open-user-avatar-resolves-directory-cache-miss ()
  (qq-media-test-with-reset
   (let (opened)
     (cl-letf (((symbol-function 'qq-media--fetch-native-user-avatar)
                (lambda (user-id done _error)
                  (should (equal user-id "10001"))
                  (funcall
                   done
                   '((url . "https://example.invalid/resolved-avatar.png")))))
               ((symbol-function 'qq-media-open-resource)
                (lambda (resource kind key)
                  (setq opened (list resource kind key)))))
       (qq-media-open-user-avatar "10001")
       (should
        (equal
         opened
         '(((url . "https://example.invalid/resolved-avatar.png"))
           image "avatar:10001")))))))


(ert-deftest qq-media-forum-avatar-uses-authoritative-feed-url ()
  (let (profile-request fetch-resource)
    (cl-letf (((symbol-function 'qq-media--ensure-resource-image)
               (lambda (_key fetch _height &optional _factory)
                 (funcall fetch
                          (lambda (resource) (setq fetch-resource resource))
                          #'ignore)
                 'forum-avatar))
              ((symbol-function 'qq-media-guild-member-avatar-image)
               (lambda (&rest args) (setq profile-request args))))
      (should
       (eq
        (qq-media-message-avatar-image
         '((session-key
            . "guild:9007199254740993:channel:9007199254741999")
           (sender-native-id . "144115219000000001")
           (sender-avatar-url . "https://example.invalid/forum-avatar.png")))
        'forum-avatar))
      (should (equal fetch-resource
                     '((url . "https://example.invalid/forum-avatar.png"))))
      (should-not profile-request))))

(ert-deftest qq-media-forward-avatar-uses-url-instead-of-repeated-user-id ()
  (let ((url "https://example.test/forward-node.png")
        fetch-key fetch-resource user-request)
    (cl-letf (((symbol-function 'qq-media--ensure-resource-image)
               (lambda (key fetch _height &optional _factory)
                 (setq fetch-key key)
                 (funcall fetch
                          (lambda (resource) (setq fetch-resource resource))
                          #'ignore)
                 'forward-avatar))
              ((symbol-function 'qq-media-avatar-image)
               (lambda (user-id) (setq user-request user-id))))
      (should
       (eq
        (qq-media-message-avatar-image
         `((sender-id . "1094950020")
           (sender-avatar-url . ,url)))
        'forward-avatar))
      (should (equal fetch-key (concat "message-avatar-url:" url)))
      (should (equal fetch-resource `((url . ,url))))
      (should-not user-request))))

(ert-deftest qq-media-forward-avatar-cache-keys-are-url-scoped ()
  (let ((first
         '((sender-id . "1094950020")
           (sender-avatar-url . "https://example.test/first.png")))
        (second
         '((sender-id . "1094950020")
           (sender-avatar-url . "https://example.test/second.png"))))
    (should-not
     (equal (qq-media-message-avatar-cache-key first)
            (qq-media-message-avatar-cache-key second)))))


(ert-deftest qq-media-custom-face-preview-reuses-catalog-url-cache ()
  (let* ((face '((favorite_emoji_id . "favorite-a")
                 (md5 . "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                 (url . "https://example.invalid/a")))
         captured)
    (cl-letf (((symbol-function 'qq-media-url-preview-image)
               (lambda (key url height)
                 (setq captured (list key url height))
                 'favorite-image)))
      (should (eq (qq-media-custom-face-image face) 'favorite-image))
      (should (equal captured
                     (list "favorite-emoji:favorite-a"
                           "https://example.invalid/a"
                           (max qq-media-face-image-height 32))))
      (let ((prefix (qq-media--custom-face-completion-prefix face)))
        (should (eq (get-text-property 0 'display prefix)
                    'favorite-image))))))

(ert-deftest qq-media-refresh-custom-faces-delegates-to-native-catalog ()
  (let (requested-force received)
    (cl-letf (((symbol-function 'qq-favorite-emoji-list)
               (lambda (force callback _errback)
                 (setq requested-force force)
                 (funcall callback '((entries . (((favorite_emoji_id . "favorite-a"))))))
                 "favorite-request")))
      (should
       (equal
        (qq-media-refresh-custom-faces
         (lambda (faces) (setq received faces)) nil t)
        "favorite-request"))
      (should requested-force)
      (should (equal received '(((favorite_emoji_id . "favorite-a"))))))))

(ert-deftest qq-media-custom-face-to-segment-keeps-only-durable-id ()
  (let* ((favorite-id
          "10001_0_0_1_01F97FC1C8118A7D09DE81124B346F67_195359_1d5176b6464998bd07f94c17d42012e5")
         (face `((favorite_emoji_id . ,favorite-id)
                 (md5 . "01f97fc1c8118a7d09de81124b346f67")
                 (url . "https://example.invalid/favorite.png")))
         (segment (qq-media-custom-face-to-segment face)))
    (should
     (equal segment
            `((type . "favorite_emoji")
              (data . ((favorite_emoji_id . ,favorite-id))))))
    (should-not (string-match-p "https://" (prin1-to-string segment)))
    (should-not (string-match-p "resource_id\|attachment_id\|file"
                                (prin1-to-string segment)))))

(ert-deftest qq-media-face-uses-local-default-emoji-png ()
  "Base faces should render from LinuxQQ default-emojis without API."
  (qq-media-test-with-reset
   (let* ((dir (make-temp-file "qq-default-emojis" t))
          (png (expand-file-name "178.png" dir))
          (qq-media-default-emoji-directory dir)
          (qq-media--face-names-table (make-hash-table :test #'equal))
          (api-called nil))
     (unwind-protect
         (progn
           (puthash "178" "/斜眼笑" qq-media--face-names-table)
           ;; Minimal valid 1x1 PNG.
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
           (cl-letf (((symbol-function 'qq-media--system-emoji-cache-roots)
                      (lambda () nil))
                     ((symbol-function 'qq-api-call)
                      (lambda (&rest _args)
                        (setq api-called t)
                        (ert-fail "base-face rendering must not use OneBot"))))
             (should (equal (qq-media--local-base-emoji-file "178") png))
             (should (equal (qq-media-face-text-fallback "178") "/斜眼笑"))
             (let ((image (qq-media-face-image "178")))
               (should image)
               (should (eq (car image) 'image)))
             (let ((display (qq-media-face-display-string "178")))
               (should (get-text-property 0 'display display)))
             (should-not api-called)))
       (when (file-directory-p dir)
         (delete-directory dir t))))))

(ert-deftest qq-media-missing-base-face-uses-text-without-onebot ()
  "A missing local face must not interrupt native chat rendering."
  (let ((qq-media-default-emoji-directory
         (make-temp-file "qq-empty-default-emojis" t))
        (api-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'qq-media--system-emoji-cache-roots)
                   (lambda () nil))
                  ((symbol-function 'qq-api-call)
                   (lambda (&rest _args)
                     (setq api-called t)
                     (ert-fail "base-face rendering must not use OneBot"))))
          (should-not (qq-media-face-image "178"))
          (should (equal (qq-media-face-display-string "178") "/斜眼笑"))
          (should-not api-called))
      (delete-directory qq-media-default-emoji-directory t))))

(ert-deftest qq-media-dynamic-system-face-cache-exposes-image-and-lottie ()
  (qq-media-test-with-reset
   (let* ((root (make-temp-file "qq-system-emoji" t))
          (png-dir (expand-file-name "493/png" root))
          (lottie-dir (expand-file-name "493/lottie" root))
          (png (expand-file-name "493.png" png-dir))
          (lottie (expand-file-name "493.json" lottie-dir))
          (qq-media-default-emoji-directory
           (make-temp-file "qq-empty-default-emojis" t)))
     (unwind-protect
         (progn
           (make-directory png-dir t)
           (make-directory lottie-dir t)
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
           (with-temp-file lottie
             (insert "{\"v\":\"5.12.1\",\"fr\":60,\"ip\":0,\"op\":1,"
                     "\"w\":1,\"h\":1,\"layers\":[]}"))
           (cl-letf (((symbol-function 'qq-media--system-emoji-cache-roots)
                      (lambda () (list root))))
             (should (equal (qq-media--local-base-emoji-file "493") png))
             (should
              (equal (qq-media--local-base-emoji-lottie-file "493") lottie))
             (let ((display (qq-media-face-display-string "493" "/睡觉")))
               (should (get-text-property 0 'display display))
               (should (equal
                        (get-text-property 0 'qq-system-face-id display)
                        "493")))))
       (delete-directory root t)
       (delete-directory qq-media-default-emoji-directory t)))))

(ert-deftest qq-media-face-segment-retains-animated-catalog-metadata ()
  (let ((qq-media--system-emoji-tables (make-hash-table :test #'equal))
        (table (make-hash-table :test #'equal)))
    (puthash
     "493"
     '((id . "493") (description . "/睡觉") (emoji_type . 1)
       (animated_pack_id . 1) (animated_sticker_id . 77))
     table)
    (puthash "slot-a" table qq-media--system-emoji-tables)
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a")))
      (should
       (equal
        (qq-media-face-to-segment "493")
        '((type . "face")
          (data . ((id . "493") (face_type . "animated")
                   (pack_id . 1) (sticker_id . 77)
                   (description . "/睡觉")))))))))





(ert-deftest qq-media-message-one-line-preview-projects-primary-segment ()
  (let* ((segment
          '((type . "image")
            (data . ((file . "cached-image.png")))))
         (message `((segments . (,segment))))
         (image '(image :type png :data "bytes")))
    (cl-letf (((symbol-function
                'qq-media-segment-one-line-preview-image)
               (lambda (candidate)
                 (should (equal candidate segment))
                 image)))
      (let ((preview
             (qq-media-message-one-line-preview message "[image]")))
        (should (equal "[image]"
                       (appkit-ui-one-line-preview-text preview)))
        (should (= qq-media-one-line-preview-columns
                   (appkit-ui-one-line-preview-visual-columns preview)))
        (let ((display
               (get-text-property
                0 'display (appkit-ui-one-line-preview-visual preview))))
          (should (eq 'slice (caar display)))
          (should
           (equal "bytes"
                  (plist-get (cdr (cadr display)) :data))))
        (should
         (equal (list (qq-media-segment-preview-key segment))
                (qq-media-message-one-line-preview-keys message)))))))

(ert-deftest qq-media-segment-preview-image-uses-local-file-without-api ()
  (qq-media-test-with-reset
   (let* ((local-file (make-temp-file "qq-preview" nil ".png"))
          (segment `((type . "image")
                     (data . ((file . ,local-file)))))
          (api-called nil)
          image)
     (unwind-protect
         (progn
           (with-temp-file local-file
             (insert "png-bytes"))
           (cl-letf (((symbol-function 'qq-api-call)
                      (lambda (&rest _args)
                        (setq api-called t)
                        nil))
                     ((symbol-function 'qq-media--preview-image-from-file)
                      (lambda (file _spec)
                        (list 'preview file))))
             (setq image (qq-media-segment-preview-image segment))
             (should (equal image (list 'preview local-file)))
             (should-not api-called)
             ;; Second call hits image cache.
             (should (equal (qq-media-segment-preview-image segment)
                            (list 'preview local-file)))))
       (when (file-exists-p local-file)
         (delete-file local-file))))))

(ert-deftest qq-media-face-completion-sorted-by-numeric-id ()
  "Base face picker must list faces in QQ id order, not hash/string order."
  (let ((qq-media--face-names-table (make-hash-table :test #'equal)))
    (puthash "10" "/尴尬" qq-media--face-names-table)
    (puthash "2" "/色" qq-media--face-names-table)
    (puthash "0" "/惊讶" qq-media--face-names-table)
    (puthash "178" "/斜眼笑" qq-media--face-names-table)
    (let ((cands (qq-media-face-completion-candidates)))
      (should (equal cands
                     '("/惊讶  (0)"
                       "/色  (2)"
                       "/尴尬  (10)"
                       "/斜眼笑  (178)")))
      (should (equal (qq-media-face-id-from-completion (car cands)) "0"))
      (should (equal (qq-media-face-id-from-completion "/斜眼笑  (178)")
                     "178")))))

(ert-deftest qq-media-face-completion-table-metadata ()
  "Completion table must pin sort order and declare affixation."
  (let ((qq-media--face-names-table (make-hash-table :test #'equal)))
    (puthash "0" "/惊讶" qq-media--face-names-table)
    (puthash "1" "/撇嘴" qq-media--face-names-table)
    (let* ((table (qq-media-face-completion-table))
           (meta (funcall table "" nil 'metadata)))
      (should (eq (car meta) 'metadata))
      (should (eq (completion-metadata-get meta 'display-sort-function)
                  #'identity))
      (should (eq (completion-metadata-get meta 'cycle-sort-function)
                  #'identity))
      (should (eq (completion-metadata-get meta 'affixation-function)
                  #'qq-media-face-affixation-function))
      (should (equal (all-completions "" table)
                     '("/惊讶  (0)" "/撇嘴  (1)"))))))

(ert-deftest qq-media-face-affixation-uses-local-png ()
  "Picker affix should show the local default-emoji PNG when present."
  (let* ((dir (make-temp-file "qq-emoji-affix" t))
         (qq-media-default-emoji-directory dir)
         (qq-media-face-image-height 18)
         (png (expand-file-name "0.png" dir)))
    (unwind-protect
        (progn
          ;; Minimal valid 1x1 PNG.
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
          (let* ((affixed (qq-media-face-affixation-function
                           '("/惊讶  (0)" "/missing  (999)")))
                 (prefix0 (nth 1 (nth 0 affixed)))
                 (prefix1 (nth 1 (nth 1 affixed))))
            (should (get-text-property 0 'display prefix0))
            (should (eq (car (get-text-property 0 'display prefix0)) 'image))
            ;; Missing id keeps a plain spacer (no image property).
            (should-not (get-text-property 0 'display prefix1))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest qq-media-open-resource-adapts-shared-backend-and-cache ()
  "QQ supplies only its cache policy to the shared media opener."
  (let ((qq-media-cache-directory "/tmp/qq-media-cache/")
        (qq-media--resource-cache (make-hash-table :test #'equal))
        (owner (list 'exact-owner))
        captured)
    (cl-letf (((symbol-function 'appkit-media-open-resource)
               (lambda (&rest arguments)
                 (setq captured arguments)
                 (funcall
                  (plist-get (cdr arguments) :cache-update-function)
                  '((file . "/tmp/cat.png")
                    (url . "https://example.com/cat.png")))
                 'opened)))
      (should (eq 'opened
                  (qq-media-open-resource
                   '((url . "https://example.com/cat.png"))
                   'image
                   "image:test"
                   :owner owner)))
      (should (equal (nth 0 captured)
                     '((url . "https://example.com/cat.png"))))
      (should (eq (plist-get (cdr captured) :kind) 'image))
      (should (equal (plist-get (cdr captured) :cache-key) "image:test"))
      (should (equal (plist-get (cdr captured) :cache-directory)
                     qq-media-cache-directory))
      (should (equal (plist-get (cdr captured) :client-label) "qq"))
      (should (eq (plist-get (cdr captured) :owner) owner))
      (should (equal (alist-get 'file
                                (qq-media--cached-resource "image:test"))
                     "/tmp/cat.png")))))

(ert-deftest qq-media-open-avatar-scopes-disk-cache-to-url ()
  (let* ((resource '((file . "/tmp/stale-avatar.png")
                     (url . "https://example.com/current-avatar.png")))
         captured)
    (cl-letf (((symbol-function 'appkit-media-open-resource)
               (lambda (&rest arguments) (setq captured arguments))))
      (qq-media-open-resource resource 'image "avatar:10001")
      (should
       (equal (car captured)
              '((url . "https://example.com/current-avatar.png"))))
      (should
       (equal
        (plist-get (cdr captured) :cache-key)
        (qq-media--remote-image-cache-key "avatar:10001" resource)))
      (should (equal (alist-get 'file resource) "/tmp/stale-avatar.png")))))

(ert-deftest qq-media-open-video-file-segment-delegates-to-player ()
  "An mp4 delivered as a file segment still takes the video-player path."
  (let* ((segment '((type . "file")
                    (data . ((name . "movie.mp4")
                             (url . "https://example.com/movie.mp4")))))
         (owner (list 'exact-owner))
         played-source played-owner played-cache-key)
    (cl-letf (((symbol-function 'qq-media-segment-local-file)
               (lambda (_segment) nil))
              ((symbol-function 'qq-media-resolve-segment-resource)
               (lambda (_segment callback &optional _errback)
                 (funcall callback
                          '((url . "https://example.com/movie.mp4")))))
              ((symbol-function 'appkit-media-play-video-source)
               (lambda (source &optional _client-label &rest keys)
                 (setq played-source source
                       played-owner (plist-get keys :owner)
                       played-cache-key (plist-get keys :cache-key)))))
      (qq-media-segment-open segment :owner owner)
      (should (equal played-source "https://example.com/movie.mp4"))
      (should (eq played-owner owner))
      (should
       (equal played-cache-key
              (qq-media--segment-resource-key segment))))))

(ert-deftest qq-media-video-segments-are-inline-preview-capable ()
  (should
   (qq-media-segment-preview-capable-p
    '((type . "video")
      (data . ((path . "/tmp/short.mp4")
               (file_size . "2048")
               (remote_status . "unavailable"))))))
  (should
   (qq-media-segment-preview-capable-p
    '((type . "file")
      (data . ((name . "short.mp4")
               (url . "https://example.com/short.mp4")))))))

(ert-deftest qq-media-video-preview-reuses-shared-animation-pipeline ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-video-preview" t))
          (source (make-temp-file "qq-short-video" nil ".mp4"))
          (segment `((type . "video")
                     (data . ((path . ,source)
                              (name . "short.mp4")
                              (file_size . "2048")
                              (duration_secs . 4.5)
                              (remote_status . "unavailable")))))
          (key (qq-media-segment-preview-key segment))
          (image '(image :type gif :appkit-media-inline-animation t))
          captured updated)
     (unwind-protect
         (cl-letf (((symbol-function 'appkit-media-start-video-preview)
                    (lambda (&rest arguments)
                      (setq captured arguments)
                      (funcall (plist-get arguments :callback)
                               image "/tmp/preview.gif")))
                   ((symbol-function 'qq-media--note-cache-updated)
                    (lambda (media-key) (setq updated media-key)))
                   ((symbol-function 'appkit-media-video-preview-display-image)
                    (lambda (candidate &optional _namespace) candidate))
                   ((symbol-function 'image-size)
                    (lambda (&rest _args) '(16 . 16))))
           (qq-media-segment-preview-image segment)
           (should (equal (plist-get captured :key) (concat "qq:" key)))
           (should (equal (plist-get captured :source) source))
           (should (equal (plist-get captured :source-size) "2048"))
           (should (= (plist-get captured :duration) 4.5))
           (should (eq image (gethash key qq-media--image-cache)))
           (should-not (gethash key qq-media--fetching-cache))
           (should (eq image (qq-media-segment-preview-image segment)))
           (should (equal updated key)))
       (when (file-exists-p source) (delete-file source))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-video-preview-uses-local-poster-without-video-source ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-video-poster" t))
          (poster (make-temp-file "qq-video-poster" nil ".png"))
          (segment `((type . "video")
                     (data . ((path . "/missing/video.mp4")
                              (thumb . ,poster)
                              (name . "video.mp4")
                              (file_size . "2048")
                              (remote_status . "unavailable")))))
          (image '(image :type jpeg))
          captured)
     (unwind-protect
         (cl-letf (((symbol-function 'appkit-media-start-video-preview)
                    (lambda (&rest arguments)
                      (setq captured arguments)
                      (funcall (plist-get arguments :callback)
                               image "/tmp/poster.jpg")))
                   ((symbol-function 'appkit-media-video-preview-display-image)
                    (lambda (candidate &optional _namespace) candidate)))
           (qq-media-segment-preview-image segment)
           (should-not (plist-get captured :source))
           (should (equal (plist-get captured :preview-source) poster))
           (should (equal (plist-get captured :source-size) "2048")))
       (when (file-exists-p poster) (delete-file poster))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-video-poster-never-becomes-playable-local-source ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-video-cache" t))
          (poster (make-temp-file "qq-video-poster" nil ".jpg"))
          (segment '((type . "video")
                     (data . ((file . "video-handle")
                              (path . "/missing/video.mp4")
                              (remote_status . "unavailable")))))
          (preview-key (qq-media-segment-preview-key segment))
          (disk-poster
           (format "%s.jpg"
                   (qq-media--remote-image-cache-file-base preview-key))))
     (unwind-protect
         (progn
           (qq-media--cache-resource preview-key `((file . ,poster)))
           (should-not (qq-media-segment-local-file segment))
           (remhash preview-key qq-media--resource-cache)
           (with-temp-file disk-poster
             (insert "poster bytes"))
           (should-not (qq-media-segment-local-file segment))
           (let ((capabilities (qq-media-segment-capabilities segment)))
             (should-not (plist-get capabilities :local-file))
             (should-not (plist-get capabilities :open))))
       (when (file-exists-p poster) (delete-file poster))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))




(ert-deftest qq-media-real-download-invalidates-negative-video-preview ()
  (qq-media-test-with-reset
   (let* ((file (make-temp-file "qq-downloaded-video" nil ".mp4"))
          (segment '((type . "video")
                     (data . ((file . "clip.mp4")
                              (remote_status . "available")
                              (url . "https://video.example/clip")))))
          (preview-key (qq-media-segment-preview-key segment)))
     (unwind-protect
         (progn
           (puthash preview-key t qq-media--preview-missing-cache)
           (qq-media--put-segment-download-state
            segment `(:status downloaded :path ,file))
           (should-not (gethash preview-key qq-media--preview-missing-cache)))
       (delete-file file)))))

(ert-deftest qq-media-video-play-provides-an-asynchronous-error-callback ()
  (let ((segment '((type . "video") (data . nil)))
        supplied-error
        displayed)
    (cl-letf (((symbol-function 'qq-media-segment-playable-p)
               (lambda (_segment) t))
              ((symbol-function 'qq-media-segment-local-file)
               (lambda (_segment) nil))
              ((symbol-function 'qq-media-resolve-segment-resource)
               (lambda (_segment _success &optional error)
                 (setq supplied-error error)
                 (funcall error nil "manual resolution failed")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq displayed (apply #'format format-string args)))))
      (qq-media-segment-play segment))
    (should (functionp supplied-error))
    (should (equal displayed
                   "qq: failed to play video: manual resolution failed"))))

(ert-deftest qq-media-video-play-keeps-owner-across-runtime-replacement ()
  "A late resolver callback cannot transfer its player to a same-id app."
  (let* ((segment '((type . "video") (data . nil)))
         (old-app (appkit-app-start 'qq :id 'default :shutdown #'ignore))
         (qq-runtime--app old-app)
         replacement resolver-success played-owner played-source)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-media-segment-playable-p)
                   (lambda (_segment) t))
                  ((symbol-function 'qq-media-segment-local-file)
                   (lambda (_segment) nil))
                  ((symbol-function 'qq-media-resolve-segment-resource)
                   (lambda (_segment success &optional _error)
                     (setq resolver-success success)))
                  ((symbol-function 'appkit-media-play-video-source)
                   (lambda (source &optional _client-label &rest keys)
                     (setq played-source source
                           played-owner (plist-get keys :owner)))))
          (qq-media-segment-play segment :owner old-app)
          (should (functionp resolver-success))
          (appkit-app-close old-app)
          (setq replacement
                (appkit-app-start 'qq :id 'default :shutdown #'ignore)
                qq-runtime--app replacement)
          (funcall resolver-success
                   '((url . "https://example.com/late.mp4")))
          (should (equal played-source "https://example.com/late.mp4"))
          (should (eq played-owner old-app))
          (should-not (eq played-owner replacement))
          (should-not (appkit-app-live-p old-app)))
      (when (appkit-app-live-p old-app)
        (appkit-app-close old-app))
      (when (appkit-app-live-p replacement)
        (appkit-app-close replacement)))))

(ert-deftest qq-media-video-process-follows-exact-account-generation ()
  "Account stop kills its real player without touching a replacement's one."
  (let ((shell (executable-find "sh"))
        (sleeper (executable-find "sleep")))
    (skip-unless (and shell sleeper))
    (let* ((source (make-temp-file "qq-media-player-" nil ".mp4"))
           ;; SOURCE is appended after these arguments.  The shell receives it
           ;; as $2 while executing the absolute sleep program from $1.
           (appkit-media-video-player-command
            (list shell "-c" "exec \"$1\" 30" "qq-media-player" sleeper))
           (segment '((type . "video") (data . nil)))
           (qq-runtime--app nil)
           old-app replacement-app old-process replacement-process)
      (unwind-protect
          (cl-letf (((symbol-function 'qq-media-segment-playable-p)
                     (lambda (_segment) t))
                    ((symbol-function 'qq-media-segment-local-file)
                     (lambda (_segment) source)))
            (setq old-app
                  (appkit-app-start 'qq :id 'default :shutdown #'ignore)
                  qq-runtime--app old-app
                  old-process
                  (qq-media-segment-play segment :owner old-app))
            (should (process-live-p old-process))
            (qq-runtime-stop)
            (should-not (process-live-p old-process))

            (setq replacement-app
                  (appkit-app-start 'qq :id 'default :shutdown #'ignore)
                  qq-runtime--app replacement-app
                  replacement-process
                  (qq-media-segment-play segment :owner replacement-app))
            (should (process-live-p replacement-process))
            ;; Re-stopping the exact old generation is inert for the same-id
            ;; replacement and its independently owned process.
            (appkit-app-close old-app)
            (should (process-live-p replacement-process))
            (qq-runtime-stop)
            (should-not (process-live-p replacement-process)))
        (dolist (process (list old-process replacement-process))
          (when (processp process)
            (set-process-sentinel process nil)
            (when (process-live-p process)
              (delete-process process))))
        (when (appkit-app-live-p old-app)
          (appkit-app-close old-app))
        (when (appkit-app-live-p replacement-app)
          (appkit-app-close replacement-app))
        (when (file-exists-p source)
          (delete-file source))))))

;; Strict video remote-status model overrides for the pre-wire-model fixtures.

(ert-deftest qq-media-expired-video-does-not-resolve-a-remote-resource ()
  (qq-media-test-with-reset
   (let ((segment '((type . "video")
                    (data . ((file . "expired-video-token")
                             (remote_status . "expired")))))
         api-called resolved failure)
     (cl-letf (((symbol-function 'qq-api-call)
                (lambda (&rest _) (setq api-called t))))
       (let ((caps (qq-media-segment-capabilities segment)))
         (should (equal (plist-get caps :status) "Expired"))
         (should-not (plist-get caps :open))
         (should-not (plist-get caps :download))
         (should-not (plist-get caps :save))
         (should-not (plist-get caps :copy-url)))
       (qq-media-resolve-segment-resource
        segment
        (lambda (resource) (setq resolved resource))
        (lambda (_response reason) (setq failure reason)))
       (should-not api-called)
       (should-not resolved)
       (should (equal failure "video resource has expired"))))))

(ert-deftest qq-media-expired-video-keeps-an-existing-local-copy-usable ()
  (qq-media-test-with-reset
   (let* ((file (make-temp-file "qq-expired-video" nil ".mp4"))
          (segment `((type . "video")
                     (data . ((path . ,file)
                              (remote_status . "expired")))))
          resolved)
     (unwind-protect
         (let ((caps (qq-media-segment-capabilities segment)))
           (should (plist-get caps :open))
           (should (plist-get caps :save))
           (should-not (plist-get caps :download))
           (should-not (plist-get caps :copy-url))
           (should (equal (plist-get caps :status) "Expired"))
           (qq-media-resolve-segment-resource
            segment (lambda (resource) (setq resolved resource)))
           (should (equal (alist-get 'file resolved) file)))
       (when (file-exists-p file) (delete-file file))))))

(ert-deftest qq-media-nonstring-remote-status-is-invalid ()
  (let ((caps
         (qq-media-segment-capabilities
          '((type . "video")
            (data . ((file . "token") (remote_status . :false)))))))
    (should (eq (plist-get caps :remote-status) 'invalid))
    (should (equal (plist-get caps :status) "Invalid remote status"))
    (should-not (plist-get caps :open))))

(ert-deftest qq-media-video-terminal-remote-status-capabilities ()
  (dolist (case
           '(("unavailable" "Unavailable" "video resource is unavailable")
             ("unresolved" "Unresolved" "video resource is unresolved")))
    (pcase-let ((`(,wire ,status ,reason) case))
      (let* ((segment `((type . "video")
                        (data . ((file . "remote-token")
                                 (remote_status . ,wire)))))
             (caps (qq-media-segment-capabilities segment))
             api-called failure)
        (should (equal (plist-get caps :status) status))
        (dolist (key '(:open :download :save :copy-url :resolve-remote))
          (should-not (plist-get caps key)))
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (&rest _) (setq api-called t))))
          (qq-media-resolve-segment-resource
           segment #'ignore
           (lambda (_response text) (setq failure text))))
        (should-not api-called)
        (should (equal failure reason))))))



(ert-deftest qq-media-resolvable-video-prefers-a-real-local-file ()
  (qq-media-test-with-reset
   (let* ((file (make-temp-file "qq-local-video" nil ".mp4"))
          (segment
           `((type . "video")
             (data . ((file . "video.mp4")
                      (path . ,file)
                      (remote_status . "resolvable")
                      (resolver
                       . ((kind . "snapshot")
                          (peer . ((chat_type . 2)
                                   (peer_uid . "20001")
                                   (guild_id . "")))
                          (file_uuid . "native-file-uuid")))))))
          api-called resolved)
     (unwind-protect
         (cl-letf (((symbol-function 'qq-api-resolve-video)
                    (lambda (&rest _args) (setq api-called t))))
           (qq-media-resolve-segment-resource
            segment (lambda (resource) (setq resolved resource)))
           (should-not api-called)
           (should (equal (alist-get 'file resolved) file)))
       (when (file-exists-p file) (delete-file file))))))

(ert-deftest qq-media-available-video-uses-only-wire-url ()
  (qq-media-test-with-reset
   (let* ((url "https://example.com/movie.mp4")
          (segment `((type . "video")
                     (data . ((file . "must-not-go-to-get-file")
                              (url . ,url)
                              (remote_status . "available")))))
          api-called resolved)
     (let ((caps (qq-media-segment-capabilities segment)))
       (dolist (key '(:open :download :save :copy-url :resolve-remote))
         (should (plist-get caps key)))
       (should (equal (plist-get caps :remote-url) url)))
     (cl-letf (((symbol-function 'qq-api-call)
                (lambda (&rest _) (setq api-called t))))
       (qq-media-resolve-segment-resource
        segment (lambda (resource) (setq resolved resource))))
     (should-not api-called)
     (should (equal resolved `((url . ,url)))))))

(ert-deftest qq-media-video-missing-status-or-available-without-url-is-invalid ()
  (dolist (segment
           '(((type . "video") (data . ((file . "token"))))
             ((type . "video")
              (data . ((file . "token") (remote_status . "bogus"))))
             ((type . "video")
              (data . ((file . "token") (remote_status . "available"))))))
    (let ((caps (qq-media-segment-capabilities segment)))
      (should-not (plist-get caps :resolve-remote))
      (should-not (plist-get caps :open))
      (if (equal (alist-get 'remote_status (alist-get 'data segment))
                 "available")
          (should (equal (plist-get caps :remote-error)
                         "available video resource has no URL"))
        (should (eq (plist-get caps :remote-status) 'invalid))))))

(ert-deftest qq-media-segment-kind-recognizes-media-file-urls ()
  (should
   (eq (qq-media-segment-kind
        '((type . "file")
          (data . ((file . "https://example.com/picture.gif?token=1")))))
       'image))
  (let ((video '((type . "file")
                 (data . ((name . "movie.MP4#fragment")
                          (url . "https://example.com/movie.mp4"))))))
    (should (eq (qq-media-segment-kind video) 'video))
    (should (qq-media-segment-playable-p video))))

(ert-deftest qq-media-download-setup-error-is-retryable ()
  (qq-media-test-with-reset
   (let* ((path (make-temp-name "/tmp/qq-media-download-error-"))
          (segment '((type . "file")
                     (data . ((file . "remote-token")
                              (name . "report.pdf")))))
          (capabilities
           `(:download t
		       :download-state (:status not-downloaded :path ,path))))
     (cl-letf (((symbol-function 'qq-media-segment-capabilities)
                (lambda (_segment) capabilities))
               ((symbol-function 'qq-media-resolve-segment-resource)
                (lambda (_segment success &optional _error)
                  (funcall success
                           '((url . "https://example.invalid/report.pdf")))))
               ((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda (&rest _arguments)
                  (error "queue unavailable")))
               ((symbol-function 'message) #'ignore))
       (qq-media-segment-start-download segment)
       (let ((state (qq-media-segment-download-state segment)))
         (should (eq 'error (plist-get state :status)))
         (should (string-match-p "queue unavailable"
                                 (plist-get state :error))))))))

(ert-deftest qq-media-segment-download-retains-cancelable-transfer-handle ()
  (qq-media-test-with-reset
   (let* ((path (make-temp-name "/tmp/qq-media-download-handle-"))
          (segment '((type . "file")
                     (data . ((file . "remote-token")
                              (name . "report.pdf")))))
          (capabilities
           `(:download t
		       :download-state (:status not-downloaded :path ,path))))
     (cl-letf (((symbol-function 'qq-media-segment-capabilities)
                (lambda (_segment) capabilities))
               ((symbol-function 'qq-media-resolve-segment-resource)
                (lambda (_segment success &optional _error)
                  (funcall success
                           '((url . "https://example.invalid/report.pdf")))))
               ((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda (&rest _arguments) 'download-handle))
               ((symbol-function 'message) #'ignore))
       (qq-media-segment-start-download segment)
       (let ((state (qq-media-segment-download-state segment)))
         (should (eq 'downloading (plist-get state :status)))
         (should (eq 'download-handle (plist-get state :transfer)))
         (should (symbolp (plist-get state :token))))))))

(ert-deftest qq-media-cached-avatar-rendering-never-starts-fetches ()
  (let (api-called)
    (cl-letf (((symbol-function 'qq-media--cached-image) (lambda (_key) nil))
              ((symbol-function 'qq-api-get-avatar)
               (lambda (&rest _args) (setq api-called t)))
              ((symbol-function 'qq-media--fetch-native-group-avatar)
               (lambda (&rest _args) (setq api-called t))))
      (should (equal (qq-media-avatar-cached-display-string "10001") "@"))
      (should (equal (qq-media-group-avatar-cached-display-string "20001") "#"))
      (should-not api-called))))

(ert-deftest qq-media-native-record-capabilities-never-expose-a-resource-path ()
  (let* ((media-id "media-00112233-4455-6677-8899-aabbccddeeff")
         (segment `((type . "record")
                    (data . ((duration_seconds . 17)
                             (media_id . ,media-id)))))
         (qq-media--native-record-playbacks (make-hash-table :test #'equal))
         (qq-remote-media--media (make-hash-table :test #'equal))
         (account-id "10001"))
    (puthash media-id
             `((media_id . ,media-id)
               (content
                . ((phase . "materializing")
                   (bytes_done . "4096")
                   (bytes_total . "8192"))))
             qq-remote-media--media)
    (cl-letf (((symbol-function 'qq-account-current-id)
               (lambda () account-id))
              ((symbol-function 'qq-server-ready-p)
               (lambda () t))
              ((symbol-function 'qq-rpc-method-available-p)
               (lambda (_method) t))
              ((symbol-function 'qq-media-native-record-playback-available-p)
               (lambda () t)))
      (let ((caps (qq-media-segment-capabilities segment)))
        (should (plist-get caps :open))
        (should (equal (plist-get caps :status)
                       "Preparing 4096/8192 bytes"))
        (dolist (key '(:download :save :copy-url :local-file
				 :resolve-remote :remote-url))
          (should-not (plist-get caps key)))
        (should (equal (qq-media--segment-resource-key segment)
                       (concat "record:" media-id))))
      (puthash
       media-id
       (list :session
             (appkit-media-player-session--create
              :status 'paused :played-seconds 3.0))
       qq-media--native-record-playbacks)
      (let ((caps (qq-media-segment-capabilities segment)))
        (should (plist-get caps :open))
        (should (equal (plist-get caps :status) "Paused"))))))

(ert-deftest qq-media-segment-transfer-uses-native-byte-counts ()
  (let* ((media-id "media-00112233-4455-6677-8899-aabbccddeeff")
         (segment `((type . "record")
                    (data . ((duration_seconds . 17)
                             (media_id . ,media-id)))))
         (qq-remote-media--media (make-hash-table :test #'equal))
         canceled)
    (puthash media-id
             `((media_id . ,media-id)
               (content
                . ((phase . "materializing")
                   (bytes_done . "4096")
                   (bytes_total . "8192"))))
             qq-remote-media--media)
    (cl-letf (((symbol-function 'qq-media-segment-download-state)
               (lambda (_segment) '(:status not-downloaded)))
              ((symbol-function 'qq-remote-media-cancel)
               (lambda (id &rest _)
                 (setq canceled id))))
      (let ((transfer (qq-media-segment-transfer segment)))
        (should (eq (plist-get transfer :direction) 'download))
        (should (eq (plist-get transfer :state) 'active))
        (should (equal (plist-get transfer :bytes-done) "4096"))
        (should (equal (plist-get transfer :bytes-total) "8192"))
        (should (functionp (plist-get transfer :action)))
        (funcall (plist-get transfer :action))
        (should (equal canceled media-id))))))

(ert-deftest qq-media-native-video-routes-content-and-thumbnail-by-part ()
  (let* ((media-id "media-11223344-5566-7788-99aa-bbccddeeff00")
         (segment `((type . "video")
                    (data . ((width . 640)
                             (height . 360)
                             (duration_seconds . 23)
                             (media_id . ,media-id)))))
         content-call thumbnail-call)
    (should
     (equal
      (qq-media--segment-resource-key segment)
      (qq-media--native-video-key media-id)))
    (should
     (equal
      (qq-media-segment-preview-key segment)
      (qq-media--native-video-thumbnail-key media-id)))
    (cl-letf (((symbol-function 'qq-media--fetch-native-video-resource)
               (lambda (called-segment key callback _errback)
                 (setq content-call (list called-segment key))
                 (funcall callback '((file . "/tmp/video.mp4")))))
              ((symbol-function
                'qq-media--fetch-native-video-thumbnail-resource)
               (lambda (called-segment key done _error)
                 (setq thumbnail-call (list called-segment key))
                 (funcall done '((file . "/tmp/poster.png")))))
              ((symbol-function 'qq-media--ensure-resource-image)
               (lambda (_key fetcher _spec &optional _builder)
                 (funcall fetcher #'ignore #'ignore)
                 'poster-image))
              ((symbol-function 'appkit-media-video-preview-display-image)
               (lambda (image _client) image)))
      (let (resolved)
        (qq-media--fetch-segment-resource
         segment (lambda (resource) (setq resolved resource)) #'ignore)
        (should (equal resolved '((file . "/tmp/video.mp4"))))
        (should
         (equal
          content-call
          (list segment (qq-media--native-video-key media-id)))))
      (should (eq (qq-media-segment-preview-image segment) 'poster-image))
      (should
       (equal
        thumbnail-call
        (list segment (qq-media--native-video-thumbnail-key media-id)))))))

(ert-deftest qq-media-native-file-media-id-reuses-content-materialization ()
  (let* ((media-id "media-22334455-6677-4889-9aab-ccddeeff0011")
         (segment `((type . "file")
                    (data . ((media_id . ,media-id)
                             (file_name . "photo.png")
                             (file_size . "12345")
                             (file_kind . "image")))))
         (qq-remote-media--media (make-hash-table :test #'equal))
         fetch-call resolved)
    (puthash media-id
             `((media_id . ,media-id)
               (kind . "file")
               (content . ((phase . "available")
                           (bytes_done . "0")
                           (expected_size . "12345"))))
             qq-remote-media--media)
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-server-ready-p) (lambda () t))
              ((symbol-function 'qq-rpc-method-available-p) (lambda (_method) t))
              ((symbol-function 'qq-media--fetch-native-file-resource)
               (lambda (called-segment key callback _errback)
                 (setq fetch-call (list called-segment key))
                 (funcall callback '((file . "/tmp/photo.png")))))
              ((symbol-function 'qq-api-call)
               (lambda (&rest _arguments)
                 (ert-fail "native media_id must not enter the v1 OneBot resolver"))))
      (let ((capabilities (qq-media-segment-capabilities segment)))
        (should (plist-get capabilities :open))
        (should (plist-get capabilities :download))
        (should (equal (plist-get capabilities :status) "Remote image")))
      (qq-media--fetch-segment-resource
       segment (lambda (resource) (setq resolved resource)) #'ignore)
      (should (equal resolved '((file . "/tmp/photo.png"))))
      (should
       (equal fetch-call
              (list segment (qq-media--native-file-key media-id)))))))

(ert-deftest qq-media-group-file-reuses-the-opaque-file-materializer ()
  (let* ((media-id "media-33445566-7788-499a-8bbc-ddeeff001122")
         (segment `((type . "group_file")
                    (data . ((media_id . ,media-id)
                             (file_name . "archive.zip")
                             (file_size . "4814589")))))
         (qq-remote-media--media (make-hash-table :test #'equal))
         fetched)
    (puthash media-id
             `((media_id . ,media-id)
               (kind . "file")
               (content . ((phase . "available")
                           (bytes_done . "0")
                           (expected_size . "4814589"))))
             qq-remote-media--media)
    (cl-letf (((symbol-function 'qq-runtime-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-server-ready-p) (lambda () t))
              ((symbol-function 'qq-rpc-method-available-p) (lambda (_method) t))
              ((symbol-function 'qq-media--fetch-native-file-resource)
               (lambda (called-segment key callback _errback)
                 (setq fetched (list called-segment key))
                 (funcall callback '((file . "/tmp/archive.zip"))))))
      (should (equal (qq-media--native-file-media-id segment) media-id))
      (should (plist-get (qq-media-segment-capabilities segment) :open))
      (qq-media--fetch-segment-resource segment #'ignore #'ignore)
      (should
       (equal fetched
              (list segment (qq-media--native-file-key media-id)))))))

(ert-deftest qq-media-terminal-native-file-preview-never-restarts-from-redisplay ()
  (qq-media-test-with-reset
   (let* ((media-id "media-33445566-7788-499a-8bbc-ddeeff001122")
          (segment `((type . "file")
                     (data . ((media_id . ,media-id)
                              (file_name . "received.png")
                              (file_kind . "image")))))
          (qq-remote-media--media (make-hash-table :test #'equal))
          (calls 0))
     (puthash media-id
              `((media_id . ,media-id)
                (kind . "file")
                (content
                 . ((phase . "failed")
                    (error . ((code . "media_download_rejected")
                              (message . "terminal download failure"))))))
              qq-remote-media--media)
     (cl-letf (((symbol-function 'qq-media--fetch-native-file-resource)
                (lambda (&rest _arguments)
                  (cl-incf calls)))
               ((symbol-function 'qq-media--preview-image-from-file)
                (lambda (&rest _arguments) 'unexpected-image)))
       (should-not (qq-media-segment-preview-image segment))
       (should-not (qq-media-segment-preview-image segment))
       (should (= calls 0))))))

(ert-deftest qq-media-failed-automatic-preview-attempt-is-single-shot ()
  (qq-media-test-with-reset
   (let* ((media-id "media-44556677-8899-4aab-9ccd-eeff00112233")
          (segment `((type . "file")
                     (data . ((media_id . ,media-id)
                              (file_name . "received.png")
                              (file_kind . "image")))))
          (qq-remote-media--media (make-hash-table :test #'equal))
          (calls 0))
     (puthash media-id
              `((media_id . ,media-id)
                (kind . "file")
                (content . ((phase . "available"))))
              qq-remote-media--media)
     (cl-letf (((symbol-function 'qq-media--fetch-native-file-resource)
                (lambda (_segment _key _done error)
                  (cl-incf calls)
                  (funcall error nil "scripted terminal failure"))))
       (should-not (qq-media-segment-preview-image segment))
       (should-not (qq-media-segment-preview-image segment))
       (should (= calls 1)))

     ;; An explicit open/download operation may still succeed and populate the
     ;; persistent cache.  Rendering that cached result does not schedule a
     ;; second native request.
     (let* ((key (qq-media-segment-preview-key segment))
            (file (make-temp-file "qq-native-preview" nil ".png")))
       (unwind-protect
           (progn
             (qq-media--cache-resource key `((file . ,file)))
             (cl-letf (((symbol-function 'qq-media--preview-image-from-file)
                        (lambda (path _spec) (list 'preview path))))
               (should (equal (qq-media-segment-preview-image segment)
                              (list 'preview file)))
               (should (= calls 1))))
         (when (file-exists-p file)
           (delete-file file)))))))

(ert-deftest qq-media-native-record-second-click-cancels-preparation ()
  (let* ((media-id "media-10213243-5465-7687-98a9-bacbdcedfe0f")
         (segment `((type . "record")
                    (data . ((duration_seconds . 8)
                             (media_id . ,media-id)))))
         (qq-media--native-record-playbacks (make-hash-table :test #'equal))
         (qq-media--native-record-current-id nil)
         (account-id "10001")
         operation prepared-id)
    (cl-letf (((symbol-function 'qq-account-current-id)
               (lambda () account-id))
              ((symbol-function 'qq-media-native-record-playback-available-p)
               (lambda () t))
              ((symbol-function 'qq-remote-media-prepare-record-playback)
               (lambda (called-media-id _callback &optional _errback)
                 (setq prepared-id called-media-id
                       operation
                       (qq-remote-media-operation-create
                        :active-p t
                        :media-id called-media-id
                        :account-id account-id))))
              ((symbol-function 'qq-media--notify-native-record-state)
               #'ignore)
              ((symbol-function 'message) #'ignore))
      (qq-media-play-native-record segment)
      (should (equal prepared-id media-id))
      (should (equal qq-media--native-record-current-id media-id))
      (should (eq (plist-get (qq-media-native-record-playback-state media-id)
                             :status)
                  'preparing))
      (should (qq-remote-media-operation-active-p operation))
      (qq-media-play-native-record segment)
      (should-not (qq-remote-media-operation-active-p operation))
      (should-not qq-media--native-record-current-id)
      (should (eq (plist-get (qq-media-native-record-playback-state media-id)
                             :status)
                  'stopped)))))

(ert-deftest qq-media-segment-open-routes-native-record-to-player ()
  (let* ((media-id "media-fedcba98-7654-3210-fedc-ba9876543210")
         (segment `((type . "record")
                    (data . ((duration_seconds . 23)
                             (media_id . ,media-id)))))
         (owner (list 'exact-app-generation))
         called-segment called-owner)
    (cl-letf (((symbol-function 'qq-media-play-native-record)
               (lambda (received &rest keys)
                 (setq called-segment received
                       called-owner (plist-get keys :owner))
                 'preparing)))
      (should (eq (qq-media-segment-open segment :owner owner) 'preparing))
      (should (eq called-segment segment))
      (should (eq called-owner owner)))))

(ert-deftest qq-media-native-record-click-toggles-appkit-player-session ()
  (let* ((media-id "media-22334455-6677-8899-aabb-ccddeeff0011")
         (segment `((type . "record")
                    (data . ((duration_seconds . 15)
                             (media_id . ,media-id)))))
         (session
          (appkit-media-player-session--create
           :status 'playing :played-seconds 2.0))
         (qq-media--native-record-playbacks
          (make-hash-table :test #'equal))
         (qq-media--native-record-current-id media-id)
         toggles)
    (puthash media-id (list :status 'playing :session session)
             qq-media--native-record-playbacks)
    (cl-letf (((symbol-function 'appkit-media-player-toggle)
               (lambda (current)
                 (push current toggles)
                 (setf (appkit-media-player-session-status current)
                       (if (eq (appkit-media-player-session-status current)
                               'playing)
                           'paused
                         'playing))
                 current)))
      (qq-media-play-native-record segment)
      (should (eq (plist-get
                   (qq-media-native-record-playback-state media-id)
                   :status)
                  'paused))
      (qq-media-play-native-record segment)
      (should (eq (plist-get
                   (qq-media-native-record-playback-state media-id)
                   :status)
                  'playing))
      (should (equal (list session session) toggles)))))

(ert-deftest qq-media-native-record-preparation-follows-exact-app-owner ()
  (let* ((media-id "media-33445566-7788-99aa-bbcc-ddeeff001122")
         (segment `((type . "record")
                    (data . ((duration_seconds . 11)
                             (media_id . ,media-id)))))
         (owner (appkit-app-start 'qq :id 'record-owner :shutdown #'ignore))
         (qq-media--native-record-playbacks (make-hash-table :test #'equal))
         (qq-media--native-record-current-id nil)
         (account-id "10001")
         operation)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-account-current-id)
                   (lambda () account-id))
                  ((symbol-function 'qq-media-native-record-playback-available-p)
                   (lambda () t))
                  ((symbol-function 'qq-remote-media-prepare-record-playback)
                   (lambda (called-media-id _callback &optional _errback)
                     (setq operation
                           (qq-remote-media-operation-create
                            :active-p t
                            :media-id called-media-id
                            :account-id account-id))))
                  ((symbol-function 'qq-media--notify-native-record-state)
                   #'ignore))
          (qq-media-play-native-record segment :owner owner)
          (let ((handle
                 (plist-get (gethash media-id qq-media--native-record-playbacks)
                            :owner-handle)))
            (should (appkit-handle-alive-p handle)))
          (appkit-app-close owner)
          (should-not (qq-remote-media-operation-active-p operation))
          (should-not qq-media--native-record-current-id)
          (should (eq (plist-get (qq-media-native-record-playback-state media-id)
                                 :status)
                      'stopped)))
      (when (appkit-app-live-p owner)
        (appkit-app-close owner)))))

(ert-deftest qq-media-native-record-appkit-exit-revokes-local-access ()
  (let* ((media-id "media-11223344-5566-7788-99aa-bbccddeeff00")
         (file (make-temp-file "qq-record-player" nil ".wav"))
         (access-id "access-11223344-5566-7788-99aa-bbccddeeff00")
         (appkit-media-audio-player-command '("sh" "-c" "exit 0"))
         (qq-media--native-record-playbacks
          (make-hash-table :test #'equal))
         (qq-media--native-record-current-id media-id)
         closed session)
    (puthash media-id
             '(:status preparing
               :account-id "10001"
               :duration-seconds 1)
             qq-media--native-record-playbacks)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-account-get)
                   (lambda (_account-id) t))
                  ((symbol-function 'qq-media--close-local-access)
                   (lambda (called-access-id)
                     (push called-access-id closed)))
                  ((symbol-function 'qq-media--notify-native-record-state)
                   #'ignore))
          (setq session
                (qq-media--start-native-record-player
                 media-id
                 `((access . ((access_id . ,access-id)
                              (path . ,file))))))
          (let ((process
                 (appkit-media-player-session-process session)))
            (while (process-live-p process)
              (accept-process-output process 0.1))
            (accept-process-output process 0.1))
          (let ((public-state
                 (qq-media-native-record-playback-state media-id))
                (private-state
                 (gethash media-id qq-media--native-record-playbacks)))
            (should (eq (plist-get public-state :status) 'finished))
            (dolist (key '(:session :operation :access-id :path))
              (should-not (plist-member public-state key)))
            (should (appkit-media-player-session-p
                     (plist-get private-state :session)))
            (should-not (plist-get private-state :access-id)))
          (should (equal closed (list access-id)))
          (should-not qq-media--native-record-current-id))
      (when (and (appkit-media-player-session-p session)
                 (not (appkit-media-player-session-finalized-p session)))
        (appkit-media-player-stop session))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'qq-media-test)

;;; qq-media-test.el ends here
