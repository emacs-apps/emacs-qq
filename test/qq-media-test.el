;;; qq-media-test.el --- Tests for qq-media -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-media)
(require 'qq-runtime)

(defun qq-media-test-drain (surface)
  "Consume finite queued Effect settlements for SURFACE."
  (let ((loop (appkit-surface-loop surface)) (passes 0))
    (while (and (eq (appkit-loop-status loop) 'running)
                (> (appkit-loop-pending-count loop) 0))
      (when (> (cl-incf passes) 32) (ert-fail "Media Surface did not settle"))
      (appkit-loop-run-pass loop))))

(defmacro qq-media-test-with-surface (&rest body)
  "Run BODY with an isolated account-owned media Surface."
  (declare (indent 0) (debug t))
  `(let* ((qq-runtime--accounts (make-hash-table :test #'equal))
          (qq-state--partitions (make-hash-table :test #'equal))
          (qq-state--active-account-id nil)
          (qq-media-cache-update-hook nil)
          (qq-media--resource-cache (make-hash-table :test #'equal))
          (qq-media--consumers (make-hash-table :test #'eq))
          (surface (qq-runtime-open-account-surface
                    :account-id "media-test" :id 'media :mode #'fundamental-mode
                    :render-function #'ignore))
          (buffer (appkit-surface-buffer surface)))
     (unwind-protect (progn ,@body)
       (qq-runtime-stop-account "media-test")
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest qq-media-acquisition-replacement-rejects-late-presentation ()
  (qq-media-test-with-surface
    (let (callbacks presented)
      (cl-letf (((symbol-function 'qq-media--acquire-source)
                 (lambda (_source resolve _reject &optional _owner)
                   (push resolve callbacks)))
                ((symbol-function 'qq-media--presentation-effect)
                 (lambda (_input source)
                   (appkit-effect-create
                    :key 'qq-media-open :input source
                    :start (lambda (_context resource _observe resolve _reject)
                             (push (alist-get 'file resource) presented)
                             (funcall resolve nil))
                    :success (lambda (_input _result) '(qq-media closed))
                    :failure #'qq-media--effect-failed
                    :cancellation-requirement 'logical))))
        (qq-media-open-resource '((url . "https://example.invalid/old.png"))
                                'image "image:old" :owner surface)
        (qq-media-test-drain surface)
        (let ((old (car callbacks)))
          (qq-media-open-resource '((url . "https://example.invalid/current.png"))
                                  'image "image:current" :owner surface)
          (qq-media-test-drain surface)
          (funcall old '((file . "/gateway/old.png")))
          (qq-media-test-drain surface)
          (should-not presented))
        (funcall (car callbacks) '((file . "/gateway/current.png")))
        (should-not presented)
        (qq-media-test-drain surface)
        (should (equal presented '("/gateway/current.png")))
        ;; The presentation immediately closes; its descriptors have no owner.
        (should-not (gethash '("media-test" . "image:current")
                             qq-media--resource-cache))
        (should-not (gethash '("media-test" . "image:old")
                             qq-media--resource-cache))))))

(ert-deftest qq-media-resolution-cannot-present-in-replacement-surface ()
  (qq-media-test-with-surface
    (let ((segment '((type . "video")
                     (data . ((url . "https://example.invalid/video.mp4")))))
          resolved presented replacement)
      (unwind-protect
          (cl-letf (((symbol-function 'qq-media-resolve-segment-resource)
                     (lambda (_segment resolve &optional _reject _streamable)
                       (setq resolved resolve) nil))
                    ((symbol-function 'appkit-media-video-presentation-start)
                     (lambda (&rest _) (setq presented t))))
            (qq-media-segment-open segment :owner surface)
            (appkit-surface-stop surface)
            (setq replacement
                  (qq-runtime-open-account-surface
                   :account-id "media-test"
                   :id 'media
                   :mode #'fundamental-mode
                   :render-function #'ignore))
            (funcall resolved '((url . "https://example.invalid/video.mp4")))
            (qq-media-test-drain replacement)
            (should-not presented)
            (should-not (plist-get (appkit-surface-model replacement) :media)))
        (when replacement
          (kill-buffer (appkit-surface-buffer replacement)))))))


(defmacro qq-media-test-with-reset (&rest body)
  "Run BODY with isolated media ownership and presentation caches."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--context-account-id "media-test")
         (qq-media--consumer (list 'test-owner))
         (qq-media--consumers (make-hash-table :test #'eq))
         (qq-media--generation 0)
         (qq-media--resource-cache (make-hash-table :test #'equal))
         (qq-media--image-cache (make-hash-table :test #'equal))
         (qq-media--one-line-image-cache (make-hash-table :test #'equal))
         (qq-media--preview-missing-cache (make-hash-table :test #'equal))
         (qq-media--fetching-cache (make-hash-table :test #'equal))
         (qq-media--native-preview-attempts (make-hash-table :test #'equal))
         (qq-media--download-state-table (make-hash-table :test #'equal))
         (qq-media--pending-cache-updates (make-hash-table :test #'equal))
         (qq-media--cache-update-timer nil)
         (qq-media--native-record-playbacks (make-hash-table :test #'equal))
         (qq-media--system-emoji-tables (make-hash-table :test #'equal))
         (qq-media--system-emoji-requests (make-hash-table :test #'equal))
         (qq-media--native-record-current-id nil)
         (qq-media-cache-update-hook nil))
     (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t)))
       (unwind-protect (progn ,@body)
         (qq-media-clear-cache)))))

(defmacro qq-media-test-with-grants (&rest body)
  "Run BODY against opaque independent references at the resource boundary."
  (declare (indent 0) (debug t))
  `(let ((grants (make-hash-table :test #'eq))
         acquired released)
     (cl-letf (((symbol-function 'qq-resource)
                (lambda (id)
                  `((resource_id . ,id) (phase . "ready")
                    (suggested_name . ,id))))
               ((symbol-function 'qq-resource-acquire-access)
                (lambda (id callback _errback)
                  (let* ((ref (list 'lease id))
                         (access `((path . ,(concat "/gateway/" id)))))
                    (puthash ref access grants)
                    (push ref acquired)
                    (funcall callback access ref)
                    ref)))
               ((symbol-function 'qq-resource-access-grant)
                (lambda (ref) (gethash ref grants)))
               ((symbol-function 'qq-resource-access-live-p)
                (lambda (ref) (and (gethash ref grants) t)))
               ((symbol-function 'qq-resource-access-release)
                (lambda (ref)
                  (when ref
                    (push ref released)
                    (remhash ref grants))))
               ((symbol-function 'qq-resource-await-ready)
                (lambda (id callback _errback)
                  (funcall callback `((resource_id . ,id) (phase . "ready"))))))
       (qq-media-test-with-reset ,@body))))

(ert-deftest qq-media-two-consumers-retain-independent-source-authority ()
  (qq-media-test-with-grants
    (let ((first (list 'first)) (second (list 'second)) one two)
      (qq-media--acquire-resource "shared.png" (lambda (source) (setq one source))
                                  #'ert-fail first)
      (qq-media--acquire-resource "shared.png" (lambda (source) (setq two source))
                                  #'ert-fail second)
      (should (equal (alist-get 'file one) (alist-get 'file two)))
      (should (= (length acquired) 2))
      (should-not (eq (car acquired) (cadr acquired)))
      (should-not (alist-get 'lease one))
      (should-not (alist-get 'lease two))
      (qq-media-release-consumer first)
      (should (= (length released) 1))
      (let ((qq-media--consumer second))
        (qq-media--cache-resource "image:shared" two)
        (should (equal (qq-media--cached-resource "image:shared") two)))
      (should (= (length acquired) 2))
      (qq-media-release-consumer first)
      (should (= (length released) 1))
      (qq-media-release-consumer second)
      (should (= (length released) 2))
      (should (= (hash-table-count grants) 0)))))

(ert-deftest qq-media-surface-close-releases-only-its-own-source ()
  (qq-media-test-with-grants
    (qq-media-test-with-surface
      (let ((other (list 'other)))
        (qq-media--acquire-resource "shared.png" #'ignore #'ert-fail surface)
        (qq-media--acquire-resource "shared.png" #'ignore #'ert-fail other)
        (appkit-surface-stop surface)
        (should-not (gethash surface qq-media--consumers))
        (should (gethash other qq-media--consumers))
        (should (= (length released) 1))
        (should (= (hash-table-count grants) 1))
        (qq-media-release-consumer other)
        (should (= (hash-table-count grants) 0))))))

(ert-deftest qq-media-buffer-close-and-mode-change-release-source ()
  (dolist (close '(kill mode))
    (qq-media-test-with-grants
      (let ((owner (generate-new-buffer " *qq-media-owner*")))
        (unwind-protect
            (progn
              (qq-media--acquire-resource "image.png" #'ignore #'ert-fail owner)
              (if (eq close 'kill)
                  (kill-buffer owner)
                (with-current-buffer owner (fundamental-mode)))
              (should-not (gethash owner qq-media--consumers))
              (should (= (length released) 1)))
          (when (buffer-live-p owner) (kill-buffer owner)))))))

(ert-deftest qq-media-final-consumer-close-retires-shared-presentations ()
  (qq-media-test-with-grants
    (let ((first (list 'first))
          (second (list 'second))
          (preview '(image :file "/gateway/shared.png"))
          (compact '(image :type canvas :data "compact-pixels"))
          flushed)
      (cl-letf (((symbol-function 'appkit-media-file-present-p) (lambda (_file) t))
                ((symbol-function 'appkit-media-one-line-preview-image-from-file)
                 (lambda (_file _width) compact))
                ((symbol-function 'image-flush)
                 (lambda (image &optional _frame) (push image flushed))))
        (dolist (owner (list first second))
          (let ((qq-media--consumer owner))
            (qq-media--acquire-resource
             "shared.png"
             (lambda (source) (qq-media--cache-resource "shared" source))
             #'ert-fail)
            (qq-media--cache-image "shared" preview)
            (should (eq (qq-media--one-line-preview-image-from-file
                         "/gateway/shared.png") compact))))
        (qq-media-release-consumer first)
        (should (= (hash-table-count grants) 1))
        (should (= (hash-table-count qq-media--resource-cache) 1))
        (should (= (hash-table-count qq-media--image-cache) 1))
        (should (= (hash-table-count qq-media--one-line-image-cache) 1))
        (should-not flushed)
        (let ((qq-media--consumer second))
          (should (eq (qq-media--cached-image "shared") preview)))
        (qq-media-release-consumer second)
        (dolist (table (list grants qq-media--resource-cache qq-media--image-cache
                             qq-media--one-line-image-cache))
          (should (= (hash-table-count table) 0)))
        (should (memq preview flushed))
        (should (memq compact flushed))))))

(ert-deftest qq-media-closed-failed-preview-consumers-do-not-accumulate ()
  (qq-media-test-with-reset
    (let ((attempts 0))
      (dotimes (index 32)
        (let ((owner (generate-new-buffer " *qq-failed-preview*")))
          (unwind-protect
              (with-current-buffer owner
                (let ((qq-media--consumer owner)
                      (key (format "failed:%d" index)))
                  (dotimes (_repeat 2)
                    (qq-media--ensure-resource-image
                     key
                     (lambda (_done fail)
                       (cl-incf attempts)
                       (funcall fail nil "unavailable"))
                     nil #'ert-fail))
                  (should (= (hash-table-count qq-media--preview-missing-cache) 1))))
            (kill-buffer owner)))
        (should (= (hash-table-count qq-media--consumers) 0))
        (should (= (hash-table-count qq-media--preview-missing-cache) 0)))
      (should (= attempts 32)))))


(ert-deftest qq-media-stale-native-completion-releases-exact-lease ()
  (dolist (boundary '(close reset))
    (qq-media-test-with-grants
      (let* ((owner qq-media--consumer)
             (lease (list 'late-lease))
             (segment '((type . "image")
                        (data . ((media_id . "media-00000000-0000-4000-8000-000000000001")))))
             callback presented)
        (puthash lease '((path . "/gateway/late.png")) grants)
        (cl-letf (((symbol-function 'qq-remote-media-prepare-part-local-access)
                   (lambda (_id _part done &optional _fail _source)
                     (setq callback done))))
          (qq-media--fetch-native-image-resource
           segment "image:late" (lambda (_) (setq presented t)) #'ert-fail)
          (if (eq boundary 'reset)
              (qq-media-clear-cache)
            (qq-media-release-consumer owner))
          ;; Reusing the same owner identity must not revive its previous work.
          (qq-media--owner-state owner)
          (funcall callback `((resource_id . "late.png")
                              (access . ((path . "/gateway/late.png")))
                              (lease . ,lease)))
          (should-not presented)
          (should-not (qq-media--cached-resource "image:late"))
          (should (= (length released) 1))
          (should (eq (car released) lease)))))))

(ert-deftest qq-media-account-selection-does-not-retarget-async-image ()
  (qq-media-test-with-grants
    (let ((owner-a (list 'a)) (owner-b (list 'b)) complete)
      (let ((qq-runtime--context-account-id "slot-a")
            (qq-media--consumer owner-a))
        (qq-media--ensure-resource-image
         "avatar:10001" (lambda (done _fail) (setq complete done)) 20
         (lambda (file _spec) (list 'image :file file))))
      (let ((qq-runtime--context-account-id "slot-b")
            (qq-media--consumer owner-b))
        (qq-media--cache-resource "avatar:10001" '((file . "/gateway/b.png")))
        (qq-media--cache-image "avatar:10001" '(image :file "/gateway/b.png"))
        (cl-letf (((symbol-function 'appkit-media-file-present-p) (lambda (_) t)))
          (funcall complete '((file . "/gateway/a.png"))))
        (should (equal (qq-media--cached-image "avatar:10001")
                       '(image :file "/gateway/b.png"))))
      (let ((qq-runtime--context-account-id "slot-a")
            (qq-media--consumer owner-a))
        (should (equal (qq-media--cached-image "avatar:10001")
                       '(image :file "/gateway/a.png")))))))

(ert-deftest qq-media-invalidated-resource-discards-every-alias-descriptor ()
  (qq-media-test-with-grants
    (let (notified)
      (qq-media--acquire-resource
       "shared.png"
       (lambda (source)
         (dolist (key '("avatar:one" "avatar:two"))
           (qq-media--cache-resource key source)
           (qq-media--cache-image key '(image :file "/gateway/shared.png"))))
       #'ert-fail)
      (qq-media--cache-resource "avatar:other" '((file . "/user/other.png")))
      (qq-media--cache-image "avatar:other" '(image :file "/user/other.png"))
      (puthash "/gateway/shared.png" '(identity . stale-thumbnail)
               qq-media--one-line-image-cache)
      (clrhash grants)
      (cl-letf (((symbol-function 'qq-media--note-cache-updated)
                 (lambda (key) (push key notified)))
                ((symbol-function 'qq-resource-acquire-access)
                 (lambda (&rest _) nil)))
        (run-hook-with-args 'qq-resource-access-invalidated-hook "shared.png" 'expired)
        (dolist (key '("avatar:one" "avatar:two"))
          (should-not (qq-media--cached-image key))
          (should-not (alist-get 'file
                                 (gethash (qq-media--scoped-key key)
                                          qq-media--resource-cache)))
          (should (member key notified)))
        (should (equal (qq-media--cached-image "avatar:other")
                       '(image :file "/user/other.png")))
        (should (= (hash-table-count qq-media--one-line-image-cache) 0))))))

(ert-deftest qq-media-file-viewer-retains-source-after-origin-closes ()
  (qq-media-test-with-grants
    (let ((origin (list 'origin))
          (viewer (generate-new-buffer " *qq-media-viewer*"))
          source resolved)
      (unwind-protect
          (cl-letf (((symbol-function 'appkit-effect-create) (lambda (&rest args) args))
                    ((symbol-function 'find-file-noselect)
                     (lambda (file &rest _)
                       (should (equal file "/gateway/document.txt"))
                       viewer))
                    ((symbol-function 'pop-to-buffer) #'ignore))
            (qq-media--acquire-resource
             "document.txt" (lambda (resource) (setq source resource)) #'ert-fail origin)
            (let ((effect (qq-media--presentation-effect '(:kind file) source)))
              (funcall (plist-get effect :start) nil source #'ignore
                       (lambda (buffer) (setq resolved buffer)) #'ert-fail))
            (should (eq resolved viewer))
            (should (buffer-local-value 'buffer-read-only viewer))
            (should (= (length acquired) 2))
            (qq-media-release-consumer origin)
            (should (= (hash-table-count grants) 1))
            (should (gethash viewer qq-media--consumers))
            (kill-buffer viewer)
            (should (= (hash-table-count grants) 0))
            (should (= (length released) 2)))
        (when (buffer-live-p viewer) (kill-buffer viewer))))))

(ert-deftest qq-media-native-thumbnail-retains-distinct-resource-identity ()
  (qq-media-test-with-grants
    (let* ((segment '((type . "video")
                     (data . ((media_id . "media-00000000-0000-4000-8000-000000000002")))))
           (content-key (qq-media--segment-resource-key segment))
           (thumbnail-key (qq-media-segment-preview-key segment))
           content thumbnail parts)
      (cl-letf (((symbol-function 'qq-remote-media-prepare-part-local-access)
                 (lambda (_id part callback &optional _errback _source)
                   (push part parts)
                   (let* ((id (if (eq part 'content) "original.mp4" "thumbnail.jpg"))
                          (lease (list 'native-lease part))
                          (access `((path . ,(concat "/gateway/" id)))))
                     (puthash lease access grants)
                     (funcall callback `((resource_id . ,id) (lease . ,lease)
                                         (access . ,access)))))))
        (qq-media--fetch-native-video-resource
         segment content-key (lambda (source) (setq content source)) #'ert-fail)
        (qq-media--fetch-native-video-thumbnail-resource
         segment thumbnail-key (lambda (source) (setq thumbnail source)) #'ert-fail)
        (should (equal parts '(thumbnail content)))
        (should-not (equal content-key thumbnail-key))
        (should (equal (alist-get 'resource-id content) "original.mp4"))
        (should (equal (alist-get 'resource-id thumbnail) "thumbnail.jpg"))
        (should (equal (qq-media--cached-resource content-key) content))
        (should (equal (qq-media--cached-resource thumbnail-key) thumbnail))
        (should (= (hash-table-count grants) 2))
        (should-not released)))))

(ert-deftest qq-media-export-holds-original-until-copy-completion ()
  (dolist (outcome '(success error cancel))
    (qq-media-test-with-grants
      (let* ((origin (list 'origin))
             (segment '((type . "image")
                        (data . ((media_id . "media-00000000-0000-4000-8000-000000000003")))))
             (preview-key (qq-media-segment-preview-key segment))
             copy-done copy-fail copied operation-owner cancel completed failed)
        (qq-media--acquire-resource
         "animation.gif"
         (lambda (source) (qq-media--cache-resource preview-key source))
         #'ert-fail origin)
        (cl-letf (((symbol-function 'qq-media--fetch-segment-resource)
                   (lambda (_segment done &optional _fail)
                     (setq operation-owner qq-media--consumer)
                     (funcall done '((resource-id . "original.png")))))
                  ((symbol-function 'appkit-media-copy-or-download-resource-async)
                   (lambda (resource _target done fail &rest _)
                     (setq copied resource copy-done done copy-fail fail)
                     nil)))
          (setq cancel
                (qq-media--export-segment
                 segment "/user/export.png"
                 (lambda (file) (setq completed file))
                 (lambda (reason) (setq failed reason))))
          (should-not (eq operation-owner origin))
          (should (equal (alist-get 'file copied) "/gateway/original.png"))
          (should-not (alist-get 'url copied))
          (qq-media-release-consumer origin)
          (should (gethash operation-owner qq-media--consumers))
          (should (= (hash-table-count grants) 1))
          (pcase outcome
            ('success (funcall copy-done "/user/export.png"))
            ('error (funcall copy-fail "copy failed"))
            ('cancel (funcall cancel)))
          (should-not (gethash operation-owner qq-media--consumers))
          (should (= (hash-table-count grants) 0))
          (if (eq outcome 'success)
              (should (equal completed "/user/export.png"))
            (should failed))
          ;; Late copy settlement cannot resurrect an already terminal export.
          (funcall copy-done "/user/export.png")
          (should (= (length released) 2)))))))

(ert-deftest qq-media-owner-close-does-not-suppress-another-pending-preview ()
  (qq-media-test-with-grants
    (cl-letf (((symbol-function 'appkit-media-file-present-p)
               (lambda (file) (equal file "/gateway/shared.png"))))
      (let ((first (list 'first)) (second (list 'second)) callbacks)
        (dolist (owner (list first second))
          (let ((qq-media--consumer owner))
            (qq-media--ensure-resource-image
             "shared" (lambda (done _fail) (push done callbacks)) 20
             (lambda (file _spec) (list 'image :file file)))))
        (should (= (length callbacks) 2))
        (qq-media-release-consumer first)
        (funcall (cadr callbacks) '((resource-id . "shared.png")))
        (should-not acquired)
        (funcall (car callbacks) '((resource-id . "shared.png")))
        (let ((qq-media--consumer second))
          (should (equal (qq-media--cached-image "shared")
                         '(image :file "/gateway/shared.png"))))
        (should (= (length acquired) 1))))))

(ert-deftest qq-media-unavailable-grant-settles-without-reacquiring ()
  (qq-media-test-with-grants
    (let ((fetches 0) (file-checks 0) presented)
      (cl-letf (((symbol-function 'appkit-media-file-present-p)
                 (lambda (file)
                   (when (and file (> (cl-incf file-checks) 1))
                     ;; Bound a regression before it can grow resource metadata.
                     (error "Unavailable grant re-entered acquisition"))
                   nil)))
        (dotimes (_ 3)
          (qq-media--ensure-resource-image
           "unavailable"
           (lambda (done _fail)
             (cl-incf fetches)
             (funcall done '((resource-id . "unavailable.png"))))
           20
           (lambda (&rest _args) (setq presented t) '(image))))
        (should (= fetches 1))
        (should (= file-checks 1))
        (should (= (length acquired) 1))
        (should-not presented)
        (should-not (gethash (qq-media--fetch-key "unavailable")
                             qq-media--fetching-cache))
        (should (eq (gethash (qq-media--scoped-key "unavailable")
                             qq-media--preview-missing-cache)
                    t))))))

(ert-deftest qq-media-avatar-url-acquires-gateway-grant-not-stale-local-file ()
  (qq-media-test-with-grants
    (let ((resource '((file . "/stale/avatar.png")
                      (url . "https://example.invalid/current-avatar.png")))
          requested)
      (cl-letf (((symbol-function 'qq-resource-acquire-url)
                 (lambda (url _name done _fail)
                   (setq requested url)
                   (funcall done '((resource_id . "avatar.png")))))
                ((symbol-function 'appkit-media-file-present-p)
                 (lambda (file) (equal file "/gateway/avatar.png"))))
        (qq-media--ensure-resource-image
         "avatar:10001" (lambda (done _fail) (funcall done resource)) 20
         (lambda (file _spec) (list 'image :file file)))
        (should (equal requested "https://example.invalid/current-avatar.png"))
        (should (equal (qq-media--cached-image "avatar:10001")
                       '(image :file "/gateway/avatar.png")))
        (should (equal (alist-get 'file resource) "/stale/avatar.png"))
        (should (= (length acquired) 1))))))

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

(ert-deftest qq-media-avatar-square-fallback-remains-static ()
  (cl-letf (((symbol-function 'appkit-media-circular-image-from-file)
             (lambda (&rest _arguments) nil))
            ((symbol-function 'qq-media--image-from-file)
             (lambda (_file pixel-size)
               (appkit-media-image-with-resource
                (list 'image :type 'png :width pixel-size :height pixel-size)
                '((url . "https://example.invalid/animated-avatar"))
                :animated t)))
            ((symbol-function 'appkit-media-image-object-valid-p)
             (lambda (_) t)))
    (let ((image (qq-media--avatar-image-from-file "/tmp/avatar.jpg" 20)))
      (should-not (appkit-media-animated-image-p image))
      (with-temp-buffer
        (insert (appkit-media-image-display-string image "[avatar]"))
        (should (get-text-property 1 'display))
        (should-not (appkit-media-inline-mount 1 (point-max)))))))


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

(ert-deftest qq-media-custom-face-preview-materializes-durable-id ()
  (qq-media-test-with-grants
    (let ((face '((favorite_emoji_id . "favorite-a")
                  (url . "https://example.invalid/obsolete.png")))
          materialized)
      (cl-letf (((symbol-function 'qq-favorite-emoji-materialize)
                 (lambda (id callback _errback)
                   (setq materialized id)
                   (funcall callback '((resource . ((resource_id . "favorite.png")))))))
                ((symbol-function 'appkit-media-file-present-p)
                 (lambda (file) (equal file "/gateway/favorite.png")))
                ((symbol-function 'qq-media--image-from-file)
                 (lambda (file _height) (list 'image :file file)))
                ((symbol-function 'qq-resource-acquire-url)
                 (lambda (&rest _) (ert-fail "favorite catalog URL is not authority"))))
        (qq-media-custom-face-image face)
        (should (equal materialized "favorite-a"))
        (should (equal (qq-media-custom-face-image face)
                       '(image :file "/gateway/favorite.png")))))))

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

(ert-deftest qq-media-face-resource-shares-pending-preview-and-inline-load ()
  (qq-media-test-with-reset
   (let ((file (make-temp-file "qq-face-source-" nil ".json"))
         (requests 0)
         complete)
     (unwind-protect
         (progn
           (with-temp-file file
             (insert "{\"v\":\"5.12.1\",\"fr\":60,\"ip\":0,\"op\":2,"
                     "\"w\":32,\"h\":32,\"layers\":[]}"))
           (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                     ((symbol-function 'qq-rpc-method-available-p)
                      (lambda (method) (equal method "system_emoji.resource")))
                     ((symbol-function 'qq-media--fetch-face-resource)
                      (lambda (_id callback _errback)
                        (cl-incf requests)
                        (setq complete callback))))
             (should-not (qq-media-face-image "493"))
             (should-not (qq-media-face-image "493" t))
             (should (= requests 1))
             (funcall complete
                      `((file . ,file)
                        (mime-type . "application/lottie+json")
                        (emoji_id . "493")
                        (animated . t)))
             (should (equal (alist-get 'file (qq-media-face-resource "493")) file))
             (should
              (eq (appkit-media-resource-source-format
                   (qq-media--appkit-resource (qq-media-face-resource "493")))
                  'lottie))
             (should (= requests 1))))
       (delete-file file)))))

(ert-deftest qq-media-face-resource-reset-rejects-stale-load-and-failure-is-soft ()
  (qq-media-test-with-reset
   (let ((file (make-temp-file "qq-face-source-" nil ".png"))
         (requests 0)
         complete reject)
     (unwind-protect
         (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                   ((symbol-function 'qq-rpc-method-available-p)
                    (lambda (method) (equal method "system_emoji.resource")))
                   ((symbol-function 'qq-media--fetch-face-resource)
                    (lambda (_id callback errback)
                      (cl-incf requests)
                      (setq complete callback reject errback))))
           (with-temp-file file (insert "stale resource"))
           (should-not (qq-media-face-resource "178"))
           (let ((stale complete))
             (qq-media-clear-cache)
             (should-not (qq-media-face-resource "178"))
             (funcall stale `((file . ,file)))
             (should-not (qq-media--cached-resource "face:178"))
             (should (qq-media--resource-fetching-p "face:178")))
           (funcall reject '((code . "system_emoji_not_found")) "missing")
           (should-not (qq-media-face-resource "178"))
           (should (equal (qq-media-face-display-string "178" "face fallback")
                          "face fallback"))
           (should (= requests 2)))
       (delete-file file)))))

(ert-deftest qq-media-segment-inline-resource-keeps-posters-out-of-video ()
  (qq-media-test-with-reset
   (let* ((image (make-temp-file "qq-content" nil ".png"))
          (video (make-temp-file "qq-content" nil ".mp4"))
          (poster (make-temp-file "qq-poster" nil ".gif"))
          (image-segment '((type . "image")
                           (data . ((media_id . "media-aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")))))
          (video-segment '((type . "video")
                           (data . ((media_id . "media-bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb")
                                    (thumb . "https://example.test/poster.jpg")))))
          (preview-key (qq-media-segment-preview-key image-segment))
          (poster-key (qq-media-segment-preview-key video-segment)))
     (unwind-protect
         (progn
           (qq-media--cache-resource preview-key `((file . ,image)))
           (qq-media--cache-resource poster-key `((file . ,poster)))
           (should (equal (alist-get
                           'file (qq-media-segment-inline-resource image-segment))
                          image))
           (should-not (qq-media-segment-inline-resource video-segment))
           (qq-media--cache-resource
            (qq-media--segment-resource-key video-segment)
            `((file . ,video)))
           (should (equal (alist-get
                           'file (qq-media-segment-inline-resource video-segment))
                          video))
           (should-not
            (qq-media-segment-inline-resource
             '((type . "video")
               (data . ((url . "https://example.test/clip.mp4")
                        (remote_status . "expired"))))))
           (should-not
            (qq-media-segment-inline-resource
             '((type . "video")
               (data . ((url . "https://example.test/clip.mp4")
                        (remote_status . "available"))))))
           (should-not
            (qq-media-segment-inline-resource
             '((type . "video")
               (data . ((url . "http://example.test/clip.mp4")
                        (remote_status . "available"))))))
           (should-not
            (qq-media-segment-inline-resource
             '((type . "video")
               (data . ((url . "https://")
                        (remote_status . "available")))))))
       (dolist (file (list image video poster))
         (when (file-exists-p file) (delete-file file)))))))

(ert-deftest qq-media-large-face-missing-identity-never-downgrades ()
  (let ((qq-runtime--context-account-id "media-test")
        (qq-media--system-emoji-tables (make-hash-table :test #'equal))
        (table (make-hash-table :test #'equal)))
    (puthash "5" '((id . "5") (emoji_type . 1)) table)
    (puthash "media-test" table qq-media--system-emoji-tables)
    (should-error (qq-media-large-face-to-segment "5") :type 'user-error)))

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

(ert-deftest qq-media-video-poster-never-becomes-playable-local-source ()
  (qq-media-test-with-reset
    (let* ((poster (make-temp-file "qq-video-poster" nil ".jpg"))
           (segment '((type . "video")
                      (data . ((file . "video-handle")
                               (remote_status . "unavailable")))))
           (preview-key (qq-media-segment-preview-key segment)))
      (unwind-protect
          (progn
            (qq-media--cache-resource preview-key `((file . ,poster)))
            (should-not (qq-media-segment-local-file segment))
            (let ((capabilities (qq-media-segment-capabilities segment)))
              (should-not (plist-get capabilities :local-file))
              (should-not (plist-get capabilities :open))))
        (delete-file poster)))))

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
           (puthash (qq-media--scoped-key preview-key) t qq-media--preview-missing-cache)
           (qq-media--put-segment-download-state
            segment `(:status downloaded :path ,file))
           (should-not (gethash (qq-media--scoped-key preview-key)
                                qq-media--preview-missing-cache)))
       (delete-file file)))))

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
       (should failure)))))

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
           '(("unavailable" "Unavailable")
             ("unresolved" "Unresolved")))
    (pcase-let ((`(,wire ,status) case))
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
        (should failure)))))

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

(ert-deftest qq-media-available-video-acquires-wire-url-through-gateway ()
  (qq-media-test-with-grants
    (let* ((url "https://example.com/movie.mp4")
           (segment `((type . "video")
                      (data . ((file . "must-not-go-to-get-file")
                               (url . ,url)
                               (remote_status . "available")))))
           requested resolved)
      (cl-letf (((symbol-function 'qq-resource-acquire-url)
                 (lambda (source _name callback _errback)
                   (setq requested source)
                   (funcall callback '((resource_id . "movie.mp4")))))
                ((symbol-function 'qq-api-call)
                 (lambda (&rest _) (ert-fail "wire URL must not use OneBot")))
                ((symbol-function 'appkit-media-file-present-p)
                 (lambda (file) (equal file "/gateway/movie.mp4"))))
        (qq-media-resolve-segment-resource
         segment (lambda (resource) (setq resolved resource)))
        (should (equal requested url))
        (should (equal (alist-get 'file resolved) "/gateway/movie.mp4"))
        (should (equal (alist-get 'resource-id resolved) "movie.mp4"))
        (let ((playback (qq-media-segment-inline-resource segment)))
          (should (equal (alist-get 'file playback) "/gateway/movie.mp4"))
          (should-not (alist-get 'url playback)))))))

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
                           '((file . "/gateway/report.pdf")))))
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

(ert-deftest qq-media-segment-download-retains-cancelable-operation ()
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
                           '((file . "/gateway/report.pdf")))))
               ((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda (&rest _arguments) 'download-handle))
               ((symbol-function 'message) #'ignore))
       (qq-media-segment-start-download segment)
       (let ((state (qq-media-segment-download-state segment)))
         (should (eq 'downloading (plist-get state :status)))
         (should (functionp (plist-get state :cancel)))
         (qq-media-segment-cancel-transfer segment)
         (should (eq 'error
                     (plist-get (qq-media-segment-download-state segment) :status))))))))

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
              :status 'paused
              :played-seconds 3.0))
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
        ;; No operation belongs to this consumer: do not cancel shared work.
        (should-not canceled)))))

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
               (lambda (image _client) image))
              ((symbol-function 'qq-server-ready-p) (lambda () t)))
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

(ert-deftest qq-media-native-video-open-streams-instead-of-materializing ()
  (qq-media-test-with-reset
   (let* ((media-id "media-11223344-5566-7788-99aa-bbccddeeff01")
          (stream-id "stream-0123456789abcdef0123456789abcdef")
          (url (concat "http://127.0.0.1:41234/media/" stream-id))
          (segment `((type . "video") (data . ((media_id . ,media-id)))))
          (file (make-temp-file "qq-stream-video" nil ".mp4"))
          fetched opened streamed file-resolved)
     (unwind-protect
         (cl-letf (((symbol-function 'qq-remote-media-streaming-available-p)
                    (lambda () t))
                   ((symbol-function 'qq-remote-media-open-stream)
                    (lambda (id callback &optional _errback _source)
                      (setq opened id)
                      (funcall callback `((stream_id . ,stream-id)
                                          (media_id . ,id)
                                          (url . ,url)
                                          (expires_at . 1)))))
                   ((symbol-function 'qq-media--fetch-native-video-resource)
                    (lambda (_segment _key callback _errback)
                      (setq fetched t)
                      (funcall callback `((file . ,file))))))
           (qq-media-resolve-segment-resource
            segment (lambda (resource) (setq streamed resource)) #'ignore t)
           (should (equal opened media-id))
           (should-not fetched)
           (should (equal (alist-get 'url streamed) url))
           (should (equal (alist-get 'stream-id streamed) stream-id))
           ;; Loopback stream URLs are neither upgraded nor cached.
           (should (equal (alist-get 'url (qq-media--appkit-resource streamed))
                          url))
           (should-not (qq-media--cached-resource
                        (qq-media--native-video-key media-id)))
           ;; File consumers (inline hosts, save, previews) still materialize.
           (qq-media-resolve-segment-resource
            segment (lambda (resource) (setq file-resolved resource)) #'ignore)
           (should fetched)
           (should (equal (alist-get 'file file-resolved) file)))
       (delete-file file)))))

(ert-deftest qq-media-native-video-stream-failure-falls-back-to-materializing ()
  (qq-media-test-with-reset
   (let* ((media-id "media-11223344-5566-7788-99aa-bbccddeeff02")
          (segment `((type . "video") (data . ((media_id . ,media-id)))))
          (file (make-temp-file "qq-stream-video" nil ".mp4"))
          resolved)
     (unwind-protect
         (cl-letf (((symbol-function 'qq-remote-media-streaming-available-p)
                    (lambda () t))
                   ((symbol-function 'qq-remote-media-open-stream)
                    (lambda (_id _callback &optional errback _source)
                      (funcall errback nil "media_stream_unsupported")))
                   ((symbol-function 'qq-media--fetch-native-video-resource)
                    (lambda (_segment _key callback _errback)
                      (funcall callback `((file . ,file))))))
           (qq-media-resolve-segment-resource
            segment (lambda (resource) (setq resolved resource)) #'ignore t)
           (should (equal (alist-get 'file resolved) file)))
       (delete-file file)))))

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
     ;; owned source.  Rendering that cached result does not schedule a
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
               (lambda (called-media-id _callback &optional _errback _source)
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

(ert-deftest qq-media-native-record-click-toggles-appkit-player-session ()
  (let* ((media-id "media-22334455-6677-8899-aabb-ccddeeff0011")
         (segment `((type . "record")
                    (data . ((duration_seconds . 15)
                             (media_id . ,media-id)))))
         (session
          (appkit-media-player-session--create
           :status 'playing
           :played-seconds 2.0))
         (qq-media--native-record-playbacks
          (make-hash-table :test #'equal))
         (qq-media--native-record-current-id media-id)
         (lease (list 'paused-record-lease))
         toggles released)
    (puthash media-id (list :status 'playing :session session :lease lease)
             qq-media--native-record-playbacks)
    (cl-letf (((symbol-function 'qq-resource-access-release)
               (lambda (ref) (push ref released)))
              ((symbol-function 'qq-resource-access-grant)
               (lambda (ref)
                 (and (eq ref lease) '((path . "/gateway/record.ogg")))))
              ((symbol-function 'appkit-media-player-toggle)
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
      (should-not released)
      (should (eq (plist-get (gethash media-id qq-media--native-record-playbacks)
                            :lease)
                  lease))
      (qq-media-play-native-record segment)
      (should (eq (plist-get
                   (qq-media-native-record-playback-state media-id)
                   :status)
                  'playing))
      (should-not released)
      (should (equal (list session session) toggles)))))

(ert-deftest qq-media-native-record-preparation-follows-exact-app-owner ()
  (let* ((media-id "media-33445566-7788-99aa-bbcc-ddeeff001122")
         (segment `((type . "record")
                    (data . ((duration_seconds . 11)
                             (media_id . ,media-id)))))
         (owner (appkit-app-start qq-runtime--account-type :identity 'record-owner))
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
                   (lambda (called-media-id _callback &optional _errback _source)
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
         (lease (list 'record-lease))
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
                  ((symbol-function 'qq-resource-access-release)
                   (lambda (ref)
                     (when ref (push ref closed))))
                  ((symbol-function 'qq-media--notify-native-record-state)
                   #'ignore))
          (setq session
                (qq-media--start-native-record-player
                 media-id
                 `((lease . ,lease)
                   (access . ((path . ,file))))))
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
            (dolist (key '(:session :operation :lease :path))
              (should-not (plist-member public-state key)))
            (should (appkit-media-player-session-p
                     (plist-get private-state :session)))
            (should-not (plist-get private-state :lease)))
          (should (equal closed (list lease)))
          (should-not qq-media--native-record-current-id))
      (when (and (appkit-media-player-session-p session)
                 (not (appkit-media-player-session-finalized-p session)))
        (appkit-media-player-stop session))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest qq-media-favorite-image-rejects-account-change-during-download ()
  (dolist (case '((t "slot-b" "media_account_mismatch")
                  (nil "slot-a" "account_removed")))
    (let* ((qq-runtime--context-account-id "slot-a")
           (qq-remote-media--media (make-hash-table :test #'equal))
           (qq-remote-media-changed-hook nil)
           (media-id "media-11223344-5566-7788-99aa-bbccddeeff00")
           (segment `((type . "image") (data . ((media_id . ,media-id)))))
           (media (copy-tree
                   `((media_id . ,media-id) (account_id . "slot-a")
                     (content . ((phase . "materializing"))))))
           (live t)
           failure)
      (puthash media-id media qq-remote-media--media)
      (cl-letf (((symbol-function 'qq-account-get)
                 (lambda (id) (and live (equal id "slot-a")
                                   '((account_id . "slot-a")))))
                ((symbol-function 'qq-remote-media-materialize)
                 (lambda (_id callback &optional _errback)
                   (funcall callback media)))
                ((symbol-function 'qq-favorite-emoji-add)
                 (lambda (&rest _)
                   (ert-fail "must not collect another or removed account's image"))))
        (qq-media-save-image-as-favorite
         segment (lambda (_) (ert-fail "unexpected favorite success"))
         (lambda (body _reason) (setq failure (alist-get 'code body))))
        (setq live (car case))
        (setf (alist-get 'account_id media) (cadr case)
              (alist-get 'content media)
              '((phase . "materialized")
                (resource_id . "res-11223344-5566-7788-99aa-bbccddeeff00")))
        (run-hook-with-args 'qq-remote-media-changed-hook 'changed media-id)
        (should (equal failure (nth 2 case)))
        (should-not qq-remote-media-changed-hook)))))


(provide 'qq-media-test)

;;; qq-media-test.el ends here
