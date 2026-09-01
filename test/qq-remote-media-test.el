;;; qq-remote-media-test.el --- Tests for remote media -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-remote-media)

(defconst qq-remote-media-test-id
  "media-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")

(defconst qq-remote-media-test-capabilities
  '("media.list" "media.status" "media.materialize" "media.cancel"
    "media.release"
    "resource.derive_playable_record" "resource.open_local"
    "resource.close_local"))

(cl-defun qq-remote-media-test-part
    (&key
     (phase "available")
     (bytes-done "0")
     (expected-size "128")
     (bytes-total "128")
     resource-id error)
  "Return one remote-media part fixture."
  `(,@(when expected-size `((expected_size . ,expected-size)))
    (phase . ,phase)
    (bytes_done . ,bytes-done)
    ,@(when bytes-total `((bytes_total . ,bytes-total)))
    ,@(when resource-id `((resource_id . ,resource-id)))
    ,@(when error `((error . ,error)))))

(cl-defun qq-remote-media-test-snapshot
    (&key
     (media-id qq-remote-media-test-id)
     (account-id "slot-a")
     (message-id "7348923749823749823")
     (sequence "42")
     (segment-index 0)
     (duration-seconds 17)
     (phase "available")
     (bytes-done "0")
     (created-at 1784700000)
     (updated-at 1784700000)
     (expected-size "128")
     (bytes-total "128")
     resource-id error thumbnail)
  "Return one closed native remote-media fixture."
  `((media_id . ,media-id)
    (account_id . ,account-id)
    (message_id . ,message-id)
    ,@(when sequence `((sequence . ,sequence)))
    (segment_index . ,segment-index)
    (kind . "record")
    (duration_seconds . ,duration-seconds)
    (content
     . ,(qq-remote-media-test-part
         :phase phase :bytes-done bytes-done
         :expected-size expected-size :bytes-total bytes-total
         :resource-id resource-id :error error))
    ,@(when thumbnail `((thumbnail . ,thumbnail)))
    (created_at . ,created-at)
    (updated_at . ,updated-at)))

(cl-defun qq-remote-media-test-resource
    (&key
     (resource-id "res-native-silk")
     (phase "ready")
     (suggested-name "voice.silk")
     (size "128")
     (media-type "audio/x-tencent-silk")
     (created-at 1784700000)
     (updated-at 1784700001))
  "Return one closed staged-resource fixture."
  `((resource_id . ,resource-id)
    (phase . ,phase)
    (suggested_name . ,suggested-name)
    (size . ,size)
    (media_type . ,(and (equal phase "ready") media-type))
    (digests
     . ,(and (equal phase "ready")
             '((sha256 . "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
               (sha1 . "a9993e364706816aba3e25717850c26c9cd0d89d")
               (md5 . "900150983cd24fb0d6963f7d28e17f72"))))
    (created_at . ,created-at)
    (updated_at . ,updated-at)
    (expires_at . 1784786400)
    (error)))

(defmacro qq-remote-media-test-with-state (&rest body)
  "Run BODY with isolated remote-media and resource projections."
  (declare (indent 0) (debug t))
  `(let ((qq-remote-media--media (make-hash-table :test #'equal))
         (qq-remote-media--order nil)
         (qq-remote-media--gateway-instance-id nil)
         (qq-remote-media--refresh-owner nil)
         (qq-remote-media--resync-request-id nil)
         (qq-remote-media--playable-resources (make-hash-table :test #'equal))
         (qq-remote-media-changed-hook nil)
         (qq-remote-media-desync-hook nil)
         (qq-resource--resources (make-hash-table :test #'equal))
         (qq-resource--order nil)
         (qq-resource--refresh-owner nil)
         (qq-resource-changed-hook nil))
     ,@body))

(ert-deftest qq-remote-media-accessors-copy-projected-values ()
  (qq-remote-media-test-with-state
    (qq-remote-media--replace
     (list (qq-remote-media-test-snapshot)) 'test)
    (let* ((public (qq-remote-media qq-remote-media-test-id))
           (public-account-id (alist-get 'account_id public)))
      (aset public-account-id 0 ?Y)
      (should (equal
               (alist-get 'account_id
                          (qq-remote-media qq-remote-media-test-id))
               "slot-a")))))

(ert-deftest qq-remote-media-message-id-promotes-without-changing-identity ()
  (qq-remote-media-test-with-state
    (qq-remote-media--upsert
     (qq-remote-media-test-snapshot :message-id nil) 'history)
    (qq-remote-media--upsert
     (qq-remote-media-test-snapshot
      :message-id "7348923749823749823"
      :updated-at 1784700001)
     'live)
    (should
     (equal (alist-get 'message_id
                       (qq-remote-media qq-remote-media-test-id))
            "7348923749823749823"))
    ;; An older command response cannot erase exact evidence learned from the
    ;; live observation, even when both updates share timestamp granularity.
    (qq-remote-media--upsert
     (qq-remote-media-test-snapshot
      :message-id nil
      :phase "materializing"
      :updated-at 1784700001)
     'response)
    (let ((projected (qq-remote-media qq-remote-media-test-id)))
      (should (equal (alist-get 'message_id projected)
                     "7348923749823749823"))
      (should (equal (alist-get 'phase
                                (qq-remote-media-part projected 'content))
                     "materializing")))))

(ert-deftest qq-remote-media-rejects-stable-position-or-exact-id-conflicts ()
  (qq-remote-media-test-with-state
    (qq-remote-media--upsert (qq-remote-media-test-snapshot) 'first)
    (should-error
     (qq-remote-media--upsert
      (qq-remote-media-test-snapshot :sequence "43") 'conflict))
    (should-error
     (qq-remote-media--upsert
      (qq-remote-media-test-snapshot :message-id "7348923749823749824")
      'conflict))))

(ert-deftest qq-remote-media-materialize-starts-background-state-machine ()
  (qq-remote-media-test-with-state
    (let (delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-remote-media-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (should (equal method "media.materialize"))
                   (should (equal params
                                  `((media_id . ,qq-remote-media-test-id)
                                    (part . "content"))))
                   (funcall
                    callback
                    `((media . ,(qq-remote-media-test-snapshot
                                  :phase "materializing"
                                  :bytes-done "0"
                                  :updated-at 1784700001))))
                   "materialize-request")))
        (should
         (equal
          (qq-remote-media-materialize
           qq-remote-media-test-id
           (lambda (result) (setq delivered result)))
          "materialize-request"))
        (should
         (equal
          (alist-get
           'phase (qq-remote-media-part delivered 'content))
          "materializing"))
        (should (qq-remote-media qq-remote-media-test-id))
        (should-not (qq-resource "res-native-silk"))))))

(ert-deftest qq-remote-media-thumbnail-materialization-selects-exact-part ()
  (qq-remote-media-test-with-state
    (let (delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-remote-media-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (should (equal method "media.materialize"))
                   (should
                    (equal
                     params
                     `((media_id . ,qq-remote-media-test-id)
                       (part . "thumbnail"))))
                   (let ((snapshot
                          (qq-remote-media-test-snapshot
                           :thumbnail
                           (qq-remote-media-test-part
                            :phase "materializing"
                            :expected-size "64"
                            :bytes-total "64"))))
                     (setf (alist-get 'kind snapshot) "video")
                     (funcall callback `((media . ,snapshot))))
                   "thumbnail-request")))
        (should
         (equal
          (qq-remote-media-materialize-part
           qq-remote-media-test-id 'thumbnail
           (lambda (media) (setq delivered media)))
          "thumbnail-request"))
        (should
         (equal
          (alist-get
           'phase (qq-remote-media-part delivered 'content))
          "available"))
        (should
         (equal
          (alist-get
           'phase (qq-remote-media-part delivered 'thumbnail))
          "materializing"))))))

(ert-deftest qq-remote-media-await-materialized-follows-events-and-is-locally-cancellable ()
  (qq-remote-media-test-with-state
    (let (delivered failed)
      (qq-remote-media--upsert
       (qq-remote-media-test-snapshot :phase "materializing") 'test)
      (let ((watch
             (qq-remote-media-await-materialized
              qq-remote-media-test-id
              (lambda (media) (setq delivered media))
              (lambda (_body reason) (setq failed reason)))))
        (should-not delivered)
        (qq-remote-media--handle-event
         "media.changed"
         `((media
            . ,(qq-remote-media-test-snapshot
                :phase "materialized"
                :resource-id "res-native-silk"
                :bytes-done "128"
                :updated-at 1784700001))))
        (should
         (equal
          (alist-get
           'resource_id (qq-remote-media-part delivered 'content))
          "res-native-silk"))
        (should-not failed)
        (should-not (qq-request-watch-active-p watch))))))

(ert-deftest qq-remote-media-cancel-operation-mutates-service-lifecycle ()
  (qq-remote-media-test-with-state
      (let ((operation
           (qq-remote-media-operation-create
            :active-p t :media-id qq-remote-media-test-id
            :part 'content :account-id "slot-a"))
          canceled)
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (method) (equal method "media.cancel")))
                ((symbol-function 'qq-remote-media-cancel-part)
                 (lambda (media-id part &optional _callback _errback)
                   (setq canceled (list media-id part))
                   "cancel-request")))
        (should (qq-remote-media-cancel-operation operation))
        (should-not (qq-remote-media-operation-active-p operation))
        (should
         (equal canceled (list qq-remote-media-test-id 'content)))))))

(ert-deftest qq-remote-media-playback-pipeline-reuses-derived-wav ()
  (qq-remote-media-test-with-state
    (let* ((account-id "slot-a")
           (path (make-temp-file "qq-playback-" nil ".wav" "RIFF"))
           (raw (qq-remote-media-test-resource
                 :resource-id "res-native-silk"))
           (playable (qq-remote-media-test-resource
                      :resource-id "res-playback-wav"
                      :suggested-name "voice.wav"
                      :size "1964"
                      :media-type "audio/wav"))
           (derive-count 0)
           (open-count 0)
           delivered materialize-callback media-ready-callback
           status-callback raw-ready-callback playable-ready-callback
           derive-callback open-callback)
      (setf (alist-get 'suggested_name raw) "voice.silk"
            (alist-get 'media_type raw) "audio/x-tencent-silk"
            (alist-get 'suggested_name playable) "voice.wav"
            (alist-get 'media_type playable) "audio/wav")
      (unwind-protect
          (cl-letf (((symbol-function 'qq-account-current-id)
                     (lambda () account-id))
                    ((symbol-function 'qq-account-get)
                     (lambda (candidate)
                       (and (equal candidate account-id)
                            `((account_id . ,account-id)
                              (phase . "online")))))
                    ((symbol-function 'qq-remote-media-materialize)
                     (lambda (_media-id callback _errback)
                       (setq materialize-callback callback)
                       "materialize"))
                    ((symbol-function 'qq-remote-media-await-materialized)
                     (lambda (_media-id callback _errback)
                       (let ((watch
                              (qq-request-watch-create
                               :active-p t :cancel-function #'ignore)))
                         (setq media-ready-callback
                               (lambda (media)
                                 (qq-request-watch-cancel watch)
                                 (funcall callback media)))
                         watch)))
                    ((symbol-function 'qq-resource-status)
                     (lambda (_resource-id callback _errback)
                       (setq status-callback callback)
                       "resource-status"))
                    ((symbol-function 'qq-resource-await-ready)
                     (lambda (resource-id callback _errback)
                       (let* ((watch
                               (qq-request-watch-create
                                :active-p t :cancel-function #'ignore))
                              (deliver
                               (lambda (resource)
                                 (qq-request-watch-cancel watch)
                                 (funcall callback resource))))
                         (if (equal resource-id "res-native-silk")
                             (setq raw-ready-callback deliver)
                           (setq playable-ready-callback deliver))
                         watch)))
                    ((symbol-function
                      'qq-resource-derive-playable-record)
                     (lambda (_source _name callback _errback)
                       (cl-incf derive-count)
                       (setq derive-callback callback)
                       "derive"))
                    ((symbol-function 'qq-resource-open-local)
                     (lambda (resource-id callback _errback)
                       (cl-incf open-count)
                       (setq open-callback
                             (lambda ()
                               (funcall
                                callback
                                `((access_id
                                   . ,(format
                                       "access-aaaaaaaa-bbbb-4ccc-8ddd-%012d"
                                       open-count))
                                  (resource_id . ,resource-id)
                                  (path . ,path)
                                  (expires_at . 1784703600)))))
                       "open")))
            (cl-labels
                ((drive (derive-p)
                   (setq derive-callback nil
                         playable-ready-callback nil)
                   (let ((operation
                          (qq-remote-media-prepare-record-playback
                           qq-remote-media-test-id
                           (lambda (result) (push result delivered)))))
                     (should
                      (qq-remote-media-operation-active-p operation))
                     (funcall
                      materialize-callback
                      (qq-remote-media-test-snapshot
                       :phase "materializing"
                       :updated-at 1784700001))
                     (funcall
                      media-ready-callback
                      (qq-remote-media-test-snapshot
                       :phase "materialized"
                       :resource-id "res-native-silk"
                       :bytes-done "128"
                       :updated-at 1784700002))
                     (funcall status-callback raw)
                     (puthash "res-native-silk" raw
                              qq-resource--resources)
                     (funcall raw-ready-callback raw)
                     (if derive-p
                         (progn
                           (funcall
                            derive-callback
                            (qq-remote-media-test-resource
                             :resource-id "res-playback-wav"
                             :phase "staging"
                             :suggested-name "voice.wav"
                             :size "1964"))
                           (puthash "res-playback-wav" playable
                                    qq-resource--resources)
                           (funcall playable-ready-callback playable))
                       (should-not derive-callback))
                     (funcall open-callback)
                     (should-not
                      (qq-remote-media-operation-active-p operation)))))
              (drive t)
              (drive nil))
            (should (= derive-count 1))
            (should (= open-count 2))
            (should (= (length delivered) 2))
            (should
             (cl-every
              (lambda (result)
                (and (equal (alist-get 'source_resource_id result)
                            "res-native-silk")
                     (equal (alist-get 'playback_resource_id result)
                            "res-playback-wav")
                     (equal (alist-get 'path (alist-get 'access result)) path)))
              delivered)))
        (delete-file path)))))

(ert-deftest qq-remote-media-events-project-and-remove-opaque_handles ()
  (qq-remote-media-test-with-state
    (let (changes)
      (add-hook 'qq-remote-media-changed-hook
                (lambda (reason media-id)
                  (push (list reason media-id) changes)))
      (qq-remote-media--handle-event
       "media.changed"
       `((media . ,(qq-remote-media-test-snapshot))))
      (should (qq-remote-media qq-remote-media-test-id))
      (qq-remote-media--handle-event
       "media.changed"
       `((media . ,(qq-remote-media-test-snapshot))))
      (should (= (length changes) 1))
      (qq-remote-media--handle-event
       "media.removed" `((media_id . ,qq-remote-media-test-id)))
      (should-not (qq-remote-media qq-remote-media-test-id))
      (should (equal (car changes)
                     `(removed ,qq-remote-media-test-id))))))

(ert-deftest qq-remote-media-ready-uses-typed-single-flight-resync ()
  (qq-remote-media-test-with-state
    (let ((capabilities '("media.list")) calls)
      (cl-letf (((symbol-function 'qq-server-capabilities)
                 (lambda () capabilities))
                ((symbol-function 'qq-remote-media-refresh)
                 (lambda (_callback _errback reason &optional _owner)
                   (push reason calls)
                   "media-list-request")))
        (qq-remote-media--handle-ready "gateway-a")
        (qq-remote-media--handle-ready "gateway-a")
        (should (equal calls '(ready)))
        (should (equal qq-remote-media--gateway-instance-id "gateway-a"))
        (should (equal (car qq-remote-media--resync-request-id)
                       'media-resync))
        (setq capabilities nil)
        (qq-remote-media--handle-ready "gateway-b")
        (should (equal qq-remote-media--gateway-instance-id "gateway-b"))
        (should-not qq-remote-media--resync-request-id)
        (should (equal calls '(ready)))))))

(ert-deftest qq-remote-media-newest-refresh-owns-full-replacement ()
  (qq-remote-media-test-with-state
    (let ((old-id "media-11111111-2222-4333-8444-555555555555")
          (new-id "media-11111111-2222-4333-8444-555555555556")
          requests old-callback old-error new-callback)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("media.list")))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "media-request-%d" (length requests))))))
        (qq-remote-media-refresh
         (lambda (_) (setq old-callback t))
         (lambda (body _failure) (setq old-error body)))
        (qq-remote-media-refresh
         (lambda (_) (setq new-callback t)) #'ignore)
        (funcall
         (car (nth 1 requests))
         `((media . [,(qq-remote-media-test-snapshot :media-id new-id)])))
        (funcall
         (car (nth 0 requests))
         `((media . [,(qq-remote-media-test-snapshot :media-id old-id)])))
        (should new-callback)
        (should-not old-callback)
        (should (equal (alist-get 'code old-error) "superseded_request"))
        (should (qq-remote-media new-id))
        (should-not (qq-remote-media old-id))
        (should-not qq-remote-media--refresh-owner)))))

(ert-deftest qq-remote-media-reset-cancels-pending-refresh ()
  (qq-remote-media-test-with-state
    (let (late-success canceled failures)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("media.list")))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params success _failure &optional _early)
                   (setq late-success success)
                   'media-refresh-token))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-remote-media-refresh
         nil (lambda (body _reason) (push body failures)))
        (qq-remote-media-reset)
        (should (equal canceled '(media-refresh-token)))
        (should (= (length failures) 1))
        (should-not qq-remote-media--refresh-owner)
        (funcall late-success
                 `((media . [,(qq-remote-media-test-snapshot)])))
        (should (= (length failures) 1))
        (should-not (qq-remote-media-list))))))

(provide 'qq-remote-media-test)

;;; qq-remote-media-test.el ends here
