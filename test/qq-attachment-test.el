;;; qq-attachment-test.el --- Tests for prepared attachments -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-attachment)

(defconst qq-attachment-test-id
  "att-11111111-2222-4333-8444-555555555555")

(defconst qq-attachment-test-record-id
  "att-11111111-2222-4333-8444-555555555556")

(defconst qq-attachment-test-video-id
  "att-11111111-2222-4333-8444-555555555557")

(cl-defun qq-attachment-test-snapshot
    (&key
     (attachment-id qq-attachment-test-id)
     (resource-id "res-image-a")
     (account-id "slot-a")
     (conversation '((kind . "group") (group_uin . "8209413637")))
     (use '((kind . "image") (summary . "[图片]") (sub_type . 0)))
     (phase "queued")
     (bytes-done "0")
     (bytes-total "3")
     fast-path
     (created-at 1784700000)
     (updated-at 1784700000)
     error)
  "Return one closed Prepared Attachment snapshot fixture."
  `((attachment_id . ,attachment-id)
    (resource_id . ,resource-id)
    (account_id . ,account-id)
    (conversation . ,(copy-tree conversation))
    (use . ,(copy-tree use))
    (phase . ,phase)
    (bytes_done . ,bytes-done)
    (bytes_total . ,bytes-total)
    (fast_path . ,fast-path)
    (created_at . ,created-at)
    (updated_at . ,updated-at)
    (error . ,error)))

(defun qq-attachment-test-ready (&optional attachment-id resource-id)
  "Return one fast-path ready fixture."
  (qq-attachment-test-snapshot
   :attachment-id (or attachment-id qq-attachment-test-id)
   :resource-id (or resource-id "res-image-a")
   :phase "ready" :fast-path t :updated-at 1784700001))

(defun qq-attachment-test-ready-record ()
  "Return one fast-path ready native-record fixture."
  (qq-attachment-test-snapshot
   :attachment-id qq-attachment-test-record-id
   :resource-id "res-record-a"
   :use '((kind . "record"))
   :phase "ready" :fast-path t :updated-at 1784700001))

(defun qq-attachment-test-ready-video ()
  "Return one fast-path ready native-video fixture."
  (qq-attachment-test-snapshot
   :attachment-id qq-attachment-test-video-id
   :resource-id "res-video-a"
   :use '((kind . "video")
          (thumbnail_resource_id . "res-video-thumbnail-a"))
   :phase "ready" :fast-path t :updated-at 1784700001))

(defun qq-attachment-test-account (&optional phase)
  "Return one selected account fixture in PHASE, normally online."
  `((account_id . "slot-a")
    (label . "Primary")
    (phase . ,(or phase "online"))
    (uin . "10002")
    (uid . "u_self")
    (challenge)
    (problem)))

(defmacro qq-attachment-test-with-state (&rest body)
  "Run BODY with isolated account, resource, and attachment projections."
  (declare (indent 0) (debug t))
  `(let ((qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--refresh-owner nil)
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-attachment--attachments (make-hash-table :test #'equal))
         (qq-attachment--order nil)
         (qq-attachment--gateway-instance-id nil)
         (qq-attachment--refresh-owner nil)
         (qq-attachment--resync-request-id nil)
         (qq-attachment-changed-hook nil)
         (qq-attachment-desync-hook nil)
         (qq-resource--resources (make-hash-table :test #'equal))
         (qq-resource--order nil)
         (qq-resource--gateway-instance-id nil)
         (qq-resource--refresh-owner nil)
         (qq-resource--resync-request-id nil)
         (qq-resource-changed-hook nil)
         (qq-resource-desync-hook nil))
     (qq-account--replace-accounts
      (list (qq-attachment-test-account)) 'test "gateway-test")
     (qq-account-select "slot-a")
     ,@body))

(ert-deftest qq-attachment-conversation-params-use-session-identity ()
  (should
   (equal
    (qq-attachment--conversation-params
     "group:18446744073709551615")
    '((kind . "group") (group_uin . "18446744073709551615"))))
  (should
   (equal
    (qq-attachment--conversation-params "private:10001")
    '((kind . "private") (peer_uin . "10001")))))

(ert-deftest qq-attachment-registry-returns-owned-copies ()
  (qq-attachment-test-with-state
    (let* ((summary (copy-sequence "photo"))
           (snapshot
            (qq-attachment-test-snapshot
             :use `((kind . "image")
                    (summary . ,summary)
                    (sub_type . 0)))))
      (qq-attachment--upsert snapshot 'test)
      (let* ((public
              (qq-attachment qq-attachment-test-id))
             (public-summary
              (alist-get 'summary (alist-get 'use public))))
        (aset public-summary 0 ?Y)
        (should
         (equal
          (alist-get 'summary
                     (alist-get
                      'use
                      (qq-attachment qq-attachment-test-id)))
          "photo"))))))

(ert-deftest qq-attachment-events-never-regress-progress-or-terminal-state ()
  (qq-attachment-test-with-state
    (qq-attachment--upsert
     (qq-attachment-test-snapshot) 'queued)
    (qq-attachment--upsert
     (qq-attachment-test-snapshot
      :phase "negotiating" :updated-at 1784700001)
     'negotiating)
    (qq-attachment--upsert
     (qq-attachment-test-snapshot
      :phase "uploading" :bytes-done "2" :updated-at 1784700002)
     'progress)
    (qq-attachment--upsert
     (qq-attachment-test-snapshot
      :phase "uploading" :bytes-done "1" :updated-at 1784700003)
     'late-progress)
    (should (equal
             (alist-get 'bytes_done
                        (qq-attachment qq-attachment-test-id))
             "2"))
    (qq-attachment--upsert
     (qq-attachment-test-snapshot
      :phase "ready" :fast-path t :updated-at 1784700004)
     'ready)
    (qq-attachment--upsert
     (qq-attachment-test-snapshot) 'late-queued)
    (should (equal
             (alist-get 'phase
                        (qq-attachment qq-attachment-test-id))
             "ready"))))

(ert-deftest qq-attachment-coalesced-events-skip-intermediate-phases-monotonically ()
  (qq-attachment-test-with-state
    (qq-attachment--upsert
     (qq-attachment-test-snapshot
      :phase "ready" :fast-path t :updated-at 1784700001)
     'ready)
    ;; Gateway delivery is coalesced by attachment identity, so `sending' is
    ;; not guaranteed to reach the client before the terminal snapshot.
    (qq-attachment--upsert
     (qq-attachment-test-snapshot
      :phase "consumed" :fast-path t :updated-at 1784700002)
     'consumed)
    (should (equal
             (alist-get 'phase
                        (qq-attachment qq-attachment-test-id))
             "consumed"))
    (should-error
     (qq-attachment--upsert
      (qq-attachment-test-snapshot
       :phase "failed" :fast-path t :updated-at 1784700003
       :error '((code . "late_failure") (message . "late failure")))
      'late-failure)
     :type 'error)))

(ert-deftest qq-attachment-identical-event-notifies-once ()
  (qq-attachment-test-with-state
    (let (changes)
      (add-hook 'qq-attachment-changed-hook
                (lambda (reason attachment-id)
                  (push (list reason attachment-id) changes)))
      (dotimes (_ 2)
        (qq-attachment--handle-event
         "attachment.changed"
         `((attachment . ,(qq-attachment-test-snapshot)))))
      (should
       (equal changes
              `((changed ,qq-attachment-test-id)))))))

(ert-deftest qq-attachment-ready-uses-typed-single-flight-resync ()
  (qq-attachment-test-with-state
    (let ((capabilities '("attachment.list")) calls)
      (cl-letf (((symbol-function 'qq-server-capabilities)
                 (lambda () capabilities))
                ((symbol-function 'qq-attachment-refresh)
                 (lambda (_callback _errback reason &optional _owner)
                   (push reason calls)
                   "attachment-list-request")))
        (qq-attachment--handle-ready "gateway-a")
        (qq-attachment--handle-ready "gateway-a")
        (should (equal calls '(ready)))
        (should (equal qq-attachment--gateway-instance-id
                       "gateway-a"))
        (should (equal (car qq-attachment--resync-request-id)
                       'attachment-resync))
        (setq capabilities nil)
        (qq-attachment--handle-ready "gateway-b")
        (should (equal qq-attachment--gateway-instance-id
                       "gateway-b"))
        (should-not qq-attachment--resync-request-id)
        (should (equal calls '(ready)))))))

(ert-deftest qq-attachment-newest-refresh-owns-full-replacement ()
  (qq-attachment-test-with-state
    (let (requests old-callback old-error new-callback)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("attachment.list")))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "attachment-request-%d"
                                   (length requests))))))
        (qq-attachment-refresh
         (lambda (_) (setq old-callback t))
         (lambda (body _failure) (setq old-error body)))
        (qq-attachment-refresh
         (lambda (_) (setq new-callback t)) #'ignore)
        (funcall
         (car (nth 1 requests))
         `((attachments .
            [,(qq-attachment-test-snapshot
               :attachment-id qq-attachment-test-record-id
               :resource-id "res-new")])))
        (funcall
         (car (nth 0 requests))
         `((attachments .
            [,(qq-attachment-test-snapshot
               :resource-id "res-old")])))
        (should new-callback)
        (should-not old-callback)
        (should (equal (alist-get 'code old-error) "superseded_request"))
        (should (qq-attachment
                 qq-attachment-test-record-id))
        (should-not (qq-attachment qq-attachment-test-id))
        (should-not qq-attachment--refresh-owner)))))

(ert-deftest qq-attachment-reset-cancels-pending-refresh ()
  (qq-attachment-test-with-state
    (let (late-success canceled failures)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("attachment.list")))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params success _failure &optional _early)
                   (setq late-success success)
                   'attachment-refresh-token))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-attachment-refresh
         nil (lambda (body _reason) (push body failures)))
        (qq-attachment-reset)
        (should (equal canceled '(attachment-refresh-token)))
        (should (= (length failures) 1))
        (should-not qq-attachment--refresh-owner)
        (funcall late-success
                 `((attachments .
                    [,(qq-attachment-test-snapshot)])))
        (should (= (length failures) 1))
        (should-not (qq-attachments))))))

(ert-deftest qq-attachment-prepare-binds-stable-account-and-chat ()
  (qq-attachment-test-with-state
    (puthash "res-image-a"
             '((resource_id . "res-image-a") (phase . "ready"))
             qq-resource--resources)
    (let (method params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("attachment.prepare")))
                ((symbol-function 'qq-server-send)
                 (lambda (wire-method wire-params success _failure
                                      &optional _early)
                   (setq method wire-method params wire-params)
                   (funcall success
                            `((attachment
                               . ,(qq-attachment-test-snapshot))))
                   "prepare-request")))
        (should
         (equal
          (qq-attachment-prepare-image
           "group:8209413637" "res-image-a" "[图片]" 0
           (lambda (snapshot) (setq delivered snapshot)))
          "prepare-request"))
        (should (equal method "attachment.prepare"))
        (should (equal (alist-get 'account_id params) "slot-a"))
        (should (equal (alist-get 'resource_id params) "res-image-a"))
        (should (equal (alist-get 'conversation params)
                       '((kind . "group") (group_uin . "8209413637"))))
        (should-not (assq 'generation params))
        (should-not (assq 'generation delivered))))))

(ert-deftest qq-attachment-prepare-record-sends-only-closed-use ()
  (qq-attachment-test-with-state
    (puthash "res-record-a"
             '((resource_id . "res-record-a") (phase . "ready"))
             qq-resource--resources)
    (let (method params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("attachment.prepare")))
                ((symbol-function 'qq-server-send)
                 (lambda (wire-method wire-params success _failure
                                      &optional _early)
                   (setq method wire-method params wire-params)
                   (funcall
                    success
                    `((attachment
                       . ,(qq-attachment-test-snapshot
                           :attachment-id qq-attachment-test-record-id
                           :resource-id "res-record-a"
                           :use '((kind . "record"))))))
                   "prepare-record-request")))
        (should
         (equal
          (qq-attachment-prepare-record
           "group:8209413637" "res-record-a"
           (lambda (snapshot) (setq delivered snapshot)))
          "prepare-record-request"))
        (should (equal method "attachment.prepare"))
        (should (equal (alist-get 'use params) '((kind . "record"))))
        (should (equal (alist-get 'use delivered) '((kind . "record"))))))))

(ert-deftest qq-attachment-prepare-video-binds-two-distinct-ready-resources ()
  (qq-attachment-test-with-state
    (puthash "res-video-a"
             '((resource_id . "res-video-a") (phase . "ready"))
             qq-resource--resources)
    (puthash "res-video-thumbnail-a"
             '((resource_id . "res-video-thumbnail-a") (phase . "ready"))
             qq-resource--resources)
    (let (method params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("attachment.prepare")))
                ((symbol-function 'qq-server-send)
                 (lambda (wire-method wire-params success _failure
                                      &optional _early)
                   (setq method wire-method params wire-params)
                   (funcall
                    success
                    `((attachment . ,(qq-attachment-test-ready-video))))
                   "prepare-video-request")))
        (should
         (equal
          (qq-attachment-prepare-video
           "group:8209413637" "res-video-a" "res-video-thumbnail-a"
           (lambda (snapshot) (setq delivered snapshot)))
          "prepare-video-request"))
        (should (equal method "attachment.prepare"))
        (should
         (equal (alist-get 'use params)
                '((kind . "video")
                  (thumbnail_resource_id . "res-video-thumbnail-a"))))
        (should
         (equal (alist-get 'use delivered)
                '((kind . "video")
                  (thumbnail_resource_id . "res-video-thumbnail-a"))))
        (should-error
         (qq-attachment-prepare-video
          "group:8209413637" "res-video-a" "res-video-a")
         :type 'user-error)))))

(ert-deftest qq-attachment-sendable-checks-account-and-conversation ()
  (qq-attachment-test-with-state
    (qq-attachment--upsert
     (qq-attachment-test-ready) 'ready)
    (should
     (equal
      (qq-attachment-assert-sendable
       qq-attachment-test-id "group:8209413637" "slot-a")
      qq-attachment-test-id))
    ;; Runtime identity pairs are not part of the client contract.
    (should-error
     (qq-attachment-assert-sendable
      qq-attachment-test-id "group:8209413637" '("slot-a" . "ignored"))
     :type 'user-error)
    (should-error
     (qq-attachment-assert-sendable
      qq-attachment-test-id "group:8209413637" "slot-b")
     :type 'user-error)
    (should-error
     (qq-attachment-assert-sendable
      qq-attachment-test-id "group:10001" "slot-a")
     :type 'user-error)
    (qq-attachment--upsert
     (qq-attachment-test-ready-record) 'ready-record)
    (should
     (equal
      (qq-attachment-assert-sendable
       qq-attachment-test-record-id
       "group:8209413637" "slot-a" "record")
      qq-attachment-test-record-id))
    (should-error
     (qq-attachment-assert-sendable
      qq-attachment-test-record-id
      "group:8209413637" "slot-a")
     :type 'user-error)
    (qq-attachment--upsert
     (qq-attachment-test-ready-video) 'ready-video)
    (should
     (equal
      (qq-attachment-assert-sendable
       qq-attachment-test-video-id
       "group:8209413637" "slot-a" "video")
      qq-attachment-test-video-id))))

(ert-deftest qq-attachment-sendable-accepts-numeric-or-reordered-conversation ()
  "Conversation identity is kind + target id, never raw alist ordering or JSON number/string kind."
  (qq-attachment-test-with-state
    ;; Snapshot projected with a JSON number group_uin and reversed key order:
    ;; raw alist `equal' would fail, but kind + target-id must still match.
    (qq-attachment--upsert
     (qq-attachment-test-snapshot
      :conversation '((group_uin . 8209413637) (kind . "group"))
      :phase "ready" :fast-path t :updated-at 1784700005)
     'ready)
    (should
     (equal
      (qq-attachment-assert-sendable
       qq-attachment-test-id "group:8209413637" "slot-a")
      qq-attachment-test-id))))

(ert-deftest qq-attachment-await-observer-is-explicitly-cancellable ()
  (qq-attachment-test-with-state
    (qq-attachment--upsert
     (qq-attachment-test-snapshot
      :phase "negotiating" :updated-at 1784700001)
     'negotiating)
    (let ((called nil)
          (watch
           (qq-attachment--await
            qq-attachment-test-id
            (lambda (_) (setq called t))
            (lambda (&rest _) (setq called t)))))
      (should (= (length qq-attachment-changed-hook) 1))
      (qq-request-watch-cancel watch)
      (should-not qq-attachment-changed-hook)
      (qq-attachment--upsert
       (qq-attachment-test-snapshot
        :phase "ready" :fast-path t :updated-at 1784700002)
       'ready)
      (should-not called))))

(ert-deftest qq-attachment-record-releases-source-after-derivation ()
  (qq-attachment-test-with-state
    (let ((path (make-temp-file "qq-record-" nil ".wav" "pcm"))
          derived-source prepared-resource delivered released)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-resource-stage-local)
                (lambda (_path _name _digest success _failure)
                  (puthash "res-record-source"
                           '((resource_id . "res-record-source")
                             (phase . "ready"))
                           qq-resource--resources)
                  (funcall success
                           '((resource_id . "res-record-source")
                             (phase . "staging")))
                  "stage-request"))
               ((symbol-function 'qq-resource-derive-record)
                (lambda (source-id _name success _failure)
                  (setq derived-source source-id)
                  (puthash "res-record-a"
                           '((resource_id . "res-record-a") (phase . "ready"))
                           qq-resource--resources)
                  (funcall success
                           '((resource_id . "res-record-a")
                             (phase . "staging")))
                  "derive-request"))
               ((symbol-function 'qq-resource-release)
                (lambda (resource-id &rest _)
                  (push resource-id released)))
               ((symbol-function 'qq-attachment-prepare-record)
                (lambda (_session resource-id success _failure)
                  (setq prepared-resource resource-id)
                  (let ((ready (qq-attachment-test-ready-record)))
                    (puthash qq-attachment-test-record-id ready
                             qq-attachment--attachments)
                    (funcall success
                             (qq-attachment-test-snapshot
                              :attachment-id
                              qq-attachment-test-record-id
                              :resource-id "res-record-a"
                              :use '((kind . "record")))))
                  "prepare-request")))
            (let ((operation
                   (qq-attachment-stage-and-prepare-record
                    "group:8209413637" path
                    (lambda (snapshot) (setq delivered snapshot)) #'ignore)))
              (should-not
               (qq-attachment-operation-active-p operation))
              (should (equal derived-source "res-record-source"))
              (should (equal prepared-resource "res-record-a"))
              (should (equal released '("res-record-source")))
              (should (equal (alist-get 'phase delivered) "ready"))))
        (delete-file path)))))

(ert-deftest qq-attachment-record-cancel-during-derivation-releases-source ()
  (qq-attachment-test-with-state
    (let ((path (make-temp-file "qq-record-cancel-" nil ".wav" "pcm"))
          stage-success canceled released)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-resource-stage-local)
                (lambda (_path _name _digest success _failure)
                  (setq stage-success success)
                  "stage-request"))
               ((symbol-function 'qq-resource-derive-record)
                (lambda (_source _name _success _failure) "derive-request"))
               ((symbol-function 'qq-server-cancel)
                (lambda (request-id) (setq canceled request-id)))
               ((symbol-function 'qq-resource-release)
                (lambda (resource-id &rest _) (push resource-id released))))
            (let ((operation
                   (qq-attachment-stage-and-prepare-record
                    "private:10001" path nil #'ignore)))
              (puthash "res-record-source"
                       '((resource_id . "res-record-source")
                         (phase . "ready"))
                       qq-resource--resources)
              (funcall stage-success '((resource_id . "res-record-source")))
              (should (qq-attachment-operation-active-p operation))
              (should (qq-attachment-cancel-operation operation))
              (should (equal canceled "derive-request"))
              (should (equal released '("res-record-source")))))
        (delete-file path)))))

(ert-deftest qq-attachment-favorite-materializes-before-image-preparation ()
  (qq-attachment-test-with-state
    (let ((favorite-id
           "10002_0_0_0_DEADBEEFDEADBEEFDEADBEEFDEADBEEF_0_0")
          prepared delivered)
      (cl-letf
          (((symbol-function 'qq-favorite-emoji-materialize)
            (lambda (requested-id callback _errback)
              (should (equal requested-id favorite-id))
              (funcall
               callback
               '((resource
                  . ((resource_id . "res-favorite-a")
                     (phase . "ready")))))
              "materialize-request"))
           ((symbol-function 'qq-resource-await-ready)
            (lambda (resource-id callback _errback)
              (funcall callback `((resource_id . ,resource-id)
                                  (phase . "ready")))
              (qq-request-watch-create :active-p nil)))
           ((symbol-function 'qq-attachment-prepare-image)
            (lambda (session resource-id summary sub-type callback _errback)
              (setq prepared (list session resource-id summary sub-type))
              (let ((queued
                     (qq-attachment-test-snapshot
                      :resource-id resource-id
                      :use `((kind . "image")
                             (summary . ,summary)
                             (sub_type . ,sub-type)))))
                (qq-attachment--upsert queued 'prepare)
                (funcall callback queued))
              "prepare-request")))
        (let ((operation
               (qq-attachment-materialize-and-prepare-favorite
                "group:8209413637" favorite-id
                (lambda (snapshot) (setq delivered snapshot)) #'ignore)))
          (should (qq-attachment-operation-active-p operation))
          (should
           (equal prepared
                  '("group:8209413637" "res-favorite-a" "[收藏表情]" 1)))
          (qq-attachment--upsert
           (qq-attachment-test-snapshot
            :resource-id "res-favorite-a"
            :use '((kind . "image")
                   (summary . "[收藏表情]")
                   (sub_type . 1))
            :phase "ready" :fast-path t :updated-at 1784700001)
           'ready)
          (should (equal (alist-get 'phase delivered) "ready"))
          (should-not (qq-attachment-operation-active-p operation)))))))

(ert-deftest qq-attachment-preparation-survives-runtime-restart ()
  (qq-attachment-test-with-state
    (let ((path (make-temp-file "qq-image-owner-" nil ".png" "abc"))
          delivered failure released-resource released-attachment)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-resource-stage-local)
                (lambda (_path _name _digest success _failure)
                  (funcall success '((resource_id . "res-image-a")))
                  "stage-request"))
               ((symbol-function 'qq-resource-await-ready)
                (lambda (_resource-id callback _errback)
                  (funcall callback
                           '((resource_id . "res-image-a") (phase . "ready")))
                  (qq-request-watch-create :active-p nil)))
               ((symbol-function 'qq-attachment-prepare-image)
                (lambda (_session _resource _summary _sub success _failure)
                  (let ((queued (qq-attachment-test-snapshot)))
                    (qq-attachment--upsert queued 'prepare)
                    (funcall success queued))
                  "prepare-request"))
               ((symbol-function 'qq-resource-release)
                (lambda (resource-id &rest _)
                  (setq released-resource resource-id)))
               ((symbol-function 'qq-attachment-release)
                (lambda (attachment-id &rest _)
                  (setq released-attachment attachment-id))))
            (let ((operation
                   (qq-attachment-stage-and-prepare-image
                    "group:8209413637" path nil nil
                    (lambda (snapshot) (setq delivered snapshot))
                    (lambda (body reason) (setq failure (list body reason))))))
              (should (qq-attachment-operation-active-p operation))
              ;; A stop/start cycle replaces the same stable account slot.
              ;; Attachment ownership remains service-side and opaque.
              (qq-account--upsert-account
               (qq-attachment-test-account "stopped") 'changed)
              (qq-account--upsert-account
               (qq-attachment-test-account "starting") 'changed)
              (qq-account--upsert-account
               (qq-attachment-test-account "online") 'changed)
              (qq-attachment--upsert
               (qq-attachment-test-snapshot
                :phase "negotiating" :updated-at 1784700001)
               'negotiating)
              (qq-attachment--upsert
               (qq-attachment-test-snapshot
                :phase "ready" :fast-path t :updated-at 1784700002)
               'ready)
              (should delivered)
              (should-not
               (qq-attachment-operation-active-p operation))
              (should-not failure)
              (should-not released-resource)
              (should-not released-attachment))
            (delete-file path))))))

(ert-deftest qq-attachment-cancel-revokes-local-work-and-created-objects ()
  (let ((operation
         (qq-attachment-operation-create
          :active-p t :request-id "request-a"
          :resource-id "res-image-a"
          :attachment-id qq-attachment-test-id
          :resource-watch
          (qq-request-watch-create :active-p t :cancel-function #'ignore)
          :attachment-watch
          (qq-request-watch-create :active-p t :cancel-function #'ignore)))
        canceled released-resource released-attachment)
    (cl-letf (((symbol-function 'qq-server-cancel)
               (lambda (request-id) (setq canceled request-id)))
              ((symbol-function 'qq-resource-release)
               (lambda (resource-id &rest _)
                 (setq released-resource resource-id)))
              ((symbol-function 'qq-attachment-release)
               (lambda (attachment-id &rest _)
                 (setq released-attachment attachment-id))))
      (should (qq-attachment-cancel-operation operation))
      (should-not (qq-attachment-cancel-operation operation))
      (should (equal canceled "request-a"))
      (should (equal released-resource "res-image-a"))
      (should (equal released-attachment qq-attachment-test-id)))))

(ert-deftest qq-attachment-video-cancel-releases-body-thumbnail-and-temp-file ()
  (let* ((thumbnail-file
          (make-temp-file "qq-video-cancel-" nil ".jpg" "jpeg"))
         (operation
          (qq-attachment-operation-create
           :active-p t
           :resource-id "res-video-a"
           :thumbnail-resource-id "res-video-thumbnail-a"
           :thumbnail-file thumbnail-file))
         released)
    (cl-letf (((symbol-function 'qq-resource-release)
               (lambda (resource-id &rest _)
                 (push resource-id released))))
      (should (qq-attachment-cancel-operation operation))
      (should-not (file-exists-p thumbnail-file))
      (should
       (equal (sort released #'string<)
              '("res-video-a" "res-video-thumbnail-a"))))))

(ert-deftest qq-attachment-video-rejects-non-mp4-before-extraction ()
  (qq-attachment-test-with-state
    (let ((path (make-temp-file "qq-video-format-" nil ".mkv" "mkv"))
          extracted)
      (unwind-protect
          (cl-letf (((symbol-function 'qq-attachment--extract-video-thumbnail)
                     (lambda (&rest _) (setq extracted t))))
            (should-error
             (qq-attachment-stage-and-prepare-video
              "group:8209413637" path)
             :type 'user-error)
            (should-not extracted))
        (delete-file path)))))

(ert-deftest qq-attachment-video-generates-stages-and-prepares-both-resources ()
  (qq-attachment-test-with-state
    (let ((video-path (make-temp-file "qq-video-send-" nil ".mp4" "mp4"))
          staged-paths prepared delivered failure operation)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-runtime-current-account-id)
                (lambda () "slot-a"))
               ((symbol-function 'qq-attachment--extract-video-thumbnail)
                (lambda (_operation _video thumbnail success _failure)
                  (with-temp-file thumbnail
                    (insert "jpeg"))
                  (funcall success)
                  nil))
               ((symbol-function 'qq-resource-stage-local)
                (lambda (path _name _digest success _failure)
                  (let ((resource-id
                         (if (equal path video-path)
                             "res-video-a"
                           "res-video-thumbnail-a")))
                    (setq staged-paths (append staged-paths (list path)))
                    (puthash resource-id
                             `((resource_id . ,resource-id) (phase . "ready"))
                             qq-resource--resources)
                    (funcall success
                             `((resource_id . ,resource-id)
                               (phase . "staging")))
                    (concat "stage-" resource-id))))
               ((symbol-function 'qq-attachment-prepare-video)
                (lambda (_session resource-id thumbnail-id success _failure)
                  (setq prepared (list resource-id thumbnail-id))
                  (let ((queued
                         (qq-attachment-test-snapshot
                          :attachment-id qq-attachment-test-video-id
                          :resource-id resource-id
                          :use `((kind . "video")
                                 (thumbnail_resource_id . ,thumbnail-id)))))
                    (qq-attachment--upsert queued 'prepare)
                    (funcall success queued)
                    (qq-attachment--upsert
                     (qq-attachment-test-snapshot
                      :attachment-id qq-attachment-test-video-id
                      :resource-id resource-id
                      :use `((kind . "video")
                             (thumbnail_resource_id . ,thumbnail-id))
                      :phase "negotiating" :updated-at 1784700001)
                     'negotiating)
                    (qq-attachment--upsert
                     (qq-attachment-test-snapshot
                      :attachment-id qq-attachment-test-video-id
                      :resource-id resource-id
                      :use `((kind . "video")
                             (thumbnail_resource_id . ,thumbnail-id))
                      :phase "ready" :fast-path t :updated-at 1784700002)
                     'ready))
                  "prepare-video-request"))
               ((symbol-function 'qq-server-cancel) (lambda (_token) t)))
            (setq operation
                  (qq-attachment-stage-and-prepare-video
                   "group:8209413637" video-path
                   (lambda (snapshot) (setq delivered snapshot))
                   (lambda (body reason) (setq failure (list body reason)))))
            (should-not (qq-attachment-operation-active-p operation))
            (should-not failure)
            (should (equal prepared
                           '("res-video-a" "res-video-thumbnail-a")))
            (should (equal (alist-get 'phase delivered) "ready"))
            (should (= (length staged-paths) 2))
            (should (equal (car staged-paths) video-path))
            (should-not (file-exists-p (cadr staged-paths))))
        (when (file-exists-p video-path)
          (delete-file video-path))))))

(provide 'qq-attachment-test)

;;; qq-attachment-test.el ends here
