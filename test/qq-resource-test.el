;;; qq-resource-test.el --- Tests for service resources -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-server)
(require 'qq-resource)

(defconst qq-resource-test-capabilities
  '("resource.list" "resource.stage_local" "resource.derive_record"
    "resource.derive_playable_record" "resource.open_local"
    "resource.close_local" "resource.status"
    "resource.release" "resource.import.list_sources"
    "resource.import.list_images" "resource.import.stage_image"))

(defconst qq-resource-test-source-id
  (concat "src-linuxqq-" (make-string 64 ?a)))

(defconst qq-resource-test-candidate-id
  (concat "imp-linuxqq-" (make-string 64 ?b)))

(defun qq-resource-test-import-source (&optional domain-p)
  "Return one pathless native-cache source fixture.

Use a raw vector for `kinds' unless DOMAIN-P requests projected form."
  `((source_id . ,qq-resource-test-source-id)
    (layout . "linuxqq.nt_data.images.v1")
    (kinds . ,(if domain-p '("image") ["image"]))))

(defun qq-resource-test-import-candidate ()
  "Return one pathless native image candidate fixture."
  `((candidate_id . ,qq-resource-test-candidate-id)
    (layout . "linuxqq.nt_data.images.v1")
    (family . "picture")
    (month . "2026-04")
    (suggested_name . "84306143ce7c58d1997a95b413e5c177.png")
    (size . "14")
    (expected_md5 . "84306143ce7c58d1997a95b413e5c177")))

(cl-defun qq-resource-test-snapshot
    (&key
     (resource-id "res-opaque-a")
     (phase "staging")
     (suggested-name "payload.bin")
     (size "3")
     media-type digests
     (created-at 1784700000)
     (updated-at 1784700000)
     (expires-at 1784786400)
     error)
  "Return one closed staged-resource snapshot fixture."
  `((resource_id . ,resource-id)
    (phase . ,phase)
    (suggested_name . ,suggested-name)
    (size . ,size)
    (media_type . ,media-type)
    (digests . ,digests)
    (created_at . ,created-at)
    (updated_at . ,updated-at)
    (expires_at . ,expires-at)
    (error . ,error)))

(defun qq-resource-test-ready (&optional resource-id updated-at)
  "Return one ready fixture for RESOURCE-ID at UPDATED-AT."
  (qq-resource-test-snapshot
   :resource-id (or resource-id "res-opaque-a")
   :phase "ready"
   :media-type "application/octet-stream"
   :digests
   '((sha256 . "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
     (sha1 . "a9993e364706816aba3e25717850c26c9cd0d89d")
     (md5 . "900150983cd24fb0d6963f7d28e17f72"))
   :updated-at (or updated-at 1784700001)))

(defmacro qq-resource-test-with-state (&rest body)
  "Run BODY with an isolated resource projection."
  (declare (indent 0) (debug t))
  `(let ((qq-resource--resources (make-hash-table :test #'equal))
         (qq-resource--order nil)
         (qq-resource--gateway-instance-id nil)
         (qq-resource--refresh-owner nil)
         (qq-resource--resync-request-id nil)
         (qq-resource-changed-hook nil)
         (qq-resource-desync-hook nil))
     ,@body))

(ert-deftest qq-resource-accessors-copy-projected-values ()
  (qq-resource-test-with-state
    (let ((snapshot (qq-resource-test-snapshot)))
      (qq-resource--replace (list snapshot) 'projection-test)
      (let ((first (qq-resource "res-opaque-a")))
        (should (equal (alist-get 'suggested_name first) "payload.bin"))
        (aset (alist-get 'suggested_name first) 0 ?Y)
        (should (equal
                 (alist-get 'suggested_name
                            (qq-resource "res-opaque-a"))
                 "payload.bin"))))))

(ert-deftest qq-resource-stage-local-sends-only-explicit-source-input ()
  (qq-resource-test-with-state
    (let ((path (make-temp-file "qq-resource-" nil ".bin" "abc"))
          sent-method sent-params delivered)
      (unwind-protect
          (cl-letf (((symbol-function 'qq-server-ready-p)
                     (lambda () t))
                    ((symbol-function 'qq-server-capabilities)
                     (lambda () qq-resource-test-capabilities))
                    ((symbol-function 'qq-server-send)
                     (lambda (method params callback _errback &optional _early)
                       (setq sent-method method sent-params params)
                       (funcall callback
                                `((resource
                                   . ,(qq-resource-test-snapshot))))
                       "stage-request")))
            (should
             (equal
              (qq-resource-stage-local
               path "payload.bin" nil
               (lambda (snapshot) (setq delivered snapshot)))
              "stage-request"))
            (should (equal sent-method "resource.stage_local"))
            (should (equal (alist-get 'path sent-params)
                           (expand-file-name path)))
            (should
             (equal (alist-get 'expected sent-params)
                    '((size . "3") (sha256))))
            (should (equal (alist-get 'suggested_name sent-params)
                           "payload.bin"))
            (should (equal (alist-get 'phase delivered) "staging"))
            (should-not (assq 'path (qq-resource "res-opaque-a"))))
        (delete-file path)))))

(ert-deftest qq-resource-derive-record-keeps-source-and-result-distinct ()
  (qq-resource-test-with-state
    (puthash "res-source-wav"
             (qq-resource-test-ready "res-source-wav")
             qq-resource--resources)
    (let (sent-method sent-params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-resource-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    `((resource
                       . ,(qq-resource-test-snapshot
                           :resource-id "res-derived-silk"
                           :suggested-name "voice.silk"
                           :size "27"))))
                   "derive-request")))
        (should
         (equal
          (qq-resource-derive-record
           "res-source-wav" "voice.silk"
           (lambda (snapshot) (setq delivered snapshot)))
          "derive-request"))
        (should (equal sent-method "resource.derive_record"))
        (should
         (equal sent-params
                '((source_resource_id . "res-source-wav")
                  (suggested_name . "voice.silk"))))
        (should (equal (alist-get 'resource_id delivered)
                       "res-derived-silk"))
        (should (qq-resource "res-source-wav"))
        (should (qq-resource "res-derived-silk"))))))

(ert-deftest qq-resource-derive-record-rejects-reused-source-id ()
  (qq-resource-test-with-state
    (let (failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-resource-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    `((resource
                       . ,(qq-resource-test-snapshot
                           :resource-id "res-source-wav"))))
                   "derive-request")))
        (qq-resource-derive-record
         "res-source-wav" nil nil
         (lambda (body reason) (setq failure (list body reason))))
        (should (equal (alist-get 'code (car failure))
                       "invalid_gateway_result"))
        (should (string-match-p "reused its source identity" (cadr failure)))
        (should-not (qq-resource "res-source-wav"))))))

(ert-deftest qq-resource-derive-playable-record-keeps-native-source-distinct ()
  (qq-resource-test-with-state
    (puthash "res-source-silk"
             (qq-resource-test-ready "res-source-silk")
             qq-resource--resources)
    (let (sent-method sent-params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-resource-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    `((resource
                       . ,(qq-resource-test-snapshot
                           :resource-id "res-derived-wav"
                           :suggested-name "voice.wav"
                           :size "1964"))))
                   "derive-playable-request")))
        (should
         (equal
          (qq-resource-derive-playable-record
           "res-source-silk" "voice.wav"
           (lambda (snapshot) (setq delivered snapshot)))
          "derive-playable-request"))
        (should (equal sent-method "resource.derive_playable_record"))
        (should
         (equal sent-params
                '((source_resource_id . "res-source-silk")
                  (suggested_name . "voice.wav"))))
        (should (equal (alist-get 'resource_id delivered) "res-derived-wav"))
        (should (qq-resource "res-source-silk"))
        (should (qq-resource "res-derived-wav"))))))

(ert-deftest qq-resource-local-access-is-one-shot-and-never-a-snapshot-field ()
  (qq-resource-test-with-state
    (let ((path (make-temp-file "qq-local-access-" nil ".wav" "RIFF"))
          calls grant close-receipt)
      (unwind-protect
          (cl-letf (((symbol-function 'qq-server-ready-p)
                     (lambda () t))
                    ((symbol-function 'qq-server-capabilities)
                     (lambda () qq-resource-test-capabilities))
                    ((symbol-function 'qq-server-send)
                     (lambda (method params callback _errback &optional _early)
                       (push (list method params) calls)
                       (pcase method
                         ("resource.open_local"
                          (funcall
                           callback
                           `((access
                              . ((access_id
                                  . "access-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
                                 (resource_id . "res-playable")
                                 (path . ,path)
                                 (expires_at . 1784703600))))))
                         ("resource.close_local"
                          (funcall
                           callback
                           '((access_id
                              . "access-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
                             (closed . t)))))
                       method)))
            (qq-resource-open-local
             "res-playable" (lambda (access) (setq grant access)))
            (should (equal (alist-get 'path grant) path))
            (should (equal (alist-get 'resource_id grant) "res-playable"))
            (should (integerp (alist-get 'expires_at grant)))
            (qq-resource-close-local
             (alist-get 'access_id grant)
             (lambda (receipt) (setq close-receipt receipt)))
            (should (eq (alist-get 'closed close-receipt) t))
            (should
             (member
              '("resource.open_local" ((resource_id . "res-playable")))
              calls))
            (should
             (member
              '("resource.close_local"
                ((access_id
                  . "access-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")))
              calls))
            (should-not
             (assq 'path (qq-resource-test-ready "res-playable"))))
        (delete-file path)))))

(ert-deftest qq-resource-await-ready-observes-terminal-projection-once ()
  (qq-resource-test-with-state
    (puthash "res-await"
             (qq-resource-test-snapshot :resource-id "res-await")
             qq-resource--resources)
    (let (delivered failure)
      (qq-resource-await-ready
       "res-await"
       (lambda (resource) (push resource delivered))
       (lambda (body reason) (setq failure (list body reason))))
      (qq-resource--upsert
       (qq-resource-test-ready "res-await") 'changed)
      (qq-resource--upsert
       (qq-resource-test-ready "res-await") 'duplicate)
      (should (= (length delivered) 1))
      (should (equal (alist-get 'phase (car delivered)) "ready"))
      (should-not failure))))

(ert-deftest qq-resource-events-do-not-regress-terminal-state ()
  (qq-resource-test-with-state
    (let (changes)
      (add-hook 'qq-resource-changed-hook
                (lambda (reason resource-id)
                  (push (list reason resource-id) changes)))
      (qq-resource--handle-event
       "resource.changed"
       `((resource . ,(qq-resource-test-snapshot))))
      (qq-resource--handle-event
       "resource.changed"
       `((resource . ,(qq-resource-test-ready))))
      ;; Replayed content is not a projection change.
      (qq-resource--handle-event
       "resource.changed"
       `((resource . ,(qq-resource-test-ready))))
      ;; A delayed staging response cannot overwrite the ready snapshot.
      (qq-resource--upsert
       (qq-resource-test-snapshot) 'stage-response)
      (should (equal (alist-get 'phase
                                (qq-resource "res-opaque-a"))
                     "ready"))
      (should (= (length changes) 2))
      (qq-resource--handle-event
       "resource.removed" '((resource_id . "res-opaque-a")))
      (should-not (qq-resource "res-opaque-a"))
      (should (equal (car changes) '(removed "res-opaque-a"))))))

(ert-deftest qq-resource-native-import-lists-and-stages-with-opaque-identities ()
  (qq-resource-test-with-state
    (let (calls sources page staged)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-resource-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method params) calls)
                   (pcase method
                     ("resource.import.list_sources"
                      (funcall callback
                               `((sources
                                  . ,(vector
                                      (qq-resource-test-import-source))))))
                     ("resource.import.list_images"
                      (funcall callback
                               `((source_id . ,qq-resource-test-source-id)
                                 (candidates
                                  . ,(vector
                                      (qq-resource-test-import-candidate)))
                                 (next . ,qq-server-wire-null))))
                     ("resource.import.stage_image"
                      (funcall callback
                               `((source_id . ,qq-resource-test-source-id)
                                 (candidate_id
                                  . ,qq-resource-test-candidate-id)
                                 (resource
                                  . ,(qq-resource-test-snapshot))))))
                   method)))
        (qq-resource-import-sources
         (lambda (value) (setq sources value)))
        (qq-resource-import-images
         qq-resource-test-source-id nil 25
         (lambda (value) (setq page value)))
        (qq-resource-import-image
         qq-resource-test-source-id
         qq-resource-test-candidate-id
         (lambda (value) (setq staged value)))
        (should (equal sources
                       (list (qq-resource-test-import-source t))))
        (should
         (equal (alist-get 'candidates page)
                (list (qq-resource-test-import-candidate))))
        (should (equal (alist-get 'phase staged) "staging"))
        (should (qq-resource "res-opaque-a"))
        (should
         (member
          `("resource.import.list_images"
            ((source_id . ,qq-resource-test-source-id)
             (after)
             (limit . 25)))
          calls))
        (should
         (member
          `("resource.import.stage_image"
            ((source_id . ,qq-resource-test-source-id)
             (candidate_id . ,qq-resource-test-candidate-id)))
          calls))))))

(ert-deftest qq-resource-ready-and-lag-errors-request-one-resync ()
  (qq-resource-test-with-state
    (let (calls desync)
      (add-hook 'qq-resource-desync-hook
                (lambda (body) (setq desync body)))
      (cl-letf (((symbol-function 'qq-server-capabilities)
                 (lambda () qq-resource-test-capabilities))
                ((symbol-function 'qq-resource-refresh)
                 (lambda (_callback _errback reason &optional _owner)
                   (push reason calls)
                   "resource-list-request")))
        (qq-resource--handle-ready "gateway-a")
        (should (equal qq-resource--gateway-instance-id "gateway-a"))
        (should (equal (car qq-resource--resync-request-id)
                       'resource-resync))
        (qq-resource--handle-projection-resync
         "resources"
         '((projection . "resources") (skipped . "2")))
        (should (equal calls '(ready)))
        (should (equal (alist-get 'projection desync) "resources"))))))

(ert-deftest qq-resource-newest-refresh-owns-full-replacement ()
  (qq-resource-test-with-state
    (let (requests old-callback old-error new-callback)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("resource.list")))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "resource-request-%d"
                                   (length requests))))))
        (qq-resource-refresh
         (lambda (_) (setq old-callback t))
         (lambda (body _failure) (setq old-error body)))
        (qq-resource-refresh
         (lambda (_) (setq new-callback t)) #'ignore)
        (funcall
         (car (nth 1 requests))
         `((resources .
            [,(qq-resource-test-snapshot
               :resource-id "res-new")])))
        (funcall
         (car (nth 0 requests))
         `((resources .
            [,(qq-resource-test-snapshot
               :resource-id "res-old")])))
        (should new-callback)
        (should-not old-callback)
        (should (equal (alist-get 'code old-error) "superseded_request"))
        (should (qq-resource "res-new"))
        (should-not (qq-resource "res-old"))
        (should-not qq-resource--refresh-owner)))))

(ert-deftest qq-resource-reset-cancels-pending-refresh ()
  (qq-resource-test-with-state
    (let (late-success canceled failures)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("resource.list")))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params success _failure &optional _early)
                   (setq late-success success)
                   'resource-refresh-token))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-resource-refresh
         nil (lambda (body _reason) (push body failures)))
        (qq-resource-reset)
        (should (equal canceled '(resource-refresh-token)))
        (should (= (length failures) 1))
        (should-not qq-resource--refresh-owner)
        (funcall late-success
                 `((resources . [,(qq-resource-test-snapshot)])))
        (should (= (length failures) 1))
        (should-not (qq-resources))))))

(ert-deftest qq-resource-list-replacement-is-atomic-on-duplicates ()
  (qq-resource-test-with-state
    (qq-resource--replace
     (list (qq-resource-test-snapshot)) 'initial)
    (should-error
     (qq-resource--replace
      (list (qq-resource-test-ready)
            (qq-resource-test-ready))
      'resync))
    (should (equal (alist-get 'phase
                              (qq-resource "res-opaque-a"))
                   "staging"))))

(provide 'qq-resource-test)

;;; qq-resource-test.el ends here
