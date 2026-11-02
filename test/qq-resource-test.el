;;; qq-resource-test.el --- Tests for service resources -*- lexical-binding: t; -*-

(require 'qq-test-state)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-server)
(require 'qq-resource)

(defconst qq-resource-test-capabilities
  '("resource.list" "resource.stage_local" "resource.derive_record"
    "resource.derive_playable_record" "resource.open_local"
    "resource.close_local" "resource.renew_local" "resource.acquire_url"
    "resource.derive_preview" "resource.status"
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
         (qq-resource--access-pool (make-hash-table :test #'equal))
         (qq-resource--access-generation 0)
         (qq-resource-access-invalidated-hook nil)
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

(ert-deftest qq-resource-local-access-is-renewable-and-never-a-snapshot-field ()
  (qq-resource-test-with-state
   (let ((path (make-temp-file "qq-local-access-" nil ".wav" "RIFF"))
         calls grant renewed close-receipt)
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
                        ("resource.renew_local"
                         (funcall
                          callback
                          `((access
                             . ((access_id
                                 . "access-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
                                (resource_id . "res-playable")
                                (path . ,path)
                                (expires_at . 1784707200))))))
                        ("resource.close_local"
                         (funcall
                          callback
                          '((access_id
                             . "access-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
                            (closed . t)))))
                      method)))
           (qq-resource--upsert (qq-resource-test-ready "res-playable") 'test)
           (qq-resource-open-local
            "res-playable" (lambda (access) (setq grant access)))
           (should (equal (alist-get 'path grant) path))
           (should (equal (alist-get 'resource_id grant) "res-playable"))
           (should (integerp (alist-get 'expires_at grant)))
           (qq-resource-renew-local
            (alist-get 'access_id grant)
            (lambda (access) (setq renewed access)))
           (should (equal (alist-get 'path renewed) path))
           (should (= (alist-get 'expires_at renewed) 1784707200))
           (should
            (member
             '("resource.renew_local"
               ((access_id . "access-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")))
             calls))
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
            (assq 'path (qq-resource "res-playable"))))
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

(defmacro qq-resource-test-with-access (&rest body)
  "Run BODY with controlled epoch seconds and asynchronous access RPCs."
  (declare (indent 0) (debug t))
  `(qq-resource-test-with-state
    (let ((path (make-temp-file "qq-pool-" nil ".bin" "immutable"))
          (now 1784700000.0)
          opens renewals closes timers invalidations)
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now))
                ((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-resource-open-local)
                 (lambda (id success failure)
                   (push (list id success failure) opens)
                   'open-request))
                ((symbol-function 'qq-resource-renew-local)
                 (lambda (id success failure)
                   (push (list id success failure) renewals)
                   'renew-request))
                ((symbol-function 'qq-resource-close-local)
                 (lambda (id &optional _success _failure)
                   (push id closes)))
                ((symbol-function 'run-at-time)
                 (lambda (delay repeat callback &rest args)
                   (should-not repeat)
                   (let ((timer (vector delay callback args nil)))
                     (push timer timers)
                     timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (aset timer 3 t))))
        (unwind-protect
            (progn
              (add-hook 'qq-resource-access-invalidated-hook
                        (lambda (id reason)
                          (push (list id reason) invalidations)))
              ,@body)
          (qq-resource-access-reset)
          (delete-file path))))))

(defun qq-resource-test-access (path expires &optional access-id resource-id)
  "Return a complete grant fixture for PATH, EXPIRES, ACCESS-ID and RESOURCE-ID."
  `((access_id . ,(or access-id "access-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee"))
    (resource_id . ,(or resource-id "res-pooled"))
    (path . ,path)
    (expires_at . ,(truncate expires))))

(ert-deftest qq-resource-pool-shares-pending-and-releases-independent-consumers ()
  (qq-resource-test-with-access
   (let* (delivered
          (first (qq-resource-acquire-access
                  "res-pooled" (lambda (grant ref) (push (cons grant ref) delivered))
                  #'ignore))
          (second (qq-resource-access-retain
                   first (lambda (grant ref) (push (cons grant ref) delivered))
                   #'ignore)))
     (should (= (length opens) 1))
     (should-not (eq first second))
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (should (= (length delivered) 2))
     (should (eq (cdr (car delivered)) first))
     (qq-resource-access-release first)
     (should-not (qq-resource-access-release first))
     (should-not closes)
     (should (qq-resource-access-grant second))
     (should-not (qq-resource-access-grant first))
     (qq-resource-access-release second)
     (should (= (length closes) 1))
     (should (aref (car timers) 3))
     (should (= (hash-table-count qq-resource--access-pool) 0))
     (should-not invalidations))))

(ert-deftest qq-resource-pool-renews-before-hour-and-keeps-ref-identity ()
  (qq-resource-test-with-access
   (let* ((delivered 0)
          (ref (qq-resource-acquire-access
                "res-pooled" (lambda (_grant _ref) (cl-incf delivered)) #'ignore)))
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (should (= (aref (car timers) 0) 3540))
     (setq now (+ now 3540))
     (funcall (aref (car timers) 1))
     (should (= (length renewals) 1))
     (should (= (aref (car timers) 0) 60))
     (funcall (nth 1 (car renewals)) (qq-resource-test-access path (+ now 3600)))
     (should (= delivered 1))
     (setq now (+ now 120))
     (should (equal (alist-get 'path (qq-resource-access-grant ref)) path))
     (should (> (alist-get 'expires_at (qq-resource-access-grant ref)) now))
     (should-not invalidations)
     (qq-resource-access-release ref)
     (should (= (length closes) 1)))))

(ert-deftest qq-resource-pool-accepts-same-second-renewal ()
  (qq-resource-test-with-access
   (let* ((ref (qq-resource-acquire-access "res-pooled" nil #'ignore))
          (grant (qq-resource-test-access path (+ now 3600))))
     (funcall (nth 1 (car opens)) grant)
     (qq-resource--access-request (qq-resource--access-ref-entry ref) t)
     (funcall (nth 1 (car renewals)) grant)
     (should (equal (qq-resource-access-grant ref) grant))
     (should-not invalidations)
     (qq-resource-access-release ref))))

(ert-deftest qq-resource-pool-startup-quit-allows-a-fresh-acquisition ()
  (qq-resource-test-with-access
   (let (quit-seen)
     (cl-letf (((symbol-function 'qq-resource-open-local)
                (lambda (&rest _) (signal 'quit nil))))
       (condition-case nil
           (qq-resource-acquire-access "res-pooled" nil #'ignore)
         (quit (setq quit-seen t))))
     (should quit-seen)
     (let ((ref (qq-resource-acquire-access "res-pooled" nil #'ignore)))
       (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
       (should (equal (alist-get 'path (qq-resource-access-grant ref)) path))
       (qq-resource-access-release ref)))))

(ert-deftest qq-resource-pool-suspend-check-invalidates-all-consumers-on-use ()
  (qq-resource-test-with-access
   (let* ((first (qq-resource-acquire-access "res-pooled" nil #'ignore))
          (second (qq-resource-access-retain first nil #'ignore)))
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (setq now (+ now 3601))
     (should-not (qq-resource-access-grant first))
     (should-not (qq-resource-access-grant second))
     (should (= (length closes) 1))
     (should (equal invalidations '(("res-pooled" expired))))
     (should-not (qq-resource-access-retain second nil #'ignore)))))

(ert-deftest qq-resource-pool-pending-release-closes-late-orphan ()
  (qq-resource-test-with-access
   (let* (delivered
          (ref (qq-resource-acquire-access
                "res-pooled" (lambda (&rest _) (setq delivered t)) #'ignore)))
     (qq-resource-access-release ref)
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (should-not delivered)
     (should (= (length closes) 1))
     (should-not timers))))

(ert-deftest qq-resource-pool-renew-failure-invalidates-one-resource-once ()
  (qq-resource-test-with-access
   (let* ((first (qq-resource-acquire-access "res-pooled" nil #'ignore))
          (second (qq-resource-access-retain first nil #'ignore)))
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (setq now (+ now 3540))
     (funcall (aref (car timers) 1))
     (funcall (nth 2 (car renewals)) '((code . "resource_access_expired")) "expired")
     (should-not (qq-resource-access-grant first))
     (should-not (qq-resource-access-grant second))
     (should (equal invalidations '(("res-pooled" renew-failed))))
     (should (= (length closes) 1)))))

(ert-deftest qq-resource-pool-rejects-malformed-and-mutated-renewals ()
  (dolist (field '(access_id resource_id path expires_at missing))
    (qq-resource-test-with-access
     (let ((ref (qq-resource-acquire-access "res-pooled" nil #'ignore)))
       (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
       (setq now (+ now 3540))
       (funcall (aref (car timers) 1))
       (let ((grant (qq-resource-test-access path (+ now 3600))))
         (pcase field
           ('access_id
            (setf (alist-get field grant)
                  "access-bbbbbbbb-bbbb-4ccc-8ddd-eeeeeeeeeeee"))
           ('resource_id (setf (alist-get field grant) "res-other"))
           ('path
            (setf (alist-get field grant)
                  (concat (file-name-directory path) "./"
                          (file-name-nondirectory path))))
           ('expires_at (setf (alist-get field grant) (1- (truncate now))))
           ('missing (setq grant (assq-delete-all 'expires_at grant))))
         (funcall (nth 1 (car renewals)) grant))
       (should-not (qq-resource-access-grant ref))
       (should (equal invalidations '(("res-pooled" renew-failed))))))))

(ert-deftest qq-resource-pool-expiry-watchdog-rejects-late-renewal ()
  (qq-resource-test-with-access
   (let ((ref (qq-resource-acquire-access "res-pooled" nil #'ignore)))
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (setq now (+ now 3540))
     (funcall (aref (car timers) 1))
     (setq now (+ now 61))
     (funcall (aref (car timers) 1))
     (funcall (nth 1 (car renewals)) (qq-resource-test-access path (+ now 3600)))
     (should-not (qq-resource-access-grant ref))
     (should (equal invalidations '(("res-pooled" expired))))
     (should (= (hash-table-count qq-resource--access-pool) 0)))))

(ert-deftest qq-resource-pool-same-instance-ready-revokes-old-open-and-renew ()
  (qq-resource-test-with-access
   (let* ((qq-resource--gateway-instance-id "same-instance")
          (ref (qq-resource-acquire-access "res-pooled" nil #'ignore))
          (opening (qq-resource-acquire-access "res-opening" nil #'ignore))
          (old-open (car opens)))
     (funcall (nth 1 (cadr opens)) (qq-resource-test-access path (+ now 3600)))
     (setq now (+ now 3540))
     (funcall (aref (car timers) 1))
     (cl-letf (((symbol-function 'qq-rpc-method-available-p) (lambda (_) nil)))
       (qq-resource--handle-ready "same-instance"))
     (should-not (qq-resource-access-grant ref))
     (should-not (qq-resource-access-grant opening))
     (let ((replacement (qq-resource-acquire-access "res-pooled" nil #'ignore)))
       (funcall (nth 1 old-open)
                (qq-resource-test-access path (+ now 3600) nil "res-opening"))
       (funcall (nth 1 (car renewals))
                (qq-resource-test-access path (+ now 3600)))
       (should-not (qq-resource-access-grant replacement))
       (should (= (hash-table-count qq-resource--access-pool) 1))
       (should (= (length invalidations) 2))
       (should (= (length closes) 3))))))

(ert-deftest qq-resource-pool-callback-error-detaches-only-its-consumer ()
  (qq-resource-test-with-access
   (let* ((first (qq-resource-acquire-access
                  "res-pooled" (lambda (&rest _) (error "consumer failed")) #'ignore))
          (second (qq-resource-access-retain first nil #'ignore)))
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (should-not (qq-resource-access-grant first))
     (should (qq-resource-access-grant second))
     (should-not closes)
     (qq-resource-access-release second)
     (should (= (length closes) 1)))))

(ert-deftest qq-resource-url-and-preview-use-gateway-resource-snapshots ()
  (qq-resource-test-with-state
   (let (calls delivered failure)
     (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
               ((symbol-function 'qq-server-capabilities)
                (lambda () qq-resource-test-capabilities))
               ((symbol-function 'qq-server-send)
                (lambda (method params callback _errback &optional _early)
                  (push (list method params) calls)
                  (funcall
                   callback
                   `((resource
                      . ,(qq-resource-test-snapshot
                          :resource-id (if (equal method "resource.acquire_url")
                                           "res-url" "res-preview")))))
                  method)))
       (qq-resource-acquire-url
        "https://example.org/avatar.png" "avatar.png"
        (lambda (snapshot) (push snapshot delivered)))
       (qq-resource-derive-preview
        "res-url" "animation" (lambda (snapshot) (push snapshot delivered)))
       (should (= (length delivered) 2))
       (should (equal (caar calls) "resource.derive_preview"))
       (should (equal (cadar calls)
                      '((source_resource_id . "res-url") (kind . "animation"))))
       (should-not (assq 'path (qq-resource "res-url")))
       (should-not (assq 'path (qq-resource "res-preview")))
       (cl-letf (((symbol-function 'qq-server-send)
                  (lambda (_method _params success _error &optional _early)
                    (funcall success
                             `((resource . ,(qq-resource-test-snapshot
                                             :resource-id "res-url")))))))
         (qq-resource-derive-preview
          "res-url" "video" #'ignore
          (lambda (body _reason) (setq failure (alist-get 'code body)))))
       (should (equal failure "invalid_gateway_result"))))))

(ert-deftest qq-resource-pool-disconnect-and-replacement-instance-revoke-generation ()
  (qq-resource-test-with-access
   (let* (failure
          (old-generation qq-resource--access-generation)
          (ref (qq-resource-acquire-access
                "res-pooled" nil
                (lambda (body _reason) (setq failure body)))))
     (qq-resource--access-handle-state 'reconnecting)
     (should (> qq-resource--access-generation old-generation))
     (should-not (qq-resource-access-grant ref))
     (should (equal (alist-get 'code failure) "resource_access_invalidated"))
     (should (equal invalidations '(("res-pooled" transport-boundary))))
     (cl-letf (((symbol-function 'qq-rpc-method-available-p) (lambda (_) nil)))
       (qq-resource--handle-ready "replacement-instance"))
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (should (= (length closes) 1))
     (should (= (hash-table-count qq-resource--access-pool) 0))
     (should (= (length invalidations) 1)))))

(ert-deftest qq-resource-pool-reuses-reentrant-expiry-replacement ()
  (qq-resource-test-with-access
   (let ((expired (qq-resource-acquire-access "res-pooled" nil #'ignore))
         replacement)
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (add-hook 'qq-resource-access-invalidated-hook
               (lambda (id reason)
                 (when (eq reason 'expired)
                   (setq replacement
                         (qq-resource-acquire-access id nil #'ignore)))))
     (setq now (+ now 3601))
     (let ((second (qq-resource-acquire-access "res-pooled" nil #'ignore)))
       (should-not (qq-resource-access-grant expired))
       (should (= (length opens) 2))
       (funcall (nth 1 (car opens))
                (qq-resource-test-access
                 path (+ now 3600)
                 "access-bbbbbbbb-bbbb-4ccc-8ddd-eeeeeeeeeeee"))
       (should (qq-resource-access-grant replacement))
       (should (qq-resource-access-grant second))
       (let ((public (qq-resource-access-grant second)))
         (setf (alist-get 'path public) "/consumer-mutated")
         (should (equal (alist-get 'path (qq-resource-access-grant replacement))
                        path)))
       (qq-resource-access-release replacement)
       (should (qq-resource-access-grant second))
       (qq-resource-access-release second)))))

(ert-deftest qq-resource-pool-live-p-distinguishes-pending-released-and-expired ()
  (qq-resource-test-with-access
   (let ((pending (qq-resource-acquire-access "res-pooled" nil #'ignore)))
     (should (eq (qq-resource-access-live-p pending) t))
     (should-not (qq-resource-access-grant pending))
     (let ((released (qq-resource-access-retain pending nil #'ignore)))
       (qq-resource-access-release released)
       (should-not (qq-resource-access-live-p released))
       (should (qq-resource-access-live-p pending)))
     (funcall (nth 1 (car opens)) (qq-resource-test-access path (+ now 3600)))
     (should (qq-resource-access-live-p pending))
     (setq now (+ now 3601))
     (should-not (qq-resource-access-live-p pending))
     (should (equal invalidations '(("res-pooled" expired)))))))

(provide 'qq-resource-test)

;;; qq-resource-test.el ends here
