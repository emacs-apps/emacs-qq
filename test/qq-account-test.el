;;; qq-account-test.el --- Tests for QQ account client -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-account)

(defconst qq-account-test-capabilities
  '("device.reset"
    "account.list" "account.create" "account.status" "account.set_presence"
    "account.list_online_devices"
    "account.start"
    "account.login.list" "account.login.password" "account.login.quick"
    "account.login.captcha"
    "account.login.unusual_device"
    "account.stop" "account.logout" "account.remove"))

(defun qq-account-test-account
    (account-id &optional phase uin label challenge problem)
  "Return a closed test snapshot for ACCOUNT-ID."
  `((account_id . ,account-id)
    (label . ,label)
    (phase . ,(or phase "stopped"))
    (uin . ,uin)
    (uid)
    (challenge . ,challenge)
    (problem . ,problem)))

(defmacro qq-account-test-with-state (&rest body)
  "Run BODY with an isolated client-side Gateway registry."
  (declare (indent 0) (debug t))
  `(let ((qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--gateway-instance-id nil)
         (qq-account--refresh-owner nil)
         (qq-account--resync-request-id nil)
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-account-registry-ready-hook nil)
         (qq-account-desync-hook nil)
         (qq-account-projection-resync-hook nil))
     ,@body))

(ert-deftest qq-device-reset-is-empty-explicit-and-projects-no-material ()
  (qq-account-test-with-state
    (let (sent-method sent-params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback nil)
                   "device-reset-request")))
        (should
         (equal
          (qq-device-reset (lambda (receipt) (setq delivered receipt)))
          "device-reset-request")))
      (should (equal sent-method "device.reset"))
      (should-not sent-params)
      (should (eq delivered t)))))

(ert-deftest qq-account-uint64-wire-predicate-is-exact-and-bounded ()
  (should (qq-protocol-uint64-decimal-p "1"))
  (should
   (qq-protocol-uint64-decimal-p qq-protocol--max-uint64-decimal))
  (should (qq-protocol-uint64-decimal-p "0" t))
  (dolist (value
           '(nil 0 "0" "00" "01" "-1" "18446744073709551616"))
    (should-not (qq-protocol-uint64-decimal-p value)))
  (dolist (value '("00" "01" "18446744073709551616"))
    (should-not (qq-protocol-uint64-decimal-p value t))))

(ert-deftest qq-account-online-devices-preserve-independent-source-rosters ()
  (qq-account-test-with-state
    (qq-account--upsert-account
     (qq-account-test-account "slot-a" "online" "10001") 'changed)
    (let (sent-method sent-params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (online_clients
                       (state . "observed")
                       (devices
                        ((instance_id . 537376497)
                         (client_type . 7)
                         (kind . "phone")
                         (state . 1)
                         (platform_id . 2)
                         (platform_type . "Android")
                         (new_client_type . 7)
                         (device_name . "Phone"))))
                      (data_line_candidates
                       (state . "observed")
                       (devices
                        ((app_id . 537246243)
                         (instance_id . 537376497)
                         (client_type . 7)
                         (kind . "phone")
                         (platform_id . 2)
                         (device_name . "Phone")
                         (field_11 . 0))))
                      (data_line_peers
                       (state . "observed")
                       (devices
                        ((class . "phone")
                         (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
                         (route_profile . "linuxqq_3_2_31_51102")
                         (candidate_instance_ids . (537376497)))))))
                   "device-list-request")))
        (should
         (equal
          (qq-account-list-online-devices
           "slot-a" (lambda (snapshot) (setq delivered snapshot)))
          "device-list-request")))
      (should (equal sent-method "account.list_online_devices"))
      (should (equal sent-params '((account_id . "slot-a"))))
      (let* ((online (alist-get 'online_clients delivered))
             (candidates (alist-get 'data_line_candidates delivered))
             (peers (alist-get 'data_line_peers delivered))
             (online-device (car (alist-get 'devices online)))
             (candidate (car (alist-get 'devices candidates)))
             (peer (car (alist-get 'devices peers))))
        (should (= (alist-get 'instance_id online-device) 537376497))
        (should (= (alist-get 'instance_id candidate) 537376497))
        (should-not (assq 'data_line_peer_uid candidate))
        (should (equal (alist-get 'class peer) "phone"))
        (should (equal (alist-get 'peer_uid peer)
                       "u_Wcc5rknRRqRO8y5gxMD6sA"))))))

(ert-deftest qq-account-online-devices-reject-open-or-duplicate-snapshots ()
  (let ((client
         '((instance_id . 11)
           (client_type . 7)
           (kind . "phone")
           (state)
           (platform_id . 2)
           (platform_type)
           (new_client_type)
           (device_name)))
        (candidate
         '((app_id . 1001)
           (instance_id . 11)
           (client_type . 7)
           (kind . "phone")
           (platform_id . 2)
           (device_name . "Phone")
           (field_11))))
    (should-error
     (qq-account--project-online-devices
      `((account_id . "slot-a")
        (online_clients
         (state . "observed") (devices ,client ,client))
        (data_line_candidates (state . "unknown"))
        (data_line_peers (state . "unknown")))
      "slot-a"))
    (should-error
     (qq-account--project-online-devices
      `((account_id . "slot-a")
        (online_clients (state . "unknown"))
        (data_line_candidates
         (state . "observed")
         (devices ,(append candidate '((dev_uid . "u_not_on_wire")))))
        (data_line_peers (state . "unknown")))
      "slot-a"))
    (let* ((sparse (copy-tree client))
           (client-cell (assq 'client_type sparse))
           (platform-cell (assq 'platform_type sparse))
           (name-cell (assq 'device_name sparse)))
      (setcdr client-cell nil)
      (setcdr platform-cell "")
      (setcdr name-cell "")
      (should
       (equal
        (alist-get 'devices
                   (alist-get
                    'online_clients
                    (qq-account--project-online-devices
                     `((account_id . "slot-a")
                       (online_clients
                        (state . "observed") (devices ,sparse))
                       (data_line_candidates (state . "unknown"))
                       (data_line_peers (state . "unknown")))
                     "slot-a")))
        (list sparse))))))

(ert-deftest qq-account-online-devices-reject-substituted-dataline-class-route ()
  (dolist (peer
           '(((class . "phone")
              (peer_uid . "u_account_uid")
              (route_profile . "linuxqq_3_2_31_51102")
              (candidate_instance_ids . (11)))
             ((class . "phone")
              (peer_uid . "u_Wcc5rknRRqRO8y5gxMD6sA")
              (route_profile . "future_unverified_build")
              (candidate_instance_ids . (11)))
             ((class . "pad")
              (peer_uid . "u_l7jpPIZxQo0mzJwoEt-SKw")
              (route_profile . "linuxqq_3_2_31_51102")
              (candidate_instance_ids . (11 11)))))
    (should-error
     (qq-account--project-online-devices
      `((account_id . "slot-a")
        (online_clients (state . "unknown"))
        (data_line_candidates (state . "unknown"))
        (data_line_peers (state . "observed") (devices ,peer)))
      "slot-a"))))

(ert-deftest qq-account-registry-preserves-domain-and-owns-accessors ()
  (qq-account-test-with-state
    (let* ((account-id (copy-sequence "slot-a"))
           (label (copy-sequence "Primary"))
           (raw
            (append
             (qq-account-test-account
              account-id "online" "9007199254740993" label)
             '((future_field . t)))))
      (qq-account--replace-accounts (list raw) 'ready "gateway-1")
      (let ((snapshot (qq-account-get "slot-a"))
            (selected (qq-account-current-id)))
        (should (equal (alist-get 'uin snapshot) "9007199254740993"))
        (should (eq (alist-get 'future_field snapshot) t))
        (aset (alist-get 'account_id snapshot) 0 ?X)
        (aset (alist-get 'label snapshot) 0 ?X)
        (aset selected 0 ?X))
      (should (equal (alist-get 'account_id
                                (qq-account-get "slot-a"))
                     "slot-a"))
      (should (equal (alist-get 'label (qq-account-get "slot-a"))
                     "Primary"))
      (should (equal (qq-account-current-id) "slot-a"))
      (should (equal account-id "slot-a"))
      (should (equal label "Primary")))))

(ert-deftest qq-account-ready-replaces-registry-and-selects-singleton ()
  (qq-account-test-with-state
    (let (selection changes ready)
      (add-hook 'qq-account-selection-changed-hook
                (lambda (old new) (push (list old new) selection)))
      (add-hook 'qq-account-registry-changed-hook
                (lambda (reason account-id)
                  (push (list reason account-id) changes)))
      (add-hook 'qq-account-registry-ready-hook
                (lambda (instance-id)
                  (setq ready
                        (list instance-id
                              (qq-account-current-id)
                              (length (qq-account-list))))))
      (qq-account--handle-event
       "gateway.ready"
       `((gateway_instance_id . "gateway-1")
         (accounts . (,(qq-account-test-account "slot-a")))))
      (should (equal (qq-account-current-id) "slot-a"))
      (should (equal selection '((nil "slot-a"))))
      (should (equal changes '((ready nil))))
      (should (equal ready '("gateway-1" "slot-a" 1)))
      (should (= (length (qq-account-list)) 1)))))

(ert-deftest qq-account-ready-does-not-guess-between-multiple-accounts ()
  (qq-account-test-with-state
    (qq-account--replace-accounts
     (list (qq-account-test-account "slot-a")
           (qq-account-test-account "slot-b"))
     'ready "gateway-1")
    (should-not (qq-account-current-id))
    (qq-account-select "slot-b")
    (qq-account--replace-accounts
     (list (qq-account-test-account "slot-a" "login_required")
           (qq-account-test-account "slot-b" "online" "10002"))
     'ready "gateway-1")
    (should (equal (qq-account-current-id) "slot-b"))
    (should (equal (alist-get 'phase (qq-account-current)) "online"))
    (should (equal (alist-get 'uin (qq-account-current)) "10002"))))

(ert-deftest qq-account-ready-cancels-and-settles-pending-account-refresh ()
  (qq-account-test-with-state
    (let (late-success canceled failures)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params success _failure &optional _early)
                   (setq late-success success)
                   'account-refresh-token))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-account-refresh-accounts
         nil (lambda (body _reason) (push body failures)))
        (qq-account--handle-event
         "gateway.ready"
         '((gateway_instance_id . "gateway-ready") (accounts)))
        (should (equal canceled '(account-refresh-token)))
        (should (= (length failures) 1))
        (should (equal (alist-get 'code (car failures))
                       "superseded_request"))
        (should-not qq-account--refresh-owner)
        (funcall late-success
                 `((accounts . [,(qq-account-test-account "late-slot")])))
        (should (= (length failures) 1))
        (should-not (qq-account-get "late-slot"))))))

(ert-deftest qq-account-change-replaces-same-slot-stop-start-snapshots ()
  (qq-account-test-with-state
    (qq-account--upsert-account
     (qq-account-test-account "slot-a" "stopped" "10001")
     'changed)
    (should (equal (qq-account-current-id) "slot-a"))
    (qq-account--upsert-account
     (qq-account-test-account "slot-a" "starting" "10001")
     'changed)
    (should (equal (alist-get 'phase (qq-account-get "slot-a"))
                   "starting"))
    (qq-account--upsert-account
     (qq-account-test-account "slot-a" "stopped" "10001")
     'changed)
    (should (equal (alist-get 'phase (qq-account-get "slot-a"))
                   "stopped"))
    (should (equal (qq-account-current-id) "slot-a"))
    (should (equal qq-account--account-order '("slot-a")))))

(ert-deftest qq-account-removed-clears-selection-without-stopping-peer ()
  (qq-account-test-with-state
    (let (selection)
      (qq-account--replace-accounts
       (list (qq-account-test-account "slot-a")
             (qq-account-test-account "slot-b"))
       'ready "gateway-1")
      (qq-account-select "slot-a")
      (add-hook 'qq-account-selection-changed-hook
                (lambda (old new) (setq selection (list old new))))
      (qq-account--handle-event
       "account.removed" '((account_id . "slot-a")))
      ;; The remaining singleton becomes the local projection; no lifecycle
      ;; command is emitted for it.
      (should (equal (qq-account-current-id) "slot-b"))
      (should (equal selection '("slot-a" "slot-b")))
      (should-not (qq-account-get "slot-a"))
      (should (qq-account-get "slot-b")))))

(ert-deftest qq-account-start-uses-explicit-account-id ()
  (qq-account-test-with-state
    (let (sent-method sent-params callback-result)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            (qq-account-test-account
                             "slot-a" "login_required"))
                   "request-1")))
        (should
         (equal
          (qq-account-start
           "slot-a" (lambda (snapshot) (setq callback-result snapshot)))
          "request-1"))
        (should (equal sent-method "account.start"))
        (should (equal sent-params '((account_id . "slot-a"))))
        (should (equal (alist-get 'phase callback-result) "login_required"))
        (should (equal (alist-get 'phase (qq-account-get "slot-a"))
                       "login_required"))))))

(ert-deftest qq-account-presence-validates-account-and-value ()
  (qq-account-test-with-state
    (qq-account--upsert-account
     (qq-account-test-account "slot-a" "online" "10001") 'changed)
    (let (sent-method sent-params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (presence
                       (kind . "custom")
                       (face_id . 4294967295)
                       (wording . "writing Emacs Lisp"))))
                   "presence-request")))
        (should
         (equal
          (qq-account-set-presence
           "slot-a"
           '((kind . "custom") (face_id . 4294967295)
             (wording . "writing Emacs Lisp"))
           (lambda (receipt) (setq delivered receipt)))
          "presence-request"))
        (should (equal sent-method "account.set_presence"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (presence
             (kind . "custom")
             (face_id . 4294967295)
             (wording . "writing Emacs Lisp")))))
        (should
         (equal delivered
                '((account_id . "slot-a")
                  (presence
                   (kind . "custom")
                   (face_id . 4294967295)
                   (wording . "writing Emacs Lisp")))))
        (should (equal (alist-get 'phase (qq-account-get "slot-a"))
                       "online"))))))

(ert-deftest qq-account-presence-accepts-receipt-after-slot-update ()
  (qq-account-test-with-state
    (qq-account--upsert-account
     (qq-account-test-account "slot-a" "online" "10001") 'changed)
    (let (delivered failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (qq-account--upsert-account
                    (qq-account-test-account
                     "slot-a" "stopped" "10001")
                    'changed)
                   (funcall callback
                            '((account_id . "slot-a")
                              (presence (kind . "away"))))
                   "presence-request")))
        (qq-account-set-presence
         "slot-a" '((kind . "away"))
         (lambda (receipt) (setq delivered receipt))
         (lambda (_body reason) (setq failure reason)))
        (should (equal delivered
                       '((account_id . "slot-a")
                         (presence (kind . "away")))))
        (should-not failure)
        (should (equal (alist-get 'phase (qq-account-get "slot-a"))
                       "stopped"))))))

(ert-deftest qq-account-password-login-copies-secret-and-keeps-uin-string ()
  (qq-account-test-with-state
    (let ((password (copy-sequence "correct horse battery staple"))
          wire)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params _callback _errback &optional _early)
                   (setq wire
                         (qq-server--json-encode
                          `((method . ,method) (params . ,params))))
                   "request-2")))
        (should
         (equal
          (qq-account-login-password
           "slot-a" "9007199254740993" password)
          "request-2"))
        (should (equal password "correct horse battery staple"))
        (let* ((decoded (qq-server--json-decode wire))
               (params (alist-get 'params decoded)))
          (should (equal (alist-get 'account_id params) "slot-a"))
          (should (equal (alist-get 'uin params) "9007199254740993"))
          (should (stringp (alist-get 'uin params)))
          (should (equal (alist-get 'password params)
                         "correct horse battery staple")))))))

(ert-deftest qq-account-quick-login-list-projects-closed-ordered-identities ()
  (let (sent-method sent-params delivered)
    (cl-letf (((symbol-function 'qq-server-ready-p)
               (lambda () t))
              ((symbol-function 'qq-server-capabilities)
               (lambda () qq-account-test-capabilities))
              ((symbol-function 'qq-server-send)
               (lambda (method params callback _errback &optional _early)
                 (setq sent-method method sent-params params)
                 (funcall
                  callback
                  '((accounts
                     . [((uin . "10002")
                         (uid . "u_newer")
                         (generated_at_unix . 1784700001))
                        ((uin . "10001")
                         (uid . "u_older")
                         (generated_at_unix . 1784700000))])))
                 "quick-list-request")))
      (should
       (equal
        (qq-account-login-list
         (lambda (accounts) (setq delivered accounts)))
        "quick-list-request"))
      (should (equal sent-method "account.login.list"))
      (should-not sent-params)
      (should
       (equal
        (mapcar (lambda (account) (alist-get 'uin account)) delivered)
        '("10002" "10001")))
      (aset (alist-get 'uid (car delivered)) 0 ?X)
      (should
       (equal
        (alist-get
         'uid
         (car
          (qq-account--project-quick-login-accounts
           '((accounts
              ((uin . "10002")
               (uid . "u_newer")
               (generated_at_unix . 1784700001)))))))
        "u_newer")))))

(ert-deftest qq-account-quick-login-list-rejects-duplicates-and-open-shapes ()
  (should-error
   (qq-account--project-quick-login-accounts
    '((accounts
       ((uin . "10001") (uid . "u_a") (generated_at_unix . 1))
       ((uin . "10001") (uid . "u_b") (generated_at_unix . 2))))))
  (should-error
   (qq-account--project-quick-login-accounts
    '((accounts
       ((uin . "10001") (uid . "u_a") (generated_at_unix . 1)
        (credential . "must-not-cross-wire")))))))

(ert-deftest qq-account-quick-login-sends-only-slot-identity-and-qimei ()
  (qq-account-test-with-state
    (let (sent-method sent-params delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    (qq-account-test-account
                     "slot-a" "logging_in" "10001"))
                   "quick-login-request")))
        (should
         (equal
          (qq-account-login-quick
           "slot-a" "10001" "qimei-a"
           (lambda (snapshot) (setq delivered snapshot)))
          "quick-login-request"))
        (should (equal sent-method "account.login.quick"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (uin . "10001")
                  (qimei . "qimei-a"))))
        (should (equal (alist-get 'phase delivered) "logging_in"))
        (should
         (equal (alist-get 'phase (qq-account-get "slot-a"))
                "logging_in"))))))

(ert-deftest qq-account-remove-consumes-returned-snapshot ()
  (qq-account-test-with-state
    (qq-account--upsert-account
     (qq-account-test-account "slot-a") 'changed)
    (let (callback-result)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback (qq-account-test-account "slot-a"))
                   "request-4")))
        (qq-account-remove
         "slot-a" (lambda (snapshot) (setq callback-result snapshot)))
        (should (equal (alist-get 'account_id callback-result) "slot-a"))
        (should-not (qq-account-get "slot-a"))
        (should-not (qq-account-current-id))))))

(ert-deftest qq-account-event-stream-lag-triggers-one-registry-resync ()
  (qq-account-test-with-state
    (let ((calls 0) observed)
      (add-hook 'qq-account-desync-hook
                (lambda (body) (setq observed body)))
      (cl-letf (((symbol-function 'qq-account-refresh-accounts)
                 (lambda (&rest _)
                   (cl-incf calls)
                   "resync-1")))
        (qq-account--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed 3")))
        (qq-account--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed 4")))
        (should (= calls 1))
        (should (equal (car qq-account--resync-request-id)
                       'account-resync))
        (should (equal (alist-get 'code observed) "event_stream_lagged"))))))

(ert-deftest qq-account-websocket-lag-resyncs-every-projection ()
  (qq-account-test-with-state
    (let ((calls 0) projections)
      (add-hook 'qq-account-projection-resync-hook
                (lambda (projection _body) (push projection projections)))
      (cl-letf (((symbol-function 'qq-account-refresh-accounts)
                 (lambda (&rest _)
                   (cl-incf calls)
                   "resync-1")))
        (qq-account--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed 3")))
        (should (= calls 1))
        (should (equal (nreverse projections)
                       '("conversation_read_states" "resources"
                         "attachments" "remote_media")))))))

(ert-deftest qq-account-resync-required-accounts-triggers-one-registry-resync ()
  (qq-account-test-with-state
    (let ((calls 0) observed projections)
      (add-hook 'qq-account-desync-hook
                (lambda (body) (setq observed body)))
      (add-hook 'qq-account-projection-resync-hook
                (lambda (projection _body) (push projection projections)))
      (cl-letf (((symbol-function 'qq-account-refresh-accounts)
                 (lambda (&rest _)
                   (cl-incf calls)
                   "resync-1")))
        (qq-account--handle-resync-required
         "runtime.resync_required"
         '((projection . "accounts") (skipped . "3")))
        (should (= calls 1))
        (should (equal (car qq-account--resync-request-id)
                       'account-resync))
        (should-not projections)
        (should (equal (alist-get 'projection observed) "accounts"))))))

(ert-deftest qq-account-resync-required-dispatches-foreign-projections ()
  (qq-account-test-with-state
    (let ((calls 0) observed)
      (add-hook 'qq-account-projection-resync-hook
                (lambda (projection body)
                  (push (cons projection (alist-get 'skipped body))
                        observed)))
      (cl-letf (((symbol-function 'qq-account-refresh-accounts)
                 (lambda (&rest _)
                   (cl-incf calls)
                   "resync-1")))
        (dolist (projection '("conversation_read_states" "resources"
                              "attachments" "remote_media"))
          (qq-account--handle-resync-required
           "runtime.resync_required"
           `((projection . ,projection) (skipped . "1"))))
        (should (= calls 0))
        (should (equal (nreverse observed)
                       '(("conversation_read_states" . "1")
                         ("resources" . "1")
                         ("attachments" . "1")
                         ("remote_media" . "1"))))))))

(ert-deftest qq-account-resync-required-rejects-malformed-events ()
  (qq-account-test-with-state
    (dolist (data '(((projection . "accounts"))
                    ((projection . "unknown") (skipped . "1"))
                    ((projection . "") (skipped . "1"))
                    ((skipped . "1"))
                    ((projection . "accounts") (skipped . "1") (extra . t))))
      (should-error
       (qq-account--handle-resync-required
        "runtime.resync_required" data)))))

(ert-deftest qq-account-synchronous-resync-settlement-does-not-publish-token ()
  (qq-account-test-with-state
    (let ((calls 0))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-server-gateway-instance-id)
                 (lambda () "gateway-1"))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (cl-incf calls)
                   (funcall callback '((accounts . [])))
                   "already-settled")))
        (qq-account--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed")))
        (should (= calls 1))
        (should-not qq-account--resync-request-id)))))

(ert-deftest qq-account-synchronous-preflight-error-clears-resync-marker ()
  (qq-account-test-with-state
    (cl-letf (((symbol-function 'qq-server-ready-p)
               (lambda () nil)))
      (qq-account--handle-protocol-error
       '((code . "event_stream_lagged") (message . "missed")))
      (should-not qq-account--resync-request-id))))

(ert-deftest qq-account-refresh-replaces-registry-authoritatively ()
  (qq-account-test-with-state
    (qq-account--upsert-account
     (qq-account-test-account "old-slot") 'changed)
    (let (result)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-account-test-capabilities))
                ((symbol-function 'qq-server-gateway-instance-id)
                 (lambda () "gateway-2"))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback
                            `((accounts .
                               [,(qq-account-test-account
                                  "new-slot" "online" "10007")])))
                   "request-5")))
        (qq-account-refresh-accounts
         (lambda (accounts) (setq result accounts)))
        (should (= (length result) 1))
        (should-not (qq-account-get "old-slot"))
        (should (qq-account-get "new-slot"))
        (should (equal qq-account--gateway-instance-id "gateway-2"))))))

(ert-deftest qq-account-refresh-publishes-owner-before-synchronous-settlement ()
  (qq-account-test-with-state
    (let (owner-seen callback-value)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-server-gateway-instance-id)
                 (lambda () "gateway-sync"))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq owner-seen qq-account--refresh-owner)
                   (funcall callback
                            `((accounts .
                               [,(qq-account-test-account "slot-sync")])))
                   'opaque-sync-token)))
        (should
         (eq (qq-account-refresh-accounts
              (lambda (accounts) (setq callback-value accounts)))
             'opaque-sync-token))
        (should owner-seen)
        (should-not qq-account--refresh-owner)
        (should (equal (alist-get 'account_id (car callback-value))
                       "slot-sync"))))))

(ert-deftest qq-account-newest-refresh-prevents-out-of-order-registry-rollback ()
  (qq-account-test-with-state
    (let (requests first-callback first-error second-callback)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-server-gateway-instance-id)
                 (lambda () "gateway-order"))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "request-%d" (length requests))))))
        (should
         (eq (qq-account-refresh-accounts
              (lambda (_) (setq first-callback t))
              (lambda (body _failure) (setq first-error body)))
             'request-1))
        (should
         (eq (qq-account-refresh-accounts
              (lambda (_) (setq second-callback t)) #'ignore)
             'request-2))
        (funcall (car (nth 1 requests))
                 `((accounts .
                    [,(qq-account-test-account "new-slot")])))
        (funcall (car (nth 0 requests))
                 `((accounts .
                    [,(qq-account-test-account "old-slot")])))
        (should second-callback)
        (should-not first-callback)
        (should (equal (alist-get 'code first-error)
                       "superseded_request"))
        (should (qq-account-get "new-slot"))
        (should-not (qq-account-get "old-slot"))
        (should-not qq-account--refresh-owner)))))

(ert-deftest qq-account-stale-refresh-error-cannot-clear-newer-owner ()
  (qq-account-test-with-state
    (let (requests stale-error newer-owner)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-server-gateway-instance-id)
                 (lambda () "gateway-errors"))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "error-request-%d" (length requests))))))
        (qq-account-refresh-accounts
         nil (lambda (body _failure) (setq stale-error body)))
        (qq-account-refresh-accounts nil #'ignore)
        (setq newer-owner qq-account--refresh-owner)
        (funcall (cdr (nth 0 requests))
                 '((code . "transport_failure") (message . "old failed"))
                 "old failed")
        (should (eq newer-owner qq-account--refresh-owner))
        (should (equal (alist-get 'code stale-error)
                       "superseded_request"))
        (funcall (car (nth 1 requests)) '((accounts . [])))
        (should-not qq-account--refresh-owner)))))

(ert-deftest qq-account-reentrant-refresh-owns-projection-and-delivery ()
  (qq-account-test-with-state
    (let (first-success launched callbacks first-error)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-server-gateway-instance-id)
                 (lambda () "gateway-reentrant"))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (if first-success
                       (progn
                         (funcall callback
                                  `((accounts .
                                     [,(qq-account-test-account
                                        "inner-slot")])))
                         'inner-request)
                     (setq first-success callback)
                     'outer-request))))
        (add-hook
         'qq-account-registry-changed-hook
         (lambda (reason _account-id)
           (when (and (eq reason 'outer) (not launched))
             (setq launched t)
             (qq-account-refresh-accounts
              (lambda (_) (push 'inner callbacks)) #'ignore 'inner))))
        (qq-account-refresh-accounts
         (lambda (_) (push 'outer callbacks))
         (lambda (body _failure) (setq first-error body))
         'outer)
        (funcall first-success
                 `((accounts .
                    [,(qq-account-test-account "outer-slot")])))
        (should launched)
        (should (equal callbacks '(inner)))
        (should (equal (alist-get 'code first-error)
                       "superseded_request"))
        (should (qq-account-get "inner-slot"))
        (should-not (qq-account-get "outer-slot"))
        (should-not qq-account--refresh-owner)))))

(ert-deftest qq-account-manual-refresh-settles-automatic-resync-owner ()
  (qq-account-test-with-state
    (let (requests marker canceled)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-server-gateway-instance-id)
                 (lambda () "gateway-marker"))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "marker-request-%d" (length requests)))))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-account--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed")))
        (setq marker qq-account--resync-request-id)
        (qq-account-refresh-accounts nil #'ignore 'manual)
        ;; The manual request does not mutate MARKER directly.  Claiming it
        ;; cancels request A, whose own superseded errback eq-clears MARKER.
        (should marker)
        (should-not qq-account--resync-request-id)
        (should (equal canceled '(marker-request-1)))
        (funcall (car (nth 1 requests))
                 `((accounts .
                    [,(qq-account-test-account "manual-slot")])))
        (should-not qq-account--resync-request-id)
        (funcall (car (nth 0 requests))
                 `((accounts .
                    [,(qq-account-test-account "automatic-slot")])))
        (should-not qq-account--resync-request-id)
        (should (qq-account-get "manual-slot"))
        (should-not (qq-account-get "automatic-slot"))))))

(ert-deftest qq-account-lag-replaces-auto-marker-superseded-by-manual-refresh ()
  (qq-account-test-with-state
    (let (requests first-marker second-marker canceled)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-server-gateway-instance-id)
                 (lambda () "gateway-resync-marker"))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "resync-request-%d" (length requests)))))
                ((symbol-function 'qq-server-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-account--handle-protocol-error
         '((code . "event_stream_lagged") (message . "first")))
        (setq first-marker qq-account--resync-request-id)
        (should (qq-rpc-latest-request-p
                 qq-account--refresh-owner))
        ;; A manual authoritative request supersedes the automatic request but
        ;; does not mutate its marker.  The next lag must still be able to
        ;; replace that now-stale marker instead of waiting for request A.
        (qq-account-refresh-accounts nil #'ignore 'manual)
        (should-not qq-account--resync-request-id)
        (should (equal canceled '(resync-request-1)))
        (qq-account--handle-protocol-error
         '((code . "event_stream_lagged") (message . "second")))
        (setq second-marker qq-account--resync-request-id)
        (should-not (eq first-marker second-marker))
        (should second-marker)
        (should (= (length requests) 3))
        (should (equal canceled '(resync-request-2 resync-request-1)))
        (funcall (car (nth 0 requests)) '((accounts . [])))
        (should (eq second-marker qq-account--resync-request-id))
        (funcall (car (nth 1 requests)) '((accounts . [])))
        (should (eq second-marker qq-account--resync-request-id))
        (funcall (car (nth 2 requests)) '((accounts . [])))
        (should-not qq-account--resync-request-id)))))

(ert-deftest qq-account-event-accepts-forward-compatible-domain-fields ()
  (qq-account-test-with-state
    (let (violation)
      (cl-letf (((symbol-function 'qq-server--protocol-violation)
                 (lambda (format-string &rest arguments)
                   (setq violation (apply #'format format-string arguments)))))
        (qq-rpc--handle-transport-event
         "account.changed"
         (append (qq-account-test-account "slot-a")
                 '((future_field . t)))))
      (should-not violation)
      (should (eq (alist-get 'future_field
                             (qq-account-get "slot-a"))
                  t)))))

(provide 'qq-account-test)

;;; qq-account-test.el ends here
