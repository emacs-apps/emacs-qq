;;; qq-login-test.el --- Tests for native QQ login interaction -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-login)

(defun qq-login-test-account
    (account-id phase &optional uin challenge problem)
  "Return one test account ACCOUNT-ID in PHASE."
  `((account_id . ,account-id)
    (label)
    (phase . ,phase)
    (uin . ,uin)
    (uid)
    (challenge . ,challenge)
    (problem . ,problem)))

(defmacro qq-login-test-with-state (&rest body)
  "Run BODY with an isolated login interaction and account registry."
  (declare (indent 0) (debug t))
  `(let ((qq-login--current nil)
         (qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--gateway-instance-id nil)
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-login-change-hook nil))
     (unwind-protect
         (progn ,@body)
       (when (qq-login-active-p)
         (qq-login-cancel)))))

(defun qq-login-test-session (&optional account-id create-p label label-read-p)
  "Install and return one active login session."
  (setq qq-login--current
        (qq-login--session-create
         :active-p t
         :account-id account-id
         :create-p create-p
         :label label
         :label-read-p label-read-p
         :managed-accounts-loaded-p t
         :login-accounts-loaded-p nil
         :login-accounts nil
         :quick-login-uin nil
         :quick-login-suppressed-p nil
         :retry-failed-p t
         :status "Preparing QQ login…")))

(defun qq-login-test-quick-account (uin uid generated-at)
  "Return completion-safe EasyLogin metadata."
  `((uin . ,uin)
    (uid . ,uid)
    (generated_at_unix . ,generated-at)))

(defun qq-login-test-captcha-challenge (&optional challenge-id sid)
  "Return one closed captcha challenge with CHALLENGE-ID and SID."
  (let ((sid (or sid "123456789")))
    `((kind . "captcha")
      (challenge_id . ,(or challenge-id "captcha-challenge"))
      (url . ,(format
               (concat
                "https://ti.qq.com/safe/tools/captcha/sms-verify-login"
                "?aid=2086582797&sid=%s&uin=10001")
               sid))
      (sid . ,sid))))

(defun qq-login-test-captcha-document (url proof)
  "Return a browser-session document for URL containing PROOF."
  `((schema . 1)
    (source . ((browser . "chrome") (url . ,url)))
    (cookies)
    (page . ,proof)))

(ert-deftest qq-login-chooser-lists-quick-identities-and-new-account ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-b" "stopped" "10002") 'test)
    (let ((session (qq-login-test-session))
          choices)
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (method)
                   (member method
                           '("account.login.list"
                             "account.login.quick"))))
                ((symbol-function 'qq-account-login-list)
                 (lambda (success _failure)
                   (funcall
                    success
                    (list
                     (qq-login-test-quick-account
                      "10002" "u_newer" 1784700001)
                     (qq-login-test-quick-account
                      "10001" "u_older" 1784700000)))
                   "quick-list-request"))
                ((symbol-function 'qq-login--schedule) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq choices collection)
                   "10002 — Quick login")))
        (should-not (qq-login--resolve-account-choice session))
        (should
         (qq-login--session-login-accounts-loaded-p session))
        (should (qq-login--resolve-account-choice session)))
      (should
       (equal (mapcar #'car choices)
              '("10002 — Quick login"
                "10001 — Quick login"
                "Add QQ account")))
      (should
       (equal (qq-login--session-account-id session) "slot-b"))
      (should
       (equal (qq-login--session-quick-login-uin session) "10002"))
      (should-not (qq-login--session-create-p session))
      (should (equal (qq-account-current-id) "slot-b")))))

(ert-deftest qq-login-direct-bound-account-prefers-its-login-record ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "stopped" "10001") 'test)
    (let ((session (qq-login-test-session "slot-a"))
          requested)
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (method)
                   (member method
                           '("account.login.list"
                             "account.login.quick"))))
                ((symbol-function 'qq-account-login-list)
                 (lambda (success _failure)
                   (setq requested t)
                   (funcall
                    success
                    (list
                     (qq-login-test-quick-account
                      "10001" "u_self" 1784700001)))
                   "quick-list-request"))
                ((symbol-function 'qq-login--schedule) #'ignore))
        (should-not (qq-login--resolve-account-choice session))
        (should requested)
        (should (qq-login--resolve-account-choice session)))
      (should
       (equal (qq-login--session-quick-login-uin session) "10001"))
      (should-not
       (qq-login--session-quick-login-suppressed-p session)))))

(ert-deftest qq-login-refreshes-managed-accounts-before-opening-chooser ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session))
          choices
          refresh-reason)
      (setf (qq-login--session-managed-accounts-loaded-p session) nil)
      (cl-letf (((symbol-function 'qq-account-refresh-accounts)
                 (lambda (success _failure reason)
                   (setq refresh-reason reason)
                   (let ((accounts
                          (list
                           (qq-login-test-account
                            "slot-remote" "online" "10001"))))
                     (qq-account--replace-accounts
                      accounts 'login "gateway-test")
                     (funcall success accounts))
                   "managed-list-request"))
                ((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) nil))
                ((symbol-function 'qq-login--schedule) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq choices collection)
                   "10001 — Online")))
        (should-not (qq-login--resolve-account-choice session))
        (should
         (qq-login--session-managed-accounts-loaded-p session))
        (should (eq refresh-reason 'login))
        (should (qq-login--resolve-account-choice session)))
      (should
       (equal (mapcar #'car choices)
              '("10001 — Online" "Add QQ account")))
      (should
       (equal (qq-login--session-account-id session) "slot-remote")))))

(ert-deftest qq-login-new-account-is-a-choice-alongside-easylogin ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session)))
      (setf
       (qq-login--session-login-accounts-loaded-p session) t
       (qq-login--session-login-accounts session)
       (list
        (qq-login-test-quick-account
         "10001" "u_stored" 1784700000)))
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) t))
                ((symbol-function 'completing-read)
                 (lambda (&rest _arguments) "Add QQ account")))
        (should (qq-login--resolve-account-choice session)))
      (should (qq-login--session-create-p session))
      (should-not (qq-login--session-account-id session))
      (should-not (qq-login--session-quick-login-uin session))
      (should-not (qq-login--session-label-read-p session)))))

(ert-deftest qq-login-chooser-does-not-depend-on-easylogin-capabilities ()
  (qq-login-test-with-state
    (qq-account--replace-accounts
     (list
      (qq-login-test-account "slot-online" "online" "10001")
      (qq-login-test-account "slot-login" "logging_in"))
     'test "gateway-test")
    (let ((session (qq-login-test-session))
          choices)
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) nil))
                ((symbol-function 'qq-account-login-list)
                 (lambda (&rest _arguments)
                   (ert-fail "unadvertised EasyLogin list was requested")))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq choices collection)
                   "10001 — Online")))
        (should (qq-login--resolve-account-choice session)))
      (should
       (equal (mapcar #'car choices)
              '("10001 — Online"
                "Unbound account — Logging In"
                "Add QQ account")))
      (should
       (qq-login--session-login-accounts-loaded-p session))
      (should-not (qq-login--session-login-accounts session))
      (should
       (equal (qq-login--session-account-id session) "slot-online"))
      (should (equal (qq-account-current-id) "slot-online")))))

(ert-deftest qq-login-empty-registry-still-opens-new-account-chooser ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session))
          choices)
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) nil))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq choices collection)
                   "Add QQ account")))
        (should (qq-login--resolve-account-choice session)))
       (should (equal (mapcar #'car choices) '("Add QQ account")))
      (should (qq-login--session-create-p session))
      ;; The label belongs to the explicitly selected new-account branch.  It
      ;; must never replace the account selector itself.
      (should-not (qq-login--session-label-read-p session)))))

(ert-deftest qq-login-chooser-disambiguates-duplicate-managed-labels ()
  (qq-login-test-with-state
    (qq-account--replace-accounts
     (list
      (qq-login-test-account "slot-a" "stopped")
      (qq-login-test-account "slot-b" "stopped"))
     'test "gateway-test")
    (let ((session (qq-login-test-session))
          choices)
      (setf (qq-login--session-login-accounts-loaded-p session) t)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq choices collection)
                   "Unbound account — Stopped [slot-b]")))
        (should (qq-login--resolve-account-choice session)))
      (should
       (equal (mapcar #'car choices)
              '("Unbound account — Stopped [slot-a]"
                "Unbound account — Stopped [slot-b]"
                "Add QQ account")))
      (should
       (equal (qq-login--session-account-id session) "slot-b")))))

(ert-deftest qq-login-easylogin-list-failure-falls-back-to-account-chooser ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "stopped" "10001") 'test)
    (let ((session (qq-login-test-session))
          scheduled)
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) t))
                ((symbol-function 'qq-account-login-list)
                 (lambda (_success failure)
                   (funcall failure
                            '((code . "credential_store_failed"))
                            "credential catalog unavailable")
                   "quick-list-request"))
                ((symbol-function 'qq-login--schedule)
                 (lambda (_session) (setq scheduled t))))
        (should-not (qq-login--resolve-account-choice session)))
      (should scheduled)
      (should
       (qq-login--session-login-accounts-loaded-p session))
      (should-not (qq-login--session-login-accounts session))
      (should-not (qq-login--session-in-flight-p session))
      (should
       (equal (qq-login--session-status session)
              "Choose a QQ account.")))))

(ert-deftest qq-login-quick-choice-creates-an-unlabeled-slot-when-needed ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session)))
      (setf
       (qq-login--session-login-accounts-loaded-p session) t
       (qq-login--session-login-accounts session)
       (list
        (qq-login-test-quick-account
         "10001" "u_stored" 1784700000)))
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) t))
                ((symbol-function 'completing-read)
                 (lambda (&rest _arguments) "10001 — Quick login")))
        (should (qq-login--resolve-account-choice session)))
      (should (qq-login--session-create-p session))
      (should-not (qq-login--session-account-id session))
      (should (qq-login--session-label-read-p session))
      (should
       (equal (qq-login--session-quick-login-uin session) "10001")))))

(ert-deftest qq-login-quick-choice-reuses-the-selected-unbound-slot ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "login_required") 'test)
    (let ((session (qq-login-test-session)))
      (setf
       (qq-login--session-login-accounts-loaded-p session) t
       (qq-login--session-login-accounts session)
       (list
        (qq-login-test-quick-account
         "10001" "u_stored" 1784700000)))
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) t))
                ((symbol-function 'completing-read)
                 (lambda (&rest _arguments) "10001 — Quick login")))
        (should (qq-login--resolve-account-choice session)))
      (should-not (qq-login--session-create-p session))
      (should
       (equal (qq-login--session-account-id session) "slot-a"))
      (should
       (equal (qq-login--session-quick-login-uin session) "10001")))))

(ert-deftest qq-login-quick-choice-prefers-bound-slot-over-selected-empty-slot ()
  (qq-login-test-with-state
    (qq-account--replace-accounts
     (list
      (qq-login-test-account "slot-empty" "stopped")
      (qq-login-test-account "slot-bound" "stopped" "10001"))
     'test "gateway-test")
    (qq-account--set-current-account "slot-empty")
    (let ((session (qq-login-test-session)))
      (setf
       (qq-login--session-login-accounts-loaded-p session) t
       (qq-login--session-login-accounts session)
       (list
        (qq-login-test-quick-account
         "10001" "u_stored" 1784700000)))
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) t))
                ((symbol-function 'completing-read)
                 (lambda (&rest _arguments) "10001 — Quick login")))
        (should (qq-login--resolve-account-choice session)))
      (should
       (equal (qq-login--session-account-id session) "slot-bound"))
      (should
       (equal (qq-account-current-id) "slot-bound")))))

(ert-deftest qq-login-active-runtime-remains-an-explicit-chooser-entry ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "logging_in" "10001") 'test)
    (let ((session (qq-login-test-session))
          choices)
      (setf (qq-login--session-login-accounts-loaded-p session) t)
      (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt collection &rest _arguments)
                   (setq choices collection)
                   "10001 — Logging In")))
        (should (qq-login--resolve-account-choice session)))
      (should
       (equal (mapcar #'car choices)
              '("10001 — Logging In" "Add QQ account")))
      (should
       (equal (qq-login--session-account-id session) "slot-a")))))

(ert-deftest qq-login-chooser-shows-unselected-online-account-as-online ()
  (qq-login-test-with-state
    (qq-account--replace-accounts
     (list
      (qq-login-test-account "slot-online" "online" "10001")
      (qq-login-test-account "slot-stopped" "stopped" "10002"))
     'test "gateway-test")
    (let ((session (qq-login-test-session))
          choices)
      (setf
       (qq-login--session-login-accounts-loaded-p session) t
       (qq-login--session-login-accounts session)
       (list
        (qq-login-test-quick-account "10001" "uid-1" 1784700001)
        (qq-login-test-quick-account "10002" "uid-2" 1784700000)))
      (cl-letf (((symbol-function 'qq-rpc-method-available-p)
                 (lambda (_method) t))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (setq choices collection)
                   "10001 — Online")))
        (should (qq-login--resolve-account-choice session)))
      (should
       (equal (mapcar #'car choices)
              '("10001 — Online"
                "10002 — Quick login"
                "Add QQ account")))
      (should
       (equal (qq-login--session-account-id session) "slot-online"))
      (should-not (qq-login--session-quick-login-uin session))
      (should-not (qq-login--session-create-p session))
      (should
       (equal (qq-account-current-id) "slot-online")))))

(ert-deftest qq-login-starts-a-stopped-account ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "stopped") 'test)
    (let ((session (qq-login-test-session "slot-a"))
          started)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule)
                 #'ignore)
                ((symbol-function 'qq-account-start)
                 (lambda (account-id success _failure)
                   (setq started account-id)
                   (funcall success
                            (qq-login-test-account
                             account-id "login_required")))))
        (qq-login--drive session))
      (should (equal started "slot-a"))
      (should-not (qq-login--session-in-flight-p session))
      (should (qq-login-active-p)))))

(ert-deftest qq-login-login-required-uses-selected-easylogin-identity ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "login_required" "10001") 'test)
    (let ((session (qq-login-test-session "slot-a"))
          sent)
      (setf (qq-login--session-quick-login-uin session) "10001")
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule) #'ignore)
                ((symbol-function 'read-passwd)
                 (lambda (&rest _arguments)
                   (ert-fail "EasyLogin prompted for a password")))
                ((symbol-function 'qq-account-login-quick)
                 (lambda (account-id uin qimei success _failure)
                   (setq sent (list account-id uin qimei))
                   (funcall
                    success
                    (qq-login-test-account
                     account-id "logging_in" uin))
                   "quick-login-request")))
        (qq-login--drive session))
      (should (equal sent '("slot-a" "10001" nil)))
      (should-not (qq-login--session-in-flight-p session))
      (should (qq-login-active-p)))))

(ert-deftest qq-login-easylogin-failure-falls-back-to-password-on-retry ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "login_required" "10001") 'test)
    (let ((session (qq-login-test-session "slot-a")))
      (setf (qq-login--session-quick-login-uin session) "10001")
      (cl-letf (((symbol-function 'qq-account-login-quick)
                 (lambda (_account-id _uin _qimei _success failure)
                   (funcall
                    failure
                    '((code . "quick_login_record_not_found"))
                    "stored record disappeared")
                   "quick-login-request")))
        (qq-login--quick
         session (qq-account-get "slot-a")))
      (should-not (qq-login--session-quick-login-uin session))
      (should (qq-login--session-quick-login-suppressed-p session))
      (should
       (string-match-p
        "quick_login_record_not_found"
        (qq-login--session-error session))))))

(ert-deftest qq-login-password-preserves-exact-uin-and-clears-input-secret ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "login_required") 'test)
    (let ((session (qq-login-test-session "slot-a"))
          (password (copy-sequence "correct horse"))
          sent)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule)
                 #'ignore)
                ((symbol-function 'read-string)
                 (lambda (&rest _arguments) "9007199254740993"))
                ((symbol-function 'read-passwd)
                 (lambda (&rest _arguments) password))
                ((symbol-function 'qq-account-login-password)
                 (lambda (account-id uin secret qimei success _failure)
                   (setq sent
                         (list account-id uin (copy-sequence secret) qimei))
                   (funcall success
                            (qq-login-test-account
                             account-id "logging_in" uin)))))
        (qq-login--drive session))
      (should
       (equal sent
              '("slot-a" "9007199254740993" "correct horse" nil)))
      (should (seq-every-p (lambda (character) (= character 0)) password))
      (should-not (qq-login--session-in-flight-p session)))))

(ert-deftest qq-login-captcha-captures-and-submits-closed-proof ()
  (qq-login-test-with-state
    (let* ((challenge (qq-login-test-captcha-challenge))
           (url (alist-get 'url challenge))
           (account
            (qq-login-test-account
             "slot-a" "logging_in" "10001" challenge))
           (session (qq-login-test-session "slot-a"))
           (request (browser-session--request-create))
           (ticket (copy-sequence "synthetic-ticket"))
           capture-arguments
           sent)
      (qq-account--upsert-account account 'test)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-login--schedule) #'ignore)
                ((symbol-function 'read-passwd)
                 (lambda (&rest _arguments)
                   (ert-fail "captcha requested manual secret input")))
                ((symbol-function 'browser-session-capture)
                 (lambda (&rest arguments)
                   (setq capture-arguments arguments)
                   request))
                ((symbol-function 'browser-session-read)
                 (lambda (file)
                   (should (equal file
                                  (plist-get capture-arguments :output-file)))
                   (qq-login-test-captcha-document
                    url
                    `((ticket . ,ticket)
                      (randstr . "@test")
                      (sid . "123456789")))))
                ((symbol-function 'qq-account-login-captcha)
                 (lambda
                     (account-id challenge-id proof rand-str sid
                                 success _failure)
                   (setq sent
                         (list account-id challenge-id
                               (copy-sequence proof) rand-str sid))
                   (funcall success account))))
        (qq-login--drive session)
        (should (eq request
                    (qq-login--captcha-capture-request
                     (qq-login--session-captcha-capture session))))
        (should (equal (plist-get capture-arguments :url) url))
        (should (equal (plist-get capture-arguments :script)
                       qq-login--captcha-script))
        (should-not (plist-member capture-arguments :script-file))
        (should (string-suffix-p
                 "/profile"
                 (plist-get capture-arguments :profile-directory)))
        (should-not (plist-member capture-arguments :cookies))
        (should-not sent)
        (funcall (plist-get capture-arguments :callback) 'metadata)
        (should
         (equal sent
                '("slot-a" "captcha-challenge"
                  "synthetic-ticket" "@test" "123456789")))
        (should-not (qq-login--session-captcha-capture session))
        (should-not (qq-login--session-in-flight-p session))
        (should (seq-every-p (lambda (character) (= character 0))
                             ticket))))))

(ert-deftest qq-login-captcha-rejects-sid-conflict-before-opening-browser ()
  (qq-login-test-with-state
    (let* ((challenge (qq-login-test-captcha-challenge))
           (session (qq-login-test-session "slot-a"))
           (account
            (qq-login-test-account
             "slot-a" "logging_in" "10001" challenge)))
      (setf (alist-get 'sid challenge) "conflicting-sid")
      (cl-letf (((symbol-function 'browser-session-capture)
                 (lambda (&rest _arguments)
                   (ert-fail "conflicting captcha opened a browser"))))
        (should-error
         (qq-login--captcha session account challenge)
         :type 'user-error)
        (should-not (qq-login--session-captcha-capture session))))))

(ert-deftest qq-login-captcha-rejects-malformed-proof-without-diagnosing-it ()
  (qq-login-test-with-state
    (let* ((challenge (qq-login-test-captcha-challenge))
           (url (alist-get 'url challenge))
           (account
            (qq-login-test-account
             "slot-a" "logging_in" "10001" challenge))
           (session (qq-login-test-session "slot-a"))
           capture-arguments
           messages
           directory)
      (qq-account--upsert-account account 'test)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-login--schedule) #'ignore)
                ((symbol-function 'browser-session-capture)
                 (lambda (&rest arguments)
                   (setq capture-arguments arguments)
                   (browser-session--request-create)))
                ((symbol-function 'browser-session-read)
                 (lambda (_file)
                   (qq-login-test-captcha-document
                    url
                    '((ticket . "SECRET-PROOF")
                      (randstr . "@test")
                      (sid . "wrong-sid")))))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format format-string arguments)
                         messages))))
        (qq-login--drive session)
        (setq directory
              (qq-login--captcha-capture-directory
               (qq-login--session-captcha-capture session)))
        (funcall (plist-get capture-arguments :callback) 'metadata)
        (should-not (qq-login--session-captcha-capture session))
        (should-not (file-exists-p directory))
        (should
         (equal (qq-login--session-error session)
                "qq: browser returned an invalid captcha proof"))
        (should-not
         (seq-some
          (lambda (text) (string-match-p "SECRET-PROOF" text))
          messages))))))

(ert-deftest qq-login-captcha-cancel-waits-for-helper-cleanup ()
  (qq-login-test-with-state
    (let* ((challenge (qq-login-test-captcha-challenge))
           (account
            (qq-login-test-account
             "slot-a" "logging_in" "10001" challenge))
           (session (qq-login-test-session "slot-a"))
           capture-arguments
           cancelled
           directory)
      (qq-account--upsert-account account 'test)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-login--schedule) #'ignore)
                ((symbol-function 'browser-session-capture)
                 (lambda (&rest arguments)
                   (setq capture-arguments arguments)
                   (browser-session--request-create)))
                ((symbol-function 'browser-session-cancel)
                 (lambda (request)
                   (setq cancelled request)
                   t)))
        (qq-login--drive session)
        (let ((capture (qq-login--session-captcha-capture session)))
          (setq directory
                (qq-login--captcha-capture-directory capture))
          (should (file-directory-p directory))
          (qq-login-cancel)
          (should cancelled)
          (should (file-directory-p directory))
          (funcall
           (plist-get capture-arguments :errorback)
           '((code . "cancelled")
             (message . "browser session capture was cancelled")))
          (should-not (file-exists-p directory)))))))

(ert-deftest qq-login-captcha-stale-callback-never-submits-proof ()
  (qq-login-test-with-state
    (let* ((challenge (qq-login-test-captcha-challenge "old-challenge"))
           (url (alist-get 'url challenge))
           (account
            (qq-login-test-account
             "slot-a" "logging_in" "10001" challenge))
           (session (qq-login-test-session "slot-a"))
           capture-arguments
           cancelled
           submitted)
      (qq-account--upsert-account account 'test)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-login--schedule) #'ignore)
                ((symbol-function 'browser-session-capture)
                 (lambda (&rest arguments)
                   (setq capture-arguments arguments)
                   (browser-session--request-create)))
                ((symbol-function 'browser-session-cancel)
                 (lambda (_request) (setq cancelled t)))
                ((symbol-function 'qq-login--prepare-qr) #'ignore)
                ((symbol-function 'browser-session-read)
                 (lambda (_file)
                   (qq-login-test-captcha-document
                    url
                    '((ticket . "stale-ticket")
                      (randstr . "@old")
                      (sid . "123456789")))))
                ((symbol-function 'qq-account-login-captcha)
                 (lambda (&rest _arguments) (setq submitted t))))
        (qq-login--drive session)
        (let* ((replacement
                '((kind . "new_device")
                  (challenge_id . "new-challenge")
                  (qr_url . "https://example.invalid/scan")))
               (replacement-account
                (qq-login-test-account
                 "slot-a" "logging_in" "10001" replacement)))
          (qq-account--upsert-account replacement-account 'test)
          (qq-login--drive session))
        (should cancelled)
        (funcall (plist-get capture-arguments :callback) 'metadata)
        (should-not submitted)))))

(ert-deftest qq-login-new-device-only-projects-the-rust-owned-qr ()
  (qq-login-test-with-state
    (let* ((challenge
            '((kind . "new_device")
              (challenge_id . "challenge-7")
              (qr_url . "https://example.invalid/scan")))
           (account
            (qq-login-test-account
             "slot-a" "logging_in" "10001" challenge))
           (session (qq-login-test-session "slot-a"))
           rendered)
      (qq-account--upsert-account account 'test)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule)
                 #'ignore)
                ((symbol-function 'qq-login--prepare-qr)
                 (lambda (_session url) (setq rendered url)))
                ((symbol-function 'qq-rpc-call)
                 (lambda (&rest _arguments)
                   (ert-fail "QR projection sent an RPC"))))
        (qq-login--drive session))
      (should (equal rendered "https://example.invalid/scan"))
      (should-not (qq-login--session-in-flight-p session)))))

(ert-deftest qq-login-new-device-error-keeps-the-qr-visible ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session "slot-a")))
      (setf (qq-login--session-in-flight-p session) t
            (qq-login--session-qr-url session)
            "https://example.invalid/scan"
            (qq-login--session-qr-display session) "[QR]\n")
      (qq-login--request-error
       session
       '((code . "login_pipeline_failed"))
       "OIDB polling failed")
      (let ((model (qq-login-view-model)))
        (should (qq-login-active-p))
        (should-not (qq-login--session-in-flight-p session))
        (should (equal (plist-get model :display) "[QR]\n"))
        (should
         (equal (plist-get model :error)
                "Login failed [login_pipeline_failed]: OIDB polling failed"))))))

(ert-deftest qq-login-reentry-does-not-replace-an-in-flight-session ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session "slot-a")))
      (setf (qq-login--session-in-flight-p session) t
            (qq-login--session-handled-challenge-id session) "challenge-7"
            (qq-login--session-qr-display session) "[QR]\n"
            (qq-login--session-status session)
            "Waiting for mobile QQ confirmation…")
      (should (eq (qq-login) session))
      (should (eq qq-login--current session))
      (should (qq-login--session-in-flight-p session))
      (should
       (equal (qq-login--session-handled-challenge-id session)
              "challenge-7"))
      (should (equal (plist-get (qq-login-view-model) :display) "[QR]\n")))))

(ert-deftest qq-login-does-not-automatically-restart-a-failed-attempt ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account
      "slot-a" "failed" "10001" nil
      '((code . "login_pipeline_failed")
        (message . "new-device polling failed")))
     'test)
    (let ((session (qq-login-test-session "slot-a")))
      (setf (qq-login--session-retry-failed-p session) nil)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-account-start)
                 (lambda (&rest _arguments)
                   (ert-fail "failed login restarted without user action"))))
        (qq-login--drive session))
      (should (qq-login-active-p))
      (should
       (equal
        (qq-login--session-error session)
        "[login_pipeline_failed] new-device polling failed")))))

(ert-deftest qq-login-terminal-qr-rendering-never-opens-the-url ()
  (let (arguments)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) nil))
              ((symbol-function 'executable-find)
               (lambda (_program) "/test/qrencode"))
              ((symbol-function 'call-process)
               (lambda (_program _infile destination _display &rest args)
                 (setq arguments args)
                 (when (eq destination t)
                   (insert "[terminal QR]\n"))
                 0)))
      (pcase-let ((`(,display . ,file)
                   (qq-login--render-qr "https://example.invalid/scan")))
        (should (equal display "[terminal QR]\n"))
        (should-not file)
        (should (equal arguments
                       '("-m" "1" "-t" "UTF8"
                         "https://example.invalid/scan")))))))

(ert-deftest qq-login-new-account-remembers-an-empty-label-choice ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session nil t nil t))
          created-label)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule)
                 #'ignore)
                ((symbol-function 'qq-login--read-label)
                 (lambda () (ert-fail "label was read twice")))
                ((symbol-function 'qq-account-create)
                 (lambda (label success _failure)
                   (setq created-label label)
                   (let ((snapshot
                          (qq-login-test-account
                           "slot-new" "stopped")))
                     (qq-account--upsert-account snapshot 'response)
                     (funcall success snapshot)))))
        (qq-login--drive session))
      (should-not created-label)
      (should (equal (qq-login--session-account-id session) "slot-new"))
      (should-not (qq-login--session-create-p session))
      (should (equal (qq-account-current-id) "slot-new")))))

(ert-deftest qq-login-finishes-when-account-is-online ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "online" "10001") 'test)
    (let ((session (qq-login-test-session "slot-a")))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t)))
        (qq-login--drive session))
      (should-not (qq-login--session-active-p session))
      (should-not qq-login--current))))

(ert-deftest qq-login-explicit-online-account-does-not-project-login-view ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "online" "10001") 'test)
    (cl-letf (((symbol-function 'qq-server-ready-p)
               (lambda () t))
              ((symbol-function 'qq-core-connect)
               (lambda ()
                 (ert-fail "online account reconnected the Gateway")))
              ((symbol-function 'qq-login--changed)
               (lambda ()
                 (ert-fail "online account projected a login view"))))
      (should-not (qq-login "slot-a")))
    (should-not (qq-login-active-p))
    (should-not qq-login--current)))

(ert-deftest qq-login-generic-entry-selects-even-the-current-online-account ()
  (qq-login-test-with-state
    (qq-account--upsert-account
     (qq-login-test-account "slot-a" "online" "10001") 'test)
    (let (scheduled)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-core-running-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule)
                 (lambda (_session) (setq scheduled t))))
        (let ((session (qq-login)))
          (should (qq-login--session-p session))
          (should (qq-login--session-active-p session))
          (should-not (qq-login--session-account-id session))))
      (should scheduled))))

(provide 'qq-login-test)

;;; qq-login-test.el ends here
