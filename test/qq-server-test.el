;;; qq-server-test.el --- Tests for QQ service transport -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-server)

(defmacro qq-server-test-with-state (&rest body)
  "Run BODY with isolated native Gateway transport state."
  (declare (indent 0) (debug t))
  `(let ((qq-server--ws 'test-websocket)
         (qq-server--connecting nil)
         (qq-server--connection-owner nil)
         (qq-server--stopping nil)
         (qq-server--pending (make-hash-table :test #'equal))
         (qq-server--request-counter 0)
         (qq-server--reconnect-timer nil)
         (qq-server--reconnect-attempt 0)
         (qq-server--ready-timer nil)
         (qq-server--state 'ready)
         (qq-server--hello-instance-id "gateway-1")
         (qq-server--gateway-instance-id "gateway-1")
         (qq-server--capabilities '("account.list"))
         (qq-server--ready-accounts nil)
         (qq-server-event-hook nil)
         (qq-server-protocol-error-hook nil)
         (qq-server-state-hook nil)
         (qq-server-request-timeout nil)
         (qq-server-ready-timeout nil))
     ,@body))

(ert-deftest qq-server-token-file-matches-server-policy ()
  (let ((path (make-temp-file "qq-account-token-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "0123456789abcdef0123456789abcdef\n"))
          (set-file-modes path #o600)
          (let ((qq-server-auth-token-file path))
            (should
             (equal (qq-server--read-auth-token)
                    "0123456789abcdef0123456789abcdef"))))
      (delete-file path))))

(ert-deftest qq-server-token-file-rejects-weak-secret ()
  (let ((path (make-temp-file "qq-account-token-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "too-short\n"))
          (set-file-modes path #o600)
          (let ((qq-server-auth-token-file path))
            (should-error (qq-server--read-auth-token)
                          :type 'user-error)))
      (delete-file path))))

(ert-deftest qq-server-token-file-rejects-public-mode ()
  (skip-unless (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix
                                   darwin cygwin)))
  (let ((path (make-temp-file "qq-account-token-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "0123456789abcdef0123456789abcdef\n"))
          (set-file-modes path #o640)
          (let ((qq-server-auth-token-file path))
            (should-error (qq-server--read-auth-token)
                          :type 'user-error)))
      (delete-file path))))

(ert-deftest qq-server-send-uses-native-envelope ()
  (qq-server-test-with-state
    (let (wire)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (_ws text) (setq wire text))))
        (let* ((id (qq-server-send
                    "account.list" (make-hash-table :test #'equal)))
               (decoded (qq-server--json-decode wire)))
          (should (equal (alist-get 'kind decoded) "request"))
          (should (equal (alist-get 'id decoded) id))
          (should (equal (alist-get 'method decoded) "account.list"))
          (should (equal (alist-get 'params decoded) nil))
          (should-not (assq 'action decoded))
          (should-not (assq 'echo decoded)))))))

(ert-deftest qq-server-request-waits-for-ready ()
  (qq-server-test-with-state
    (let ((qq-server--state 'authenticating)
          reason sent)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (&rest _) (setq sent t))))
        (should-not
         (qq-server-send
          "account.list" nil nil
          (lambda (_body failure) (setq reason failure))))
        (should (equal reason "Gateway handshake is not ready"))
        (should-not sent)
        (should (= 0 (hash-table-count qq-server--pending)))))))

(ert-deftest qq-server-response-completes-exactly-once ()
  (qq-server-test-with-state
    (let (wire result (calls 0))
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (_ws text) (setq wire text))))
        (let ((id (qq-server-send
                   "account.list" nil
                   (lambda (value)
                     (cl-incf calls)
                     (setq result value)))))
          (qq-server--handle-payload
           `((kind . "response") (id . ,id)
             (result . ((accounts . [])))))
          (qq-server--handle-payload
           `((kind . "response") (id . ,id)
             (result . ((accounts . [])))))
          (should wire)
          (should (= calls 1))
          (should (equal (alist-get 'accounts result) []))
          (should-not (gethash id qq-server--pending)))))))

(ert-deftest qq-server-error-completes-request ()
  (qq-server-test-with-state
    (let (failure body)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text) #'ignore))
        (let ((id
               (qq-server-send
                "account.status" '((account_id . "missing")) nil
                (lambda (error-body reason)
                  (setq body error-body failure reason)))))
          (qq-server--handle-payload
           `((kind . "error") (id . ,id)
             (error . ((code . "account_not_found")
                       (message . "unknown account")))))
          (should (equal failure "unknown account"))
          (should (equal (alist-get 'code body) "account_not_found"))
          (should-not (gethash id qq-server--pending)))))))

(ert-deftest qq-server-unsolicited-error-has-separate-hook ()
  (qq-server-test-with-state
    (let (observed)
      (add-hook 'qq-server-protocol-error-hook
                (lambda (body) (setq observed body)))
      (qq-server--handle-payload
       '((kind . "error") (id)
         (error . ((code . "event_stream_lagged")
                   (message . "missed 2 events")))))
      (should (equal (alist-get 'code observed) "event_stream_lagged"))
      (should (= 0 (hash-table-count qq-server--pending))))))

(ert-deftest qq-server-unsolicited-wire-null-id-is-accepted ()
  (qq-server-test-with-state
    (let (observed)
      (add-hook 'qq-server-protocol-error-hook
                (lambda (body) (setq observed body)))
      (qq-server--handle-payload
       (qq-server--json-decode
        "{\"kind\":\"error\",\"id\":null,\"error\":{\"code\":\"event_stream_lagged\",\"message\":\"missed\"}}"))
      (should (equal (alist-get 'code observed) "event_stream_lagged"))
      (should-not (qq-server-wire-null-p observed)))))

(ert-deftest qq-server-hooks-own-deep-copies ()
  (qq-server-test-with-state
    (let* ((source (copy-sequence "account-1"))
           (qq-server-event-hook
            (list (lambda (_event data)
                    (aset (alist-get 'account_id data) 0 ?X))
                  (lambda (_event data)
                    (should (equal (alist-get 'account_id data)
                                   "account-1"))))))
      (qq-server--run-hook
       'qq-server-event-hook
       "account.changed" `((account_id . ,source)))
      (should (equal source "account-1")))))

(ert-deftest qq-server-metadata-accessors-own-strings ()
  (qq-server-test-with-state
    (let* ((instance (copy-sequence "gateway-1"))
           (capability (copy-sequence "account.list"))
           (account-id (copy-sequence "account-1"))
           (qq-server--gateway-instance-id instance)
           (qq-server--capabilities (list capability))
           (qq-server--ready-accounts
            `(((account_id . ,account-id)))))
      (let ((returned-instance
             (qq-server-gateway-instance-id))
            (returned-capabilities
             (qq-server-capabilities))
            (returned-accounts
             (qq-server-ready-accounts)))
        (aset returned-instance 0 ?G)
        (aset (car returned-capabilities) 0 ?A)
        (aset (alist-get 'account_id (car returned-accounts)) 0 ?Q))
      (should (equal instance "gateway-1"))
      (should (equal capability "account.list"))
      (should (equal account-id "account-1")))))

(ert-deftest qq-server-consumer-hook-error-is-isolated ()
  (qq-server-test-with-state
    (let (later-called)
      (add-hook 'qq-server-event-hook
                (lambda (&rest _) (setq later-called t)) 90)
      (add-hook 'qq-server-event-hook
                (lambda (&rest _) (error "broken consumer")) 10)
      (should
       (condition-case nil
           (progn
             (qq-server--handle-payload
              '((kind . "event") (event . "account.changed")
                (data . ((account_id . "a-1")))))
             t)
         (error nil)))
      ;; One broken consumer cannot suppress later consumers or cause a
      ;; reconnect by masquerading as a malformed wire envelope.
      (should later-called)
      (should (eq qq-server--state 'ready)))))

(ert-deftest qq-server-handshake-requires-authoritative-ready ()
  (let ((qq-server--ws nil)
        (qq-server--connecting nil)
        (qq-server--connection-owner nil)
        (qq-server--stopping nil)
        (qq-server--pending (make-hash-table :test #'equal))
        (qq-server--request-counter 0)
        (qq-server--reconnect-timer nil)
        (qq-server--reconnect-attempt 4)
        (qq-server--ready-timer nil)
        (qq-server--state 'stopped)
        (qq-server--hello-instance-id nil)
        (qq-server--gateway-instance-id nil)
        (qq-server--capabilities nil)
        (qq-server--ready-accounts nil)
        (qq-server-event-hook nil)
        (qq-server-protocol-error-hook nil)
        (qq-server-state-hook nil)
        (qq-server-request-timeout nil)
        (qq-server-ready-timeout nil)
        callbacks
        wire
        events)
    (add-hook 'qq-server-event-hook
              (lambda (event data) (push (cons event data) events)))
    (cl-letf (((symbol-function 'qq-server--read-auth-token)
               (lambda ()
                 (copy-sequence "0123456789abcdef0123456789abcdef")))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest arguments)
                 (setq callbacks arguments)
                 'gateway-socket))
              ((symbol-function 'websocket-openp) (lambda (_) t))
              ((symbol-function 'websocket-send-text)
               (lambda (_ws text) (setq wire text))))
      (qq-server-start)
      (should (eq qq-server--state 'connecting))
      (funcall (plist-get callbacks :on-open) 'gateway-socket)
      (should (eq qq-server--state 'authenticating))
      ;; Model a reconnect attempt already counted by the scheduler.  Merely
      ;; opening/authenticating must not erase it.
      (setq qq-server--reconnect-attempt 4)
      (let* ((hello (qq-server--json-decode wire))
             (id (alist-get 'id hello)))
        (should (equal (alist-get 'method hello) "gateway.hello"))
        (should (= (alist-get 'protocol_version (alist-get 'params hello))
                   qq-server-protocol-version))
        (should
         (equal (alist-get 'auth_token (alist-get 'params hello))
                "0123456789abcdef0123456789abcdef"))
        (qq-server--handle-payload
         `((kind . "response") (id . ,id)
           (result . ((protocol_version . ,qq-server-protocol-version)
                      (gateway_instance_id . "gateway-42")
                      (capabilities . ["account.list" "account.start"])))))
        ;; A successful hello response alone is not a synchronized session.
        (should (eq qq-server--state 'authenticating))
        (should (= qq-server--reconnect-attempt 4))
        (qq-server--handle-payload
         '((kind . "event") (event . "gateway.ready")
           (data . ((gateway_instance_id . "gateway-42")
                    (accounts . [((account_id . "a-1"))])))))
        (should (eq qq-server--state 'ready))
        (should (= qq-server--reconnect-attempt 0))
        (should (equal (qq-server-gateway-instance-id)
                       "gateway-42"))
        (should
         (equal (alist-get 'account_id
                           (car (qq-server-ready-accounts)))
                "a-1"))
        (should (equal (caar events) "gateway.ready"))
        ;; Raw event data retains its array vector until the account-domain
        ;; validator deliberately normalizes it.
        (should (vectorp (alist-get 'accounts (cdar events))))))))

(ert-deftest qq-server-hello-result-has-closed-schema ()
  (qq-server-test-with-state
    (let (success-callback violations timer-started)
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback _allow-before-ready)
                   (setq success-callback callback)
                   "hello-request"))
                ((symbol-function 'qq-server--protocol-violation)
                 (lambda (&rest arguments)
                   (push (apply #'format arguments) violations)))
                ((symbol-function 'qq-server--start-ready-timer)
                 (lambda () (setq timer-started t))))
        (qq-server--send-hello
         (copy-sequence "0123456789abcdef0123456789abcdef"))
        (funcall
         success-callback
         `((protocol_version . ,qq-server-protocol-version)
           (gateway_instance_id . "gateway-42")
           (capabilities . ["account.list" "account.start"])))
        (should timer-started)
        (should (equal qq-server--hello-instance-id "gateway-42"))
        (should (equal qq-server--capabilities
                       '("account.list" "account.start")))
        (dolist (extra '((generation . 7) (future_field . "unknown")))
          (setq timer-started nil
                qq-server--hello-instance-id nil
                qq-server--capabilities nil)
          (funcall
           success-callback
           `((protocol_version . ,qq-server-protocol-version)
             (gateway_instance_id . "gateway-42")
             (capabilities . ["account.list"])
             ,extra))
          (should-not timer-started)
          (should-not qq-server--hello-instance-id)
          (should-not qq-server--capabilities))
        (should (= (length violations) 2))
        (should (cl-every (lambda (violation)
                            (equal violation "Malformed gateway.hello result"))
                          violations))))))

(ert-deftest qq-server-ready-null-is-not-an-empty-account-array ()
  (qq-server-test-with-state
    (let ((qq-server--state 'authenticating))
      (should-error
       (qq-server--handle-payload
        `((kind . "event") (event . "gateway.ready")
          (data . ((gateway_instance_id . "gateway-1")
                   (accounts . ,qq-server-wire-null)))))
       :type 'error)
      (should (eq qq-server--state 'authenticating)))))

(ert-deftest qq-server-ready-object-is-not-an-account-array ()
  (qq-server-test-with-state
    (let ((qq-server--state 'authenticating))
      (dolist (object '(nil ((field . "object"))))
        (should-error
         (qq-server--handle-payload
          `((kind . "event") (event . "gateway.ready")
            (data . ((gateway_instance_id . "gateway-1")
                     (accounts . ,object)))))
         :type 'error))
      (should (eq qq-server--state 'authenticating)))))

(ert-deftest qq-server-ready-instance-mismatch-reconnects ()
  (qq-server-test-with-state
    (let ((qq-server--state 'authenticating)
          reconnect)
      (cl-letf (((symbol-function 'qq-server--disconnect)
                 (lambda (&optional requested) (setq reconnect requested))))
        (qq-server--protocol-violation
         "%s"
         (condition-case error-data
             (progn
               (qq-server--handle-payload
                '((kind . "event") (event . "gateway.ready")
                  (data . ((gateway_instance_id . "different")
                           (accounts . [])))))
               "not rejected")
           (error (error-message-string error-data))))
        (should reconnect)))))

(ert-deftest qq-server-ready-timeout-is-connection-owned ()
  (qq-server-test-with-state
    (let* ((old-owner (list :socket 'old :settled nil))
           (new-owner (list :socket 'new :settled nil))
           (qq-server--connection-owner new-owner)
           (qq-server--state 'authenticating)
           reconnect)
      (cl-letf (((symbol-function 'qq-server--disconnect)
                 (lambda (&optional requested) (setq reconnect requested))))
        (qq-server--ready-timeout old-owner)
        (should-not reconnect)
        (qq-server--ready-timeout new-owner)
        (should reconnect)))))

(ert-deftest qq-server-stop-never-sends-account-command ()
  (qq-server-test-with-state
    (let (sent closed)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'websocket-close)
                 (lambda (socket) (setq closed socket))))
        (qq-server-stop)
        (should (eq closed 'test-websocket))
        (should-not sent)
        (should (eq qq-server--state 'stopped))))))

(ert-deftest qq-server-old-socket-cannot-retire-new-owner ()
  (let* ((old-owner (list :socket 'old-socket :settled nil))
         (new-owner (list :socket 'new-socket :settled nil))
         (qq-server--connection-owner new-owner)
         (qq-server--ws 'new-socket)
         (qq-server--connecting nil)
         (qq-server--stopping nil))
    (should-not
     (qq-server--settle-connection
      old-owner 'old-socket 'close))
    (should (eq qq-server--connection-owner new-owner))
    (should (eq qq-server--ws 'new-socket))))

(provide 'qq-server-test)

;;; qq-server-test.el ends here
