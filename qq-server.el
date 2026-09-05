;;; qq-server.el --- QQ service connection and protocol boundary -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Versioned request/response/event transport for the long-running Rust
;; service.  It owns JSON wire values, WebSocket lifecycle, readiness, and
;; opaque request IDs; account and QQ domain projections live above it.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'websocket)
(require 'qq-customize)

(defconst qq-server-wire-null (make-symbol "qq-server-wire-null")
  "Private value used to represent JSON null at the service boundary.")

(defun qq-server-wire-null-p (value)
  "Return non-nil when VALUE is the service JSON null sentinel."
  (eq value qq-server-wire-null))

(defun qq-server-wire-object-p (value)
  "Return non-nil when VALUE has the shape of a decoded JSON object."
  (and (listp value)
       (cl-every (lambda (entry)
                   (and (consp entry) (symbolp (car entry))))
                 value)))

(defun qq-server-wire-exact-object-keys-p (object keys)
  "Return non-nil when decoded OBJECT has exactly symbol KEYS."
  (and (qq-server-wire-object-p object)
       (equal
        (sort (mapcar #'car object)
              (lambda (left right)
                (string-lessp (symbol-name left) (symbol-name right))))
        (sort (copy-sequence keys)
              (lambda (left right)
                (string-lessp (symbol-name left) (symbol-name right)))))))

(defun qq-server-wire--copy (value domain-p)
  "Recursively copy VALUE.

When DOMAIN-P is non-nil, turn wire null into nil and wire vectors into
lists.  Supported compound values are conses, vectors, and hash tables."
  (cond
   ((and domain-p (qq-server-wire-null-p value)) nil)
   ((stringp value) (copy-sequence value))
   ((consp value)
    (cons (qq-server-wire--copy (car value) domain-p)
          (qq-server-wire--copy (cdr value) domain-p)))
   ((vectorp value)
    (let ((items (mapcar (lambda (item)
                           (qq-server-wire--copy item domain-p))
                         (append value nil))))
      (if domain-p items (vconcat items))))
   ((hash-table-p value)
    (let ((copy (make-hash-table
                 :test (hash-table-test value)
                 :size (hash-table-size value)
                 :rehash-size (hash-table-rehash-size value)
                 :rehash-threshold (hash-table-rehash-threshold value)
                 :weakness (hash-table-weakness value))))
      (maphash (lambda (key item)
                 (puthash (qq-server-wire--copy key domain-p)
                          (qq-server-wire--copy item domain-p)
                          copy))
               value)
      copy))
   (t value)))

(defun qq-server-value-copy (value)
  "Return a recursive ownership copy of service domain VALUE."
  (qq-server-wire--copy value nil))

(defun qq-server-wire-domain-copy (value)
  "Copy accepted wire VALUE into its public service domain form.

JSON null becomes nil and JSON array vectors become lists recursively."
  (qq-server-wire--copy value t))

(defun qq-server-wire-array (value context &optional allow-domain-list)
  "Validate wire array VALUE for CONTEXT and return a domain list.

Vectors are the only raw representation produced by the service decoder.
When ALLOW-DOMAIN-LIST is non-nil, proper lists are also accepted for an
explicitly internal, already-normalized registry boundary."
  (cond
   ((vectorp value)
    (mapcar #'qq-server-value-copy (append value nil)))
   ((and allow-domain-list (proper-list-p value))
    (qq-server-value-copy value))
   (t (error "qq: %s must be an array" context))))

(defun qq-server-wire-decode (text)
  "Decode service JSON TEXT without collapsing wire value kinds."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'array
                     :false-object :false
                     :null-object qq-server-wire-null))

(defconst qq-server-protocol-version 23
  "Native Gateway protocol version implemented by this client.")

(defvar qq-server-event-hook nil
  "Hook called with EVENT and DATA for each native service event.")

(defvar qq-server-protocol-error-hook nil
  "Hook called with one unsolicited native service error body.")

(defvar qq-server-state-hook nil
  "Hook called with the new native service connection state.")

(defvar qq-server--ws nil)
(defvar qq-server--connecting nil)
(defvar qq-server--connection-owner nil
  "Identity owner of the current connecting or open websocket.")
(defvar qq-server--stopping nil)
(defvar qq-server--pending (make-hash-table :test #'equal))
(defvar qq-server--request-counter 0)
(defvar qq-server--reconnect-timer nil)
(defvar qq-server--reconnect-attempt 0)
(defvar qq-server--ready-timer nil)
(defvar qq-server--state 'stopped)
(defvar qq-server--hello-instance-id nil)
(defvar qq-server--gateway-instance-id nil)
(defvar qq-server--capabilities nil)
(defvar qq-server--ready-accounts nil)

(defun qq-server-state ()
  "Return the current native service connection state."
  qq-server--state)

(defun qq-server-gateway-instance-id ()
  "Return the current authenticated Gateway instance ID, or nil."
  (and qq-server--gateway-instance-id
       (copy-sequence qq-server--gateway-instance-id)))

(defun qq-server-capabilities ()
  "Return a copy of capabilities advertised by the current Gateway."
  (qq-server-value-copy qq-server--capabilities))

(defun qq-server-ready-accounts ()
  "Return a copy of the accounts in the latest `gateway.ready' snapshot."
  (qq-server-value-copy qq-server--ready-accounts))

(defun qq-server-running-p ()
  "Return non-nil while the native service connection is active."
  (or qq-server--connecting
      (and qq-server--ws
           (websocket-openp qq-server--ws))
      (timerp qq-server--reconnect-timer)))

(defun qq-server-ready-p ()
  "Return non-nil when native service business requests may be sent."
  (and (eq qq-server--state 'ready)
       qq-server--ws
       (websocket-openp qq-server--ws)))

(defun qq-server--run-hook (hook &rest arguments)
  "Run HOOK with ARGUMENTS without handing transport ownership to consumers."
  (apply
   #'run-hook-wrapped hook
   (lambda (function &rest hook-arguments)
     (condition-case error-data
         (apply function (mapcar #'qq-server-value-copy hook-arguments))
       (error
        (message "qq: Gateway hook %s failed in %S: %s"
                 hook function (error-message-string error-data))))
     nil)
   arguments))

(defun qq-server--set-state (state)
  "Publish native service connection STATE when it changes."
  (unless (eq state qq-server--state)
    (setq qq-server--state state)
    (qq-server--run-hook
     'qq-server-state-hook state))
  state)

(defun qq-server--next-request-id ()
  "Return a fresh opaque native service request ID."
  (format "emacs-qq-%d" (cl-incf qq-server--request-counter)))

(defun qq-server--json-encode (object)
  "Encode OBJECT into compact JSON text."
  (let ((json-encoding-pretty-print nil)
        (json-false :false)
        (json-null nil))
    (json-encode object)))

(defun qq-server--json-decode (text)
  "Decode native service JSON TEXT without collapsing wire value kinds."
  (qq-server-wire-decode text))

(defun qq-server--frame-text (frame)
  "Return UTF-8 text carried by websocket FRAME, or nil."
  (pcase (websocket-frame-opcode frame)
    ('text (websocket-frame-text frame))
    ('binary (error "Gateway server sent a binary frame"))
    (_ nil)))

(defun qq-server--exact-object-keys-p (object keys)
  "Return non-nil when alist OBJECT has exactly symbol KEYS."
  (qq-server-wire-exact-object-keys-p object keys))

(defun qq-server--read-auth-token ()
  "Read and validate `qq-server-auth-token-file'."
  (let ((path (and (stringp qq-server-auth-token-file)
                   (expand-file-name qq-server-auth-token-file))))
    (unless (and path (file-regular-p path))
      (user-error "qq: Gateway token file is not a regular file: %s"
                  (or path qq-server-auth-token-file)))
    (when (and (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix
                                   darwin cygwin))
               (let ((mode (file-modes path)))
                 (or (null mode) (/= 0 (logand mode #o077)))))
      (user-error
       "qq: Gateway token file is accessible by group/others; use chmod 600: %s"
       path))
    (let ((token
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8))
               (insert-file-contents path))
             (replace-regexp-in-string
              "[[:space:]]+\\'" "" (buffer-substring-no-properties
                                      (point-min) (point-max))))))
      (when (or (< (string-bytes token) 32)
                (string-match-p "[[:space:]]" token))
        (user-error
         "qq: Gateway token must be one non-whitespace value of at least 32 bytes"))
      token)))

(defun qq-server--cancel-entry-timer (entry)
  "Cancel the timeout stored in pending ENTRY."
  (when-let* ((timer (plist-get entry :timer)))
    (when (timerp timer)
      (cancel-timer timer))))

(defun qq-server--invoke-callback (callback &rest arguments)
  "Invoke CALLBACK with ARGUMENTS while isolating ordinary errors."
  (when callback
    (condition-case error-data
        (apply callback arguments)
      (error
       (message "qq: Gateway callback error: %s"
                (error-message-string error-data))))))

(defun qq-server--complete (id outcome value reason)
  "Complete pending request ID exactly once.

OUTCOME is `success' or `error'.  VALUE is the result or protocol error body;
REASON is a human-readable failure.  Return non-nil when ID was pending."
  (when-let* ((entry (gethash id qq-server--pending)))
    (remhash id qq-server--pending)
    (qq-server--cancel-entry-timer entry)
    (if (eq outcome 'success)
        (qq-server--invoke-callback
         (plist-get entry :success) value)
      (qq-server--invoke-callback
       (plist-get entry :error) value reason))
    t))

(defun qq-server-cancel (id)
  "Forget pending native service request ID without invoking callbacks."
  (when-let* ((entry (gethash id qq-server--pending)))
    (remhash id qq-server--pending)
    (qq-server--cancel-entry-timer entry)
    t))

(defun qq-server--request-timeout (id method)
  "Fail pending request ID for METHOD after its local timeout."
  (qq-server--complete
   id 'error nil (format "%s request timed out" method)))

(defun qq-server--fail-pending (reason)
  "Fail every pending request with REASON, even if one callback quits."
  (let (ids quit-seen-p)
    (maphash (lambda (id _entry) (push id ids))
             qq-server--pending)
    (dolist (id ids)
      (condition-case nil
          (qq-server--complete id 'error nil reason)
        (quit
         (setq quit-seen-p t
               quit-flag nil))))
    (when quit-seen-p
      (message "qq: ignored quit from callback during Gateway cleanup"))))

(defun qq-server--clear-session-metadata ()
  "Forget metadata tied to the previous websocket handshake."
  (setq qq-server--hello-instance-id nil
        qq-server--gateway-instance-id nil
        qq-server--capabilities nil
        qq-server--ready-accounts nil))

(defun qq-server--clear-owner-token (owner)
  "Erase the authentication token retained by connection OWNER."
  (when-let* ((token (plist-get owner :auth-token)))
    (when (stringp token)
      (clear-string token))
    (setf (plist-get owner :auth-token) nil)))

(defun qq-server--connection-current-p (owner socket)
  "Return non-nil when OWNER still owns SOCKET.

The first synchronous callback may bind OWNER before `websocket-open'
returns."
  (and (eq owner qq-server--connection-owner)
       (not (plist-get owner :settled))
       (let ((owned-socket (plist-get owner :socket)))
         (cond
          ((null owned-socket)
           (setf (plist-get owner :socket) socket)
           t)
          ((eq owned-socket socket))))))

(defun qq-server--clear-reconnect-timer ()
  "Cancel the current native service reconnect timer."
  (when (timerp qq-server--reconnect-timer)
    (cancel-timer qq-server--reconnect-timer))
  (setq qq-server--reconnect-timer nil))

(defun qq-server--clear-ready-timer ()
  "Cancel the timer awaiting the authoritative ready snapshot."
  (when (timerp qq-server--ready-timer)
    (cancel-timer qq-server--ready-timer))
  (setq qq-server--ready-timer nil))

(defun qq-server--ready-timeout (owner)
  "Reconnect when connection OWNER never receives `gateway.ready'."
  (when (and (eq owner qq-server--connection-owner)
             (eq qq-server--state 'authenticating))
    (setq qq-server--ready-timer nil)
    (message "qq: Gateway ready snapshot timed out")
    (qq-server--disconnect t)))

(defun qq-server--start-ready-timer ()
  "Start the current connection's `gateway.ready' deadline."
  (qq-server--clear-ready-timer)
  (when (and (numberp qq-server-ready-timeout)
             (> qq-server-ready-timeout 0))
    (let ((owner qq-server--connection-owner))
      (setq qq-server--ready-timer
            (run-at-time qq-server-ready-timeout nil
                         #'qq-server--ready-timeout owner)))))

(defun qq-server--schedule-reconnect ()
  "Schedule a native service reconnect when policy permits."
  (let ((next-attempt (1+ qq-server--reconnect-attempt))
        (max-attempts qq-server-reconnect-max-attempts))
    (if (and (integerp max-attempts) (> next-attempt max-attempts))
        (progn
          (setq qq-server--stopping t)
          (qq-server--set-state 'stopped)
          (message "qq: Gateway reached reconnect attempt limit (%d)"
                   max-attempts))
      (setq qq-server--reconnect-attempt next-attempt)
      (setq qq-server--reconnect-timer
            (run-at-time
             (max 0.2 (float qq-server-reconnect-delay)) nil
             (lambda ()
               (setq qq-server--reconnect-timer nil)
               (unless qq-server--stopping
                 (qq-server--connect)))))
      (qq-server--set-state 'reconnecting)
      (message "qq: reconnecting to Gateway in %.1fs (attempt %d)"
               (max 0.2 (float qq-server-reconnect-delay)) next-attempt))))

(defun qq-server--disconnect (&optional reconnect)
  "Disconnect only this Emacs Gateway client.

When RECONNECT is non-nil, schedule a new websocket connection.  This never
  sends an account stop or logout command to the long-lived Gateway."
  (when qq-server--connection-owner
    (qq-server--clear-owner-token
     qq-server--connection-owner)
    (setf (plist-get qq-server--connection-owner :settled) t))
  (setq qq-server--connection-owner nil
        qq-server--connecting nil)
  (when qq-server--ws
    (ignore-errors (websocket-close qq-server--ws))
    (setq qq-server--ws nil))
  (qq-server--fail-pending "Gateway transport disconnected")
  (qq-server--clear-session-metadata)
  (qq-server--clear-reconnect-timer)
  (qq-server--clear-ready-timer)
  (if (and reconnect (not qq-server--stopping))
      (qq-server--schedule-reconnect)
    (qq-server--set-state 'stopped)))

(defun qq-server--settle-connection
    (owner socket event &optional error-data)
  "Settle OWNER's SOCKET once for EVENT and optional ERROR-DATA."
  (when (qq-server--connection-current-p owner socket)
    (qq-server--clear-owner-token owner)
    (setf (plist-get owner :settled) t)
    (setq qq-server--connection-owner nil
          qq-server--connecting nil)
    (pcase event
      ('close (message "qq: Gateway websocket closed"))
      ('error (message "qq: Gateway websocket error: %s"
                       (error-message-string error-data))))
    (unless qq-server--stopping
      (qq-server--disconnect t))
    t))

(defun qq-server--protocol-violation (format-string &rest arguments)
  "Close and reconnect after a native protocol violation.

FORMAT-STRING and ARGUMENTS describe the violation."
  (message "qq: Gateway protocol violation: %s"
           (apply #'format format-string arguments))
  (qq-server--disconnect t))

(defun qq-server--dispatch-response (payload)
  "Dispatch a validated response PAYLOAD."
  (let ((id (alist-get 'id payload)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "Gateway response id must be a non-empty string"))
    (qq-server--complete
     id 'success (alist-get 'result payload nil nil #'eq) nil)))

(defun qq-server--dispatch-error (payload)
  "Dispatch a validated error PAYLOAD."
  (let* ((wire-id (alist-get 'id payload nil nil #'eq))
         (id (if (qq-server-wire-null-p wire-id) nil wire-id))
         (wire-body (alist-get 'error payload nil nil #'eq))
         (code (and (qq-server-wire-object-p wire-body)
                    (alist-get 'code wire-body)))
         (reason (and (qq-server-wire-object-p wire-body)
                      (alist-get 'message wire-body))))
    (unless (and (or (null id)
                     (and (stringp id) (not (string-empty-p id))))
                 (qq-server--exact-object-keys-p
                  wire-body '(code message))
                 (stringp code) (not (string-empty-p code))
                 (stringp reason) (not (string-empty-p reason)))
      (error "Gateway error envelope is malformed"))
    (let ((body (qq-server-wire-domain-copy wire-body)))
      (if id
          (qq-server--complete id 'error body reason)
        (progn
          (qq-server--run-hook
           'qq-server-protocol-error-hook body)
          (message "qq: Gateway protocol error %s: %s" code reason))))))

(defun qq-server--dispatch-event (payload)
  "Dispatch a validated event PAYLOAD."
  (let ((event (alist-get 'event payload))
        (data (alist-get 'data payload nil nil #'eq)))
    (unless (and (stringp event) (not (string-empty-p event)))
      (error "Gateway event name must be a non-empty string"))
    (when (equal event "gateway.ready")
      (unless (and (qq-server--exact-object-keys-p
                    data '(gateway_instance_id accounts))
                   (stringp (alist-get 'gateway_instance_id data)))
        (error "Gateway ready data is malformed"))
      (let ((domain-accounts
             (qq-server-wire-array
              (alist-get 'accounts data nil nil #'eq)
              "Gateway ready accounts")))
        (unless (and qq-server--hello-instance-id
                     (equal qq-server--hello-instance-id
                            (alist-get 'gateway_instance_id data)))
          (error "Gateway ready instance does not match hello response"))
        (setq data (qq-server-value-copy data))
        (setq qq-server--gateway-instance-id
              (copy-sequence (alist-get 'gateway_instance_id data))
              qq-server--ready-accounts
              (mapcar #'qq-server-wire-domain-copy domain-accounts)
              qq-server--reconnect-attempt 0)
        (qq-server--clear-ready-timer)
        (qq-server--set-state 'ready)
        (message "qq: native service ready")))
    (qq-server--run-hook
     'qq-server-event-hook event data)))

(defun qq-server--handle-payload (payload)
  "Validate and dispatch one decoded native service PAYLOAD."
  (unless (qq-server-wire-object-p payload)
    (error "Gateway envelope must be an object"))
  (pcase (alist-get 'kind payload)
    ("response"
     (unless (qq-server--exact-object-keys-p
              payload '(kind id result))
       (error "Gateway response envelope has invalid fields"))
     (qq-server--dispatch-response payload))
    ("error"
     (unless (qq-server--exact-object-keys-p
              payload '(kind id error))
       (error "Gateway error envelope has invalid fields"))
     (qq-server--dispatch-error payload))
    ("event"
     (unless (qq-server--exact-object-keys-p
              payload '(kind event data))
       (error "Gateway event envelope has invalid fields"))
     (qq-server--dispatch-event payload))
    (_ (error "Unknown Gateway envelope kind"))))

(defun qq-server-send
    (method params &optional callback errback allow-before-ready)
  "Send native service METHOD with PARAMS.

CALLBACK receives the successful result object.  ERRBACK receives the
protocol error body (or nil) and a human-readable reason.  Return the opaque
request ID, or nil when unavailable.  Unavailable requests invoke ERRBACK
before returning nil.  A non-nil ID denotes accepted work whose callback runs
later from the WebSocket or timeout event loop, never before this function
returns.  ALLOW-BEFORE-READY is private transport machinery used only for the
initial `gateway.hello' request."
  (if (not (and qq-server--ws
                (websocket-openp qq-server--ws)
                (or allow-before-ready
                    (qq-server-ready-p))))
      (progn
        (qq-server--invoke-callback
         errback nil
         (if (and qq-server--ws
                  (websocket-openp qq-server--ws))
             "Gateway handshake is not ready"
           "Gateway transport is not connected"))
        nil)
    (when quit-flag
      (setq quit-flag nil)
      (signal 'quit nil))
    (let* ((id (qq-server--next-request-id))
           (timeout qq-server-request-timeout)
           timer
           (entry (list :method method :success callback :error errback
                        :timer nil))
           (payload `((kind . "request")
                      (id . ,id)
                      (method . ,method)
                      (params . ,(or params
                                     (make-hash-table :test #'equal))))))
      (let ((inhibit-quit t))
        (condition-case error-data
            (progn
              (setq timer
                    (and (numberp timeout) (> timeout 0)
                         (run-at-time timeout nil
                                      #'qq-server--request-timeout
                                      id method)))
              (setf (plist-get entry :timer) timer)
              (puthash id entry qq-server--pending)
              (websocket-send-text
               qq-server--ws
               (qq-server--json-encode payload))
              (setq quit-flag nil)
              id)
          (quit
           (if (gethash id qq-server--pending)
               (progn (setq quit-flag nil) id)
             (when (timerp timer) (cancel-timer timer))
             (setq quit-flag nil)
             (signal 'quit nil)))
          (error
           (let ((reason (error-message-string error-data)))
             (if (gethash id qq-server--pending)
                 (qq-server--complete id 'error nil reason)
               (when (timerp timer) (cancel-timer timer))
               (qq-server--invoke-callback errback nil reason)))
           (setq quit-flag nil)
           nil))))))

(defun qq-server--hello-failed (_error-body reason)
  "Stop reconnecting after terminal handshake failure REASON."
  (setq qq-server--stopping t)
  (qq-server--disconnect nil)
  (qq-server--set-state 'failed)
  (message "qq: Gateway handshake failed: %s" reason))

(defun qq-server--send-hello (token)
  "Send the mandatory first request using authentication TOKEN."
  (unwind-protect
      (unless
          (qq-server-send
           "gateway.hello"
           `((protocol_version . ,qq-server-protocol-version)
             (client_name . ,qq-server-client-name)
             (auth_token . ,token))
           (lambda (result)
             (condition-case nil
                 (let ((capabilities
                        (qq-server-wire-array
                         (and (qq-server-wire-object-p result)
                              (alist-get 'capabilities result nil nil #'eq))
                         "Gateway capabilities")))
                   (if (and (equal (alist-get 'protocol_version result)
                                   qq-server-protocol-version)
                            (qq-server--exact-object-keys-p
                             result
                             '(protocol_version gateway_instance_id
                               capabilities))
                            (stringp (alist-get 'gateway_instance_id result))
                            (cl-every (lambda (capability)
                                        (and (stringp capability)
                                             (not (string-empty-p capability))))
                                      capabilities))
                       (progn
                         (setq qq-server--hello-instance-id
                               (copy-sequence
                                (alist-get 'gateway_instance_id result))
                               qq-server--capabilities
                               (qq-server-value-copy capabilities))
                         (qq-server--start-ready-timer))
                     (qq-server--protocol-violation
                      "Malformed gateway.hello result")))
               (error
                (qq-server--protocol-violation
                 "Malformed gateway.hello result"))))
           #'qq-server--hello-failed
           t)
        (error "Could not send gateway.hello"))
    ;; Avoid retaining the original file-backed secret in the connection
    ;; callback after the request text has been handed to websocket.el.
    (clear-string token)))

(defun qq-server--connect ()
  "Open and authenticate the native service websocket when needed."
  (unless (qq-server-running-p)
    ;; Resolve credentials before publishing a connecting owner.  A local
    ;; configuration error is terminal until the user calls start again.
    (condition-case error-data
        (let ((auth-token (qq-server--read-auth-token)))
          (let ((owner (list :socket nil :settled nil
                             :auth-token auth-token)))
            (setq qq-server--connection-owner owner
                  qq-server--connecting t
                  qq-server--stopping nil)
            (qq-server--set-state 'connecting)
            (condition-case socket-error
                (let ((socket
                       (websocket-open
                        qq-server-websocket-url
                        :on-open
                        (lambda (ws)
                          (when (qq-server--connection-current-p
                                 owner ws)
                            (setq qq-server--ws ws
                                  qq-server--connecting nil)
                            (qq-server--set-state 'authenticating)
                            (condition-case hello-error
                                (progn
                                  (qq-server--send-hello auth-token)
                                  (setf (plist-get owner :auth-token) nil))
                              (error
                               (qq-server--hello-failed
                                nil (error-message-string hello-error))))))
                        :on-message
                        (lambda (ws frame)
                          (when (qq-server--connection-current-p
                                 owner ws)
                            (condition-case payload-error
                                (when-let* ((text
                                             (qq-server--frame-text
                                              frame)))
                                  (unless (string-empty-p (string-trim text))
                                    (qq-server--handle-payload
                                     (qq-server--json-decode text))))
                              (error
                               (when
                                   (qq-server--connection-current-p
                                    owner ws)
                                 (qq-server--protocol-violation
                                  "%s"
                                  (error-message-string payload-error)))))))
                        :on-close
                        (lambda (ws)
                          (qq-server--settle-connection
                           owner ws 'close))
                        :on-error
                        (lambda (ws _type error-value)
                          (qq-server--settle-connection
                           owner ws 'error error-value)))))
                  (when (qq-server--connection-current-p
                         owner socket)
                    (setq qq-server--ws socket)))
              (error
               (qq-server--settle-connection
                owner nil 'error socket-error)))))
      (error
       (setq qq-server--stopping t)
       (qq-server--set-state 'failed)
       (message "qq: cannot start Gateway transport: %s"
                (error-message-string error-data))))))

(defun qq-server-start ()
  "Start the native service websocket without starting a Native Session."
  (interactive)
  (setq qq-server--stopping nil)
  (unless (timerp qq-server--reconnect-timer)
    (setq qq-server--reconnect-attempt 0))
  (qq-server--connect))

(defun qq-server-stop ()
  "Stop this Emacs websocket without stopping any QQ account."
  (interactive)
  (setq qq-server--stopping t
        qq-server--reconnect-attempt 0)
  (qq-server--disconnect nil)
  (message "qq: native service connection stopped; accounts remain managed"))

(provide 'qq-server)

;;; qq-server.el ends here
