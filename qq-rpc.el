;;; qq-rpc.el --- Typed QQ service operations and events -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; This module is the single business-RPC boundary above
;; `qq-server'.  It owns readiness and capability checks, result
;; transformation, stale-context rejection, and callback isolation.  It does
;; not depend on account or domain projections, so projection modules may all
;; depend on it without introducing dependency cycles.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-server)

(defun qq-rpc-invoke (callback &rest arguments)
  "Invoke leaf CALLBACK with owned copies of ARGUMENTS.

Ordinary callback errors are isolated from Gateway request machinery."
  (when callback
    (condition-case error-data
        (apply callback (mapcar #'qq-server-value-copy arguments))
      (error
       (message "qq: Gateway RPC callback failed: %s"
                (error-message-string error-data))))))

(defun qq-rpc-run-hook (hook &rest arguments)
  "Run each function on HOOK with owned copies of ARGUMENTS.

Consumer errors are isolated from Gateway event and projection machinery."
  (apply
   #'run-hook-wrapped hook
   (lambda (function &rest hook-arguments)
     (condition-case error-data
         (apply function (mapcar #'qq-server-value-copy hook-arguments))
       (error
        (message "qq: Gateway client hook %s failed in %S: %s"
                 hook function (error-message-string error-data))))
     nil)
   arguments))

(defun qq-rpc-client-error
    (errback code format-string &rest arguments)
  "Synchronously invoke ERRBACK with client CODE and formatted reason.

FORMAT-STRING and ARGUMENTS produce the human-readable reason.  Return nil so
preflight failures preserve the request API's unavailable-token convention."
  (let ((reason (apply #'format format-string arguments)))
    (qq-rpc-invoke
     errback `((code . ,code) (message . ,reason)) reason)
    nil))

(defun qq-rpc-method-available-p (method)
  "Return non-nil when the ready Gateway advertises METHOD."
  (member method (qq-server-capabilities)))

(defun qq-rpc--success
    (result projector callback errback current-p
            stale-code stale-message)
  "Own and project one successful RESULT, then invoke CALLBACK.

This boundary performs the sole wire-to-domain ownership conversion before
the optional unary PROJECTOR runs.  CURRENT-P is an optional zero-argument
context predicate.  ERRBACK receives STALE-CODE and STALE-MESSAGE when the
predicate normally returns nil.  Signals from the predicate or PROJECTOR are
reported as `invalid_gateway_result'."
  (let ((outcome
         (condition-case error-data
             (if (and current-p (not (funcall current-p)))
                 '(stale)
               (let* ((domain (qq-server-wire-domain-copy result))
                      (projected
                       (if projector (funcall projector domain) domain)))
                 (list 'ok projected)))
           (error (list 'invalid error-data)))))
    (pcase (car outcome)
      ('ok
       ;; Keep the user's leaf callback outside the transformer handler: a
       ;; consumer bug must not be misreported as malformed Gateway data.
       (qq-rpc-invoke callback (cadr outcome)))
      ('stale
       (qq-rpc-client-error
        errback stale-code "%s" stale-message))
      ('invalid
       (qq-rpc-client-error
        errback "invalid_gateway_result" "%s"
        (error-message-string (cadr outcome)))))))

(defun qq-rpc--failure
    (body reason errback current-p stale-code stale-message)
  "Deliver transport failure BODY and REASON for the current context.

CURRENT-P has the same ownership semantics as on the success path.  A stale
request reports STALE-CODE and STALE-MESSAGE instead of leaking a late server
failure into a replacement request owner.  A signal from CURRENT-P is a local
contract failure and is therefore reported as `invalid_gateway_result'."
  (let ((outcome
         (condition-case error-data
             (if (and current-p (not (funcall current-p)))
                 '(stale)
               '(current))
           (error (list 'invalid error-data)))))
    (pcase (car outcome)
      ('current
       (qq-rpc-invoke errback body reason))
      ('stale
       (qq-rpc-client-error
        errback stale-code "%s" stale-message))
      ('invalid
       (qq-rpc-client-error
        errback "invalid_gateway_result" "%s"
        (error-message-string (cadr outcome)))))))

(cl-defun qq-rpc-call
    (method params
            &key projector callback errback current-p
            (stale-code "stale_request")
            (stale-message "Gateway request context is no longer current"))
  "Call advertised Gateway METHOD with PARAMS through the typed RPC boundary.

This RPC boundary recursively owns and domainizes the successful wire result
before PROJECTOR may update an adapter-side projection and return the value
for CALLBACK.  PROJECTOR is optional.  Any ordinary error it signals becomes
an `invalid_gateway_result' delivered to ERRBACK.

When optional CURRENT-P normally returns nil, skip transformation and report
STALE-CODE with STALE-MESSAGE.  An error signaled by CURRENT-P is treated like
a transformer error.  User CALLBACK and ERRBACK functions are isolated leaf
consumers and receive owned copies.

Preflight errors invoke ERRBACK synchronously and return nil.  Otherwise return
exactly the opaque token from `qq-server-send'; accepted work
completes later on the transport event loop.  Transport signals are
deliberately not caught."
  (cond
   ((not (qq-server-ready-p))
    (qq-rpc-client-error
     errback "gateway_not_ready" "Gateway transport is not ready"))
   ((not (qq-rpc-method-available-p method))
    (qq-rpc-client-error
     errback "capability_unavailable"
     "Gateway does not advertise capability %s" method))
   (t
    (qq-server-send
     method params
     (lambda (result)
       (qq-rpc--success
        result projector callback errback current-p
        stale-code stale-message))
     (lambda (body reason)
       (qq-rpc--failure
        body reason errback current-p stale-code stale-message))))))

(cl-defstruct (qq-rpc-latest-request
               (:constructor qq-rpc-latest-request--create))
  "One newest-owned RPC request with an exactly-once local lifecycle."
  state
  transport-token
  errback
  owner-symbol)

(defun qq-rpc--latest-request-current-p (request)
  "Return non-nil when REQUEST is active and still owns its registry."
  (and (qq-rpc-latest-request-p request)
       (eq (qq-rpc-latest-request-state request) 'active)
       (eq request
           (symbol-value
            (qq-rpc-latest-request-owner-symbol request)))))

(defun qq-rpc--finish-latest-request (request)
  "Settle active REQUEST exactly once and release only its own marker."
  (when (and (qq-rpc-latest-request-p request)
             (eq (qq-rpc-latest-request-state request) 'active))
    (setf (qq-rpc-latest-request-state request) 'settled
          (qq-rpc-latest-request-transport-token request) nil)
    (let ((owner-symbol
           (qq-rpc-latest-request-owner-symbol request)))
      (when (eq request (symbol-value owner-symbol))
        (set owner-symbol nil)))
    t))

(defun qq-rpc--cancel-latest-request (request code message)
  "Cancel and settle active REQUEST with client CODE and MESSAGE."
  (when (and (qq-rpc-latest-request-p request)
             (eq (qq-rpc-latest-request-state request) 'active))
    (let ((token (qq-rpc-latest-request-transport-token request))
          (errback (qq-rpc-latest-request-errback request)))
      ;; Revoke before touching transport or user code.  Either may reenter.
      (qq-rpc--finish-latest-request request)
      (when token
        (qq-server-cancel token))
      (qq-rpc-client-error errback code "%s" message)
      t)))

(defun qq-rpc-cancel-latest
    (owner-symbol &optional code message)
  "Cancel and settle the request currently stored in OWNER-SYMBOL.

CODE defaults to `superseded_request'.  MESSAGE is copied into the standard
client error delivered exactly once to the request errback."
  (let ((request (symbol-value owner-symbol)))
    (when (qq-rpc-latest-request-p request)
      (qq-rpc--cancel-latest-request
       request (or code "superseded_request")
       (or message "Gateway registry request was superseded")))))

(cl-defun qq-rpc-latest-call
    (owner-symbol method params
                  &key projector callback errback)
  "Run the newest-owned registry request for OWNER-SYMBOL.

Publish a request record before cancelling its predecessor or calling METHOD.
The record owns the transport token and settles exactly once.  A newer call
cancels and reports the predecessor as superseded before it sends; reentrant
errbacks may themselves replace the new request.  Only the surviving owner
may project or deliver.  Return the transport token."
  (let* ((previous (symbol-value owner-symbol))
         (request
          (qq-rpc-latest-request--create
           :state 'active :errback errback
           :owner-symbol owner-symbol)))
    ;; Publish first so a predecessor's possibly reentrant errback observes
    ;; the replacement rather than an empty ownership window.
    (set owner-symbol request)
    (condition-case error-data
        (progn
          (when (qq-rpc-latest-request-p previous)
            (qq-rpc--cancel-latest-request
             previous "superseded_request"
             (format "Gateway %s request was superseded" method)))
          (when (qq-rpc--latest-request-current-p request)
            (cl-labels
                ((current-p ()
                   (qq-rpc--latest-request-current-p request))
                 (finish-success (value)
                   (if (current-p)
                       (when (qq-rpc--finish-latest-request request)
                         (qq-rpc-invoke callback value))
                     (when (qq-rpc--finish-latest-request request)
                       (qq-rpc-client-error
                        errback "superseded_request"
                        "Gateway %s request was superseded" method))))
                 (finish-error (body failure)
                   (when (eq (qq-rpc-latest-request-state request)
                             'active)
                     (let ((current (current-p)))
                       (when (qq-rpc--finish-latest-request request)
                         (if current
                             (qq-rpc-invoke errback body failure)
                           (qq-rpc-client-error
                            errback "superseded_request"
                            "Gateway %s request was superseded" method)))))))
              (let ((token
                     (qq-rpc-call
                      method params
                      :current-p #'current-p
                      :stale-code "superseded_request"
                      :stale-message
                      (format "Gateway %s request was superseded" method)
                      :projector projector
                      :callback #'finish-success
                      :errback #'finish-error)))
                (when (and token (current-p))
                  (setf
                   (qq-rpc-latest-request-transport-token request)
                   token))
                token))))
      ((error quit)
       (qq-rpc--finish-latest-request request)
       (signal (car error-data) (cdr error-data))))))

(defun qq-rpc-request-single-flight
    (marker-symbol marker-tag starter failure-label)
  "Start one automatic registry request owned by MARKER-SYMBOL.

An existing marker coalesces work until its request succeeds, fails, or is
cancelled by a newer request or registry reset.  MARKER-TAG names the opaque
`eq' identity.  STARTER is called with success and failure callbacks.  Only
the marker's own callbacks clear it.  FAILURE-LABEL names diagnostics."
  (unless (symbol-value marker-symbol)
      (let ((marker (list marker-tag)))
        (set marker-symbol marker)
        (cl-labels
            ((finish ()
               (when (eq marker (symbol-value marker-symbol))
                 (set marker-symbol nil)
                 t))
             (succeed (&rest _arguments)
               (finish))
             (fail (body failure)
               (when (finish)
                 (unless (equal (alist-get 'code body) "superseded_request")
                   (message "qq: Gateway %s resync failed: %s"
                            failure-label failure)))))
          (condition-case error-data
              (funcall starter #'succeed #'fail)
            ((error quit)
             (finish)
             (signal (car error-data) (cdr error-data))))))))

(defvar qq-rpc--event-handlers
  (make-hash-table :test #'equal)
  "Domain handler indexed by service event name.")

(defvar qq-rpc--error-handlers
  (make-hash-table :test #'equal)
  "Domain handler indexed by unsolicited service error code.")

(defun qq-rpc--register (table kind name handler)
  "Register named HANDLER for NAME of KIND in TABLE.

Registering the same symbol again is idempotent.  A different owner for an
existing NAME is an architecture error."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "qq: service %s name must be a non-empty string" kind))
  (unless (and handler (symbolp handler))
    (error "qq: service %s handler must be a named function" kind))
  (let ((existing (gethash name table)))
    (when (and existing (not (eq existing handler)))
      (error "qq: service %s %s already belongs to %S"
             kind name existing))
    (puthash name handler table))
  handler)

(defun qq-rpc-register-event (event handler)
  "Make named HANDLER the sole domain owner of service EVENT.

HANDLER receives EVENT and an owned, recursively domainized DATA value."
  (qq-rpc--register qq-rpc--event-handlers "event" event handler))

(defun qq-rpc-register-error (code handler)
  "Make named HANDLER the sole domain owner of unsolicited error CODE.

HANDLER receives the validated protocol error body."
  (qq-rpc--register qq-rpc--error-handlers "error" code handler))

(defun qq-rpc--handle-transport-event (event data)
  "Route service EVENT and DATA to its explicit domain owner."
  (when-let* ((handler (gethash event qq-rpc--event-handlers)))
    (funcall handler event (qq-server-wire-domain-copy data))))

(defun qq-rpc--handle-transport-error (body)
  "Route unsolicited protocol error BODY by its exact code."
  (let* ((code (and (listp body) (alist-get 'code body)))
         (handler (and (stringp code)
                       (gethash code qq-rpc--error-handlers))))
    (when handler
      (condition-case error-data
          (funcall handler (qq-server-wire-domain-copy body))
        (error
         (message "qq: service error handler %S failed for %s: %s"
                  handler code (error-message-string error-data)))))))

(add-hook 'qq-server-event-hook #'qq-rpc--handle-transport-event)
(add-hook 'qq-server-protocol-error-hook #'qq-rpc--handle-transport-error)

(provide 'qq-rpc)

;;; qq-rpc.el ends here
