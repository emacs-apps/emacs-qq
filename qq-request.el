;;; qq-request.el --- QQ request ownership -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Product requests outlive individual transport frames: some requests stage
;; media before dispatch and some are superseded.  This module provides the
;; single request handle exposed by the native facade and keeps those lifecycle
;; rules out of UI callers.
;;
;; A request's `owner' is only the observation context in which its callback
;; may still be delivered; it is not Gateway authority and is never serialized
;; onto the wire.  Nil denotes global work, while a string follows one stable
;; account slot across Native Session replacement.  Exact session identity is
;; deliberately kept behind the Gateway boundary; product callers never
;; coordinate it.
;; Appkit lifecycle ownership is stored separately and may cancel the logical
;; request without changing that account observation context.

;;; Code:

(require 'appkit-core)
(require 'cl-lib)
(require 'qq-account)
(require 'qq-server)
(require 'qq-runtime)

(cl-defstruct (qq-request-watch
               (:constructor qq-request-watch-create))
  "Cancellable local observer of a projected service resource."
  active-p
  cancel-function)

(defun qq-request-watch-cancel (watch)
  "Detach active projection WATCH exactly once."
  (when (and (qq-request-watch-p watch)
             (qq-request-watch-active-p watch))
    (let ((cancel (qq-request-watch-cancel-function watch)))
      (setf (qq-request-watch-active-p watch) nil
            (qq-request-watch-cancel-function watch) nil)
      (when cancel
        (funcall cancel))
      t)))

(cl-defstruct (qq-request
               (:constructor qq-request--create))
  "Opaque, exactly-once native request handle."
  owner
  (state 'active)
  token
  cancel-function
  lifecycle-handle)

(defvar qq-request--active (make-hash-table :test #'eq)
  "Active native requests, keyed by request identity.")

(defun qq-request--copy-owner (owner)
  "Return an isolated copy of callback observation OWNER context."
  (cond
   ((null owner) nil)
   ((and (stringp owner) (> (length owner) 0)) (copy-sequence owner))
   (t (error "qq: invalid native request owner context %S" owner))))

(defun qq-request-create (&optional owner cancel-function lifecycle-owner)
  "Create and register an active request for OWNER context.

Nil denotes global work and a non-empty string denotes a stable managed
account slot.  These scopes only control callback delivery; they do not
authorize a request or add fields to its wire parameters.

CANCEL-FUNCTION revokes adapter work and is called at most once.
LIFECYCLE-OWNER, when non-nil, owns cancellation through an Appkit handle.
Callers that only own a Gateway transport token should use `qq-request-start'."
  (let ((request
          (qq-request--create
           :owner (qq-request--copy-owner owner)
           :cancel-function cancel-function)))
    (puthash request t qq-request--active)
    (condition-case error-data
        (progn
          (when lifecycle-owner
            (setf (qq-request-lifecycle-handle request)
                  (appkit-register-handle
                   lifecycle-owner 'qq-request request
                   #'qq-request--cancel-from-lifecycle-owner)))
          request)
      ((error quit)
       (setf (qq-request-state request) 'cancelled)
       (remhash request qq-request--active)
       (signal (car error-data) (cdr error-data))))))

(defun qq-request-active-p (request)
  "Return non-nil when REQUEST still owns asynchronous work."
  (and (qq-request-p request)
       (eq (qq-request-state request) 'active)))

(defun qq-request--retire (request state)
  "Move active REQUEST to terminal STATE exactly once."
  (when (qq-request-active-p request)
    (let ((handle (qq-request-lifecycle-handle request)))
      (setf (qq-request-state request) state
            (qq-request-token request) nil
            (qq-request-cancel-function request) nil
            (qq-request-lifecycle-handle request) nil)
      (remhash request qq-request--active)
      (when (and (appkit-handle-p handle)
                 (appkit-handle-alive-p handle))
        (appkit-retire-handle handle)))
    t))

(defun qq-request-finish (request)
  "Mark REQUEST successfully settled without invoking a callback."
  (qq-request--retire request 'settled))

(defun qq-request-fail (request)
  "Mark REQUEST failed without invoking an error callback."
  (qq-request--retire request 'failed))

(defun qq-request--owner-current-p (request)
  "Return non-nil when REQUEST may deliver account-scoped callbacks.

A string owner denotes a stable managed-account slot and deliberately survives
Native Session replacement."
  (let ((owner (qq-request-owner request)))
    (or (null owner)
        (and (qq-account-get owner) t))))

(defun qq-request--invoke (callback &rest arguments)
  "Invoke leaf CALLBACK with ARGUMENTS while isolating consumer errors."
  (when callback
    (condition-case error-data
        (apply callback arguments)
      (error
       (message "qq: native request callback failed: %s"
                (error-message-string error-data))))))

(defun qq-request--invoke-owned (owner callback &rest arguments)
  "Invoke CALLBACK with ARGUMENTS inside OWNER's state partition."
  (if owner
      (qq-runtime-with-account owner
        (apply #'qq-request--invoke callback arguments))
    (apply #'qq-request--invoke callback arguments)))

(defun qq-request--cancel-direct (request)
  "Cancel active REQUEST after its lifecycle handle has been revoked."
  (when (qq-request-active-p request)
    (let ((cancel (qq-request-cancel-function request))
          (token (qq-request-token request)))
      ;; Revoke callback ownership before adapter cancellation can reenter.
      (qq-request--retire request 'cancelled)
      (condition-case error-data
          (cond
           (cancel (funcall cancel))
           (token (qq-server-cancel token)))
        (error
         (message "qq: native request cancellation failed: %s"
                  (error-message-string error-data))))
      t)))

(defun qq-request--cancel-from-lifecycle-owner (request)
  "Cancel REQUEST after its Appkit lifecycle owner revoked the child handle."
  (qq-request--cancel-direct request))

(defun qq-request-cancel (request)
  "Cancel REQUEST locally and revoke its adapter work exactly once."
  (when (qq-request-active-p request)
    (if-let* ((handle (qq-request-lifecycle-handle request))
              ((appkit-handle-alive-p handle)))
        (progn
          (appkit-cancel-handle handle)
          t)
      (qq-request--cancel-direct request))))

(cl-defun qq-request-start
    (starter
     &key callback errback (owner nil owner-supplied-p) lifecycle-owner)
  "Start one callback-scoped request through STARTER.

STARTER is called with success and error continuations and returns its opaque
adapter token.  CALLBACK receives one successful value; ERRBACK receives an
error body and human-readable reason.  OWNER uses the scope vocabulary of
`qq-request-create' and defaults to the current UI account.  LIFECYCLE-OWNER,
when non-nil, owns the request's Appkit cancellation handle.

The request retires before invoking either leaf callback.  Late callbacks and
callbacks for a removed OWNER are inert.  Native Session replacement does not
change the stable account slot.  Product-specific replacement and projection
policy belongs to the product operation, not this lifecycle type.

A nil token is valid only when a callback settles the request synchronously.
A non-nil token is accepted work whose callback runs later on the event loop."
  (let* ((owner (if owner-supplied-p
                    owner
                  (qq-runtime-current-account-id)))
         (request (qq-request-create owner nil lifecycle-owner)))
    (condition-case error-data
        (cl-labels
            ((success
               (value)
               (when (qq-request-active-p request)
                 (if (qq-request--owner-current-p request)
                     (when (qq-request-finish request)
                       (qq-request--invoke-owned
                        (qq-request-owner request) callback value))
                   (qq-request-cancel request))))
             (failure
               (body reason)
               (when (qq-request-active-p request)
                 (if (qq-request--owner-current-p request)
                     (when (qq-request-fail request)
                       (qq-request--invoke-owned
                        (qq-request-owner request)
                        errback body reason))
                   (qq-request-cancel request)))))
          (let ((token (funcall starter #'success #'failure)))
            (when (qq-request-active-p request)
              (unless token
                (error "qq: native request starter returned without settling"))
              (setf (qq-request-token request) token)))
          request)
      ((error quit)
       (qq-request-cancel request)
       (signal (car error-data) (cdr error-data))))))

(defun qq-request-revoke-stale (&rest _ignored)
  "Cancel requests whose callback observation context is no longer current."
  (let (stale)
    (maphash
     (lambda (request _present)
       (when (and (qq-request-owner request)
                  (not (qq-request--owner-current-p request)))
         (push request stale)))
     qq-request--active)
    (dolist (request stale)
      (qq-request-cancel request))))

(defun qq-request-revoke-all ()
  "Cancel every active native request."
  (let (requests)
    (maphash (lambda (request _present) (push request requests))
             qq-request--active)
    (dolist (request requests)
      (qq-request-cancel request))))

(provide 'qq-request)

;;; qq-request.el ends here
