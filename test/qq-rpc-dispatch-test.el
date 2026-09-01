;;; qq-rpc-dispatch-test.el --- Tests for service domain dispatch -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-rpc)
(require 'qq-message)
(require 'qq-read)

(defvar qq-rpc-dispatch-test--calls nil)

(defun qq-rpc-dispatch-test--event-owner (event data)
  "Record routed EVENT and DATA."
  (push (list 'event event data) qq-rpc-dispatch-test--calls))

(defun qq-rpc-dispatch-test--other-event-owner (event data)
  "Record a conflicting EVENT and DATA owner call."
  (push (list 'other event data) qq-rpc-dispatch-test--calls))

(defun qq-rpc-dispatch-test--bad-event-owner (_event _data)
  "Reject a routed test event."
  (error "invalid domain payload"))

(defun qq-rpc-dispatch-test--error-owner (body)
  "Record routed unsolicited error BODY."
  (push (list 'error body) qq-rpc-dispatch-test--calls))

(defmacro qq-rpc-dispatch-test-with-state (&rest body)
  "Run BODY with isolated dispatch tables and observations."
  (declare (indent 0) (debug t))
  `(let ((qq-rpc--event-handlers
          (make-hash-table :test #'equal))
         (qq-rpc--error-handlers
          (make-hash-table :test #'equal))
         (qq-rpc-dispatch-test--calls nil))
     ,@body))

(ert-deftest qq-rpc-routes-only-the-exact-event-owner ()
  (qq-rpc-dispatch-test-with-state
    (qq-rpc-register-event
     "example.changed" #'qq-rpc-dispatch-test--event-owner)
    (qq-rpc--handle-transport-event
     "example.unknown" '((ignored . t)))
    (should-not qq-rpc-dispatch-test--calls)
    (qq-rpc--handle-transport-event
     "example.changed"
     `((value . ["opaque" ,qq-server-wire-null])))
    (should
     (equal qq-rpc-dispatch-test--calls
            '((event "example.changed" ((value . ("opaque" nil)))))))))

(ert-deftest qq-rpc-registration-is-owned-and-reload-safe ()
  (qq-rpc-dispatch-test-with-state
    (qq-rpc-register-event
     "example.changed" #'qq-rpc-dispatch-test--event-owner)
    (should
     (eq (qq-rpc-register-event
          "example.changed" #'qq-rpc-dispatch-test--event-owner)
         #'qq-rpc-dispatch-test--event-owner))
    (should-error
     (qq-rpc-register-event
      "example.changed" #'qq-rpc-dispatch-test--other-event-owner))
    (should-error
     (qq-rpc-register-event "" #'ignore))
    (should-error
     (qq-rpc-register-event "example.lambda" (lambda (&rest _))))))

(ert-deftest qq-rpc-domain-error-is-not-a-protocol-violation ()
  (qq-rpc-dispatch-test-with-state
    (let (violations)
      (qq-rpc-register-event
       "example.changed" #'qq-rpc-dispatch-test--bad-event-owner)
      (cl-letf (((symbol-function 'qq-server--protocol-violation)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format format-string arguments) violations))))
        (should-error
         (qq-rpc--handle-transport-event
          "example.changed" '((invalid . t)))))
      (should-not violations))))

(ert-deftest qq-rpc-routes-unsolicited-errors-by-code ()
  (qq-rpc-dispatch-test-with-state
    (qq-rpc-register-error
     "example_lagged" #'qq-rpc-dispatch-test--error-owner)
    (qq-rpc--handle-transport-error
     '((code . "other_error") (message . "ignored")))
    (should-not qq-rpc-dispatch-test--calls)
    (qq-rpc--handle-transport-error
     '((code . "example_lagged") (message . "missed 2")))
    (should
     (equal qq-rpc-dispatch-test--calls
            '((error ((code . "example_lagged")
                      (message . "missed 2"))))))))

(ert-deftest qq-rpc-domain-vocabulary-has-explicit-owners ()
  (dolist (route
           '(("gateway.ready" . qq-account--handle-event)
             ("account.changed" . qq-account--handle-event)
             ("account.removed" . qq-account--handle-event)
             ("resource.changed" . qq-resource--handle-event)
             ("resource.removed" . qq-resource--handle-event)
             ("media.changed" . qq-remote-media--handle-event)
             ("media.removed" . qq-remote-media--handle-event)
             ("attachment.changed" . qq-attachment--handle-event)
             ("message.received" . qq-message--handle-event)
             ("dataline.message_received" . qq-message--handle-event)
             ("message.recalled" . qq-message--handle-event)
             ("message.reaction_changed" . qq-message--handle-event)
             ("message.essence_changed" . qq-message--handle-event)
             ("conversation.read_state_changed" . qq-read--handle-event)
             ("runtime.resync_required"
              . qq-account--handle-resync-required)))
    (should
     (eq (gethash (car route) qq-rpc--event-handlers)
         (cdr route))))
  (dolist (route
           '(("event_stream_lagged" . qq-account--handle-protocol-error)))
    (should
     (eq (gethash (car route) qq-rpc--error-handlers)
         (cdr route))))
  (dolist (consumer
           '(qq-resource--handle-projection-resync
             qq-remote-media--handle-projection-resync
             qq-attachment--handle-projection-resync))
    (should (memq consumer qq-account-projection-resync-hook)))
  (should
   (memq #'qq-rpc--handle-transport-event
         qq-server-event-hook))
  (dolist (old-consumer
           '(qq-account--handle-event
             qq-resource--handle-event
             qq-remote-media--handle-event
             qq-attachment--handle-event
             qq-message--handle-event))
    (should-not (memq old-consumer qq-server-event-hook))))

(provide 'qq-rpc-dispatch-test)
;;; qq-rpc-dispatch-test.el ends here
