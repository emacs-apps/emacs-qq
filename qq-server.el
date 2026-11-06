;;; qq-server.el --- QQ Gateway connection -*- lexical-binding: t; -*-
;;; Code:
(require 'appkit-gateway)
(require 'qq-customize)
(defconst qq-server-protocol-version 31 "Native QQ Gateway protocol version.")
(defvar qq-server-event-hook nil "Hook called with EVENT and DATA from the QQ Gateway.")
(defvar qq-server-protocol-error-hook nil "Hook called with an unsolicited QQ Gateway error.")
(defvar qq-server-state-hook nil "Hook called with the QQ connection state.")
(defvar qq-server--client
  (appkit-gateway-create :protocol-version qq-server-protocol-version
                        :event-hook 'qq-server-event-hook
                        :protocol-error-hook 'qq-server-protocol-error-hook
                        :state-hook 'qq-server-state-hook)
  "QQ transport instance; account lifetimes belong to the service.")
(defconst qq-server-wire-null appkit-gateway-wire-null)
(defalias 'qq-server-wire-null-p #'appkit-gateway-wire-null-p)
(defalias 'qq-server-wire-object-p #'appkit-gateway-wire-object-p)
(defalias 'qq-server-wire-exact-object-keys-p #'appkit-gateway-wire-exact-object-keys-p)
(defalias 'qq-server-wire--copy #'appkit-gateway-wire--copy)
(defalias 'qq-server-value-copy #'appkit-gateway-value-copy)
(defalias 'qq-server-wire-domain-copy #'appkit-gateway-wire-domain-copy)
(defalias 'qq-server-wire-array #'appkit-gateway-wire-array)
(defalias 'qq-server-wire-decode #'appkit-gateway-wire-decode)
(defalias 'qq-server--run-hook #'appkit-gateway--run-hook)
(defalias 'qq-server--json-encode #'appkit-gateway--json-encode)
(defalias 'qq-server--json-decode #'appkit-gateway--json-decode)
(defalias 'qq-server--frame-text #'appkit-gateway--frame-text)
(defalias 'qq-server--exact-object-keys-p #'appkit-gateway--exact-object-keys-p)
(defalias 'qq-server--cancel-entry-timer #'appkit-gateway--cancel-entry-timer)
(defalias 'qq-server--invoke-callback #'appkit-gateway--invoke-callback)
(defalias 'qq-server--clear-owner-token #'appkit-gateway--clear-owner-token)

(defun qq-server-state nil
  "Return the current native service connection state."
  (appkit-gateway-state qq-server--client))

(defun qq-server-gateway-instance-id nil
  "Return the current authenticated Gateway instance ID, or nil."
  (appkit-gateway-gateway-instance-id qq-server--client))

(defun qq-server-capabilities nil
  "Return a copy of capabilities advertised by the current Gateway."
  (appkit-gateway-capabilities qq-server--client))

(defun qq-server-ready-accounts nil
  "Return a copy of the accounts in the latest `gateway.ready' snapshot."
  (appkit-gateway-ready-accounts qq-server--client))

(defun qq-server-running-p nil
  "Return non-nil while the native service connection is active."
  (appkit-gateway-running-p qq-server--client))

(defun qq-server-ready-p nil
  "Return non-nil when native service business requests may be sent."
  (appkit-gateway-ready-p qq-server--client))

(defun qq-server-cancel (id)
  "Revoke ID's callbacks and unsent work, without canceling a sent Gateway RPC."
  (appkit-gateway-cancel qq-server--client id))
(defun qq-server--protocol-violation (format-string &rest arguments)
  "Reject an invalid QQ event described by FORMAT-STRING and ARGUMENTS."
  (apply #'appkit-gateway--protocol-violation qq-server--client format-string arguments))

(defun qq-server-send (method params &optional callback errback allow-before-ready)
  "Send METHOD and PARAMS on QQ; CALLBACK and ERRBACK settle the returned ID."
  (setf (appkit-gateway--timeout qq-server--client) qq-server-request-timeout)
  (appkit-gateway-send qq-server--client method params callback errback allow-before-ready))

(defun qq-server-start ()
  "Connect the QQ frontend to its independently managed service."
  (interactive)
  (setf (appkit-gateway--protocol-version qq-server--client) qq-server-protocol-version
        (appkit-gateway--websocket-url qq-server--client) qq-server-websocket-url
        (appkit-gateway--auth-token-file qq-server--client) qq-server-auth-token-file
        (appkit-gateway--client-name qq-server--client) qq-server-client-name
        (appkit-gateway--timeout qq-server--client) qq-server-request-timeout
        (appkit-gateway--handshake-timeout qq-server--client) qq-server-ready-timeout
        (appkit-gateway--reconnect-delay qq-server--client) qq-server-reconnect-delay
        (appkit-gateway--reconnect-max-attempts qq-server--client) qq-server-reconnect-max-attempts)
  (appkit-gateway-start qq-server--client))

(defun qq-server-stop ()
  "Disconnect the QQ frontend; service accounts stay online."
  (interactive)
  (appkit-gateway-stop qq-server--client))

(provide 'qq-server)
;;; qq-server.el ends here
