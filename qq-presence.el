;;; qq-presence.el --- Account presence commands for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Product-facing commands for the closed account presence protocol.  Presence
;; is a runtime command, separate from the Gateway account lifecycle phase.

;;; Code:

(require 'qq-core)
(require 'qq-protocol)

(defun qq-presence--label (presence)
  "Return a concise user-facing label for PRESENCE."
  (pcase (alist-get 'kind presence)
    ("online" "online")
    ("q_me" "Q me")
    ("away" "away")
    ("busy" "busy")
    ("do_not_disturb" "do not disturb")
    ("invisible" "invisible")
    ("custom" (format "custom (%s)" (alist-get 'wording presence)))
    (_ "unknown")))

(defun qq-presence--report-success (presence _receipt)
  "Report that PRESENCE was acknowledged."
  (message "qq: account presence set to %s" (qq-presence--label presence)))

(defun qq-presence--report-error (_response reason)
  "Report a presence request failure described by REASON."
  (message "qq: failed to set account presence: %s" reason))

(defun qq-presence-set (presence &optional callback errback)
  "Set the selected account's closed PRESENCE.

CALLBACK receives the backend acknowledgement.  ERRBACK receives its failure
body and human-readable reason."
  (setq presence
        (qq-protocol-validate-account-presence
         presence "account presence" 'user-error))
  (qq-core-set-presence
   presence
   (or callback (apply-partially #'qq-presence--report-success presence))
   (or errback #'qq-presence--report-error)))

(defun qq-presence--set-standard (kind)
  "Set standard account presence KIND."
  (qq-presence-set `((kind . ,kind))))

;;;###autoload
(defun qq-presence-online ()
  "Set the selected account presence to online."
  (interactive)
  (qq-presence--set-standard "online"))

;;;###autoload
(defun qq-presence-q-me ()
  "Set the selected account presence to Q me."
  (interactive)
  (qq-presence--set-standard "q_me"))

;;;###autoload
(defun qq-presence-away ()
  "Set the selected account presence to away."
  (interactive)
  (qq-presence--set-standard "away"))

;;;###autoload
(defun qq-presence-busy ()
  "Set the selected account presence to busy."
  (interactive)
  (qq-presence--set-standard "busy"))

;;;###autoload
(defun qq-presence-do-not-disturb ()
  "Set the selected account presence to do not disturb."
  (interactive)
  (qq-presence--set-standard "do_not_disturb"))

;;;###autoload
(defun qq-presence-invisible ()
  "Set the selected account presence to invisible."
  (interactive)
  (qq-presence--set-standard "invisible"))

;;;###autoload
(defun qq-presence-custom (face-id wording)
  "Set a custom presence with FACE-ID and WORDING."
  (interactive
   (list (read-number "Custom presence face ID: ")
         (read-string "Custom presence text: ")))
  (qq-presence-set
   `((kind . "custom") (face_id . ,face-id) (wording . ,wording))))

(provide 'qq-presence)

;;; qq-presence.el ends here
