;;; qq-protocol-test.el --- Tests for qq-protocol -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-protocol)

(ert-deftest qq-protocol-message-id-is-canonical-nonzero-decimal-string ()
  (should (equal (qq-protocol-optional-message-id "9007199254742007089")
                 "9007199254742007089"))
  (should-not (qq-protocol-optional-message-id nil))
  (dolist (value '("" "0" "00" "01" "1.0" "-1"
                   "18446744073709551616" 9007199254742007089))
    (should-not (qq-protocol-message-id-p value))
    (should-error (qq-protocol-optional-message-id value))))

(ert-deftest qq-protocol-json-true-distinguishes-decoded-false ()
  (dolist (value '(t 1 "true" "1" "yes"))
    (should (qq-protocol-json-true-p value)))
  (dolist (value '(nil :false :null 0 "false" "0" "no"))
    (should-not (qq-protocol-json-true-p value))))

(ert-deftest qq-protocol-group-uin-is-canonical-uint64-string ()
  (should (qq-protocol-group-uin-p "1"))
  (should (qq-protocol-group-uin-p "18446744073709551615"))
  (dolist (value '("0" "01" "18446744073709551616" 20001 nil))
    (should-not (qq-protocol-group-uin-p value))))

(ert-deftest qq-protocol-emacs-chat-locator-uses-uint64-group-uin ()
  (should
   (qq-protocol-emacs-chat-locator-p
    '((kind . "group") (group_id . "8209413637"))))
  (should-not
   (qq-protocol-emacs-chat-locator-p
    '((kind . "group") (group_id . "18446744073709551616")))))

(ert-deftest qq-protocol-emacs-session-locator-is-a-closed-union ()
  (dolist
      (locator
       '(((kind . "group") (group_id . "20001"))
         ((kind . "private") (user_id . "10001"))
         ((kind . "dataline")
          (peer_uid . "device-1") (variant . "desktop"))
         ((kind . "dataline")
          (peer_uid . "device-2") (variant . "mobile"))
         ((kind . "service") (peer_uid . "u_mail"))))
    (let ((validated
           (qq-protocol-validate-emacs-session-locator locator "event.chat")))
      (should (equal validated locator))
      (should-not (eq validated locator))))
  (dolist
      (locator
       '(nil
         ((kind . "group") (group_id . 20001))
         ((kind . "group") (group_id . "18446744073709551616"))
         ((kind . "group") (group_id . "20001") (extra . t))
         ((kind . "private") (user_id . ""))
         ((kind . "private") (user_id . "0"))
         ((kind . "private") (user_id . "01"))
         ((kind . "private") (user_id . 10001))
         ((kind . "dataline") (peer_uid . "device-1"))
         ((kind . "dataline")
          (peer_uid . "device-1") (variant . "tablet"))
         ((kind . "service") (peer_uid . ""))
         ((kind . "channel") (peer_uid . "opaque"))))
    (should-not (qq-protocol-emacs-session-locator-p locator))
    (should-error
     (qq-protocol-validate-emacs-session-locator locator "event.chat"))))

(ert-deftest qq-protocol-account-presence-is-a-closed-tagged-union ()
  (dolist (kind qq-protocol-account-presence-kinds)
    (let* ((presence `((kind . ,kind)))
           (validated
            (qq-protocol-validate-account-presence presence "presence")))
      (should (equal validated presence))
      (should-not (eq validated presence))))
  (should
   (qq-protocol-account-presence-p
    '((kind . "custom") (face_id . 4294967295)
      (wording . "writing Emacs Lisp"))))
  (dolist
      (invalid
       '(((kind . "away") (raw_status . 30))
         ((kind . "custom") (face_id . "123") (wording . "x"))
         ((kind . "custom") (face_id . 4294967296) (wording . "x"))
         ((kind . "custom") (face_id . 1) (wording . "x") (face_type . 1))
         ((kind . "unknown"))))
    (should-not (qq-protocol-account-presence-p invalid))
    (should-error
     (qq-protocol-validate-account-presence invalid "presence"))))

(provide 'qq-protocol-test)

;;; qq-protocol-test.el ends here
