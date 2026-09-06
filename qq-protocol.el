;;; qq-protocol.el --- Wire value contracts for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Small, dependency-free decoders shared by the transport, API, and state
;; layers.  Identity values are intentionally not "normalized": accepting a
;; numeric NT message snowflake after JSON decoding would preserve an already
;; rounded value as if it were authoritative.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst qq-protocol--max-safe-integer 9007199254740991
  "Largest integer represented exactly by a JSON/JavaScript number.")

(defconst qq-protocol--max-uint64-decimal
  "18446744073709551615"
  "Largest canonical unsigned 64-bit decimal wire value.")

(defun qq-protocol-non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun qq-protocol-uint32-p (value)
  "Return non-nil when VALUE is an exact unsigned 32-bit integer."
  (and (integerp value) (<= 0 value #xffffffff)))

(defun qq-protocol--nonzero-decimal-string-p (value)
  "Return non-nil when VALUE is a positive decimal identity string."
  (and (stringp value)
       (string-match-p "\\`[1-9][0-9]*\\'" value)))

(defun qq-protocol-uint64-decimal-p (value &optional allow-zero)
  "Return non-nil when VALUE is canonical uint64 decimal text.

By default zero is rejected.  When ALLOW-ZERO is non-nil, the exact string
`0' is accepted.  Numeric values and strings with leading zeroes are never
coerced or accepted."
  (or (and allow-zero (equal value "0"))
      (and (qq-protocol--nonzero-decimal-string-p value)
           (let ((length (length value))
                 (maximum-length
                  (length qq-protocol--max-uint64-decimal)))
             (or (< length maximum-length)
                 (and (= length maximum-length)
                      (not (string-lessp
                            qq-protocol--max-uint64-decimal value))))))))

(defun qq-protocol-user-uin-p (value)
  "Return non-nil when VALUE is a canonical nonzero uint64 QQ user UIN."
  (qq-protocol-uint64-decimal-p value))

(defun qq-protocol-decimal-less-p (left right)
  "Return non-nil when canonical uint64 decimal LEFT is less than RIGHT.

Zero is accepted, but numeric values, leading zeroes, and out-of-range text
are rejected instead of being coerced."
  (unless (qq-protocol-uint64-decimal-p left t)
    (error "qq: decimal comparison requires uint64 LEFT string, got %S" left))
  (unless (qq-protocol-uint64-decimal-p right t)
    (error "qq: decimal comparison requires uint64 RIGHT string, got %S" right))
  (or (< (length left) (length right))
      (and (= (length left) (length right))
           (string-lessp left right))))

(defun qq-protocol-decimal-string-compare (left right)
  "Compare canonical nonzero uint64 strings LEFT and RIGHT exactly.

Return -1, 0, or 1.  Length is compared before lexicographic order, which
cannot lose NT sequence precision."
  (unless (qq-protocol-uint64-decimal-p left)
    (error "qq: decimal comparison requires uint64 LEFT string, got %S" left))
  (unless (qq-protocol-uint64-decimal-p right)
    (error "qq: decimal comparison requires uint64 RIGHT string, got %S" right))
  (cond
   ((< (length left) (length right)) -1)
   ((> (length left) (length right)) 1)
   ((string-lessp left right) -1)
   ((string-lessp right left) 1)
   (t 0)))

(defun qq-protocol-group-uin-p (value)
  "Return non-nil when VALUE is a canonical nonzero uint64 QQ group UIN."
  (qq-protocol-uint64-decimal-p value))

(defun qq-protocol-message-id-p (value)
  "Return non-nil when VALUE is a canonical NT message snowflake string.

The hard-cut wire identity is a nonzero uint64 decimal string with no leading
zero."
  (qq-protocol-uint64-decimal-p value))

(defun qq-protocol-message-sequence-p (value)
  "Return non-nil when VALUE is a canonical nonzero message sequence string.

Sequences are conversation-local uint64 identities.  They stay decimal
strings at the Elisp boundary just like message snowflakes, but this separate
predicate prevents the two domains from being used interchangeably."
  (qq-protocol-uint64-decimal-p value))

(defun qq-protocol-optional-message-id (value &optional context)
  "Return optional message-id VALUE after validating its wire representation.

Nil remains nil.  Every non-nil value must be a canonical nonzero decimal
string with no leading zero.
CONTEXT is included in protocol errors."
  (cond
   ((null value) nil)
   ((qq-protocol-message-id-p value) value)
   (t
    (error "qq: %s requires message_id as a canonical nonzero decimal string, got %S"
           (or context "protocol payload") value))))

(defun qq-protocol--closed-object-p (value keys)
  "Return non-nil when VALUE is an alist with exactly unique KEYS."
  (and (consp value)
       (proper-list-p value)
       (cl-every (lambda (cell)
                   (and (consp cell) (symbolp (car cell))))
                 value)
       (let ((actual (mapcar #'car value)))
         (and (= (length actual) (length keys))
              (= (length actual)
                 (length (delete-dups (copy-sequence actual))))
              (null (seq-difference actual keys))))))

(defconst qq-protocol-account-presence-kinds
  '("online" "q_me" "away" "busy" "do_not_disturb" "invisible")
  "Closed standard account presence kinds.")

(defun qq-protocol-account-presence-p (value)
  "Return non-nil when VALUE is one closed account presence object."
  (let ((kind (and (listp value) (alist-get 'kind value))))
    (cond
     ((member kind qq-protocol-account-presence-kinds)
      (qq-protocol--closed-object-p value '(kind)))
     ((equal kind "custom")
      (and (qq-protocol--closed-object-p value '(kind face_id wording))
           (let ((face-id (alist-get 'face_id value)))
             (and (integerp face-id) (<= 0 face-id #xffffffff)))
           (stringp (alist-get 'wording value))))
     (t nil))))

(defun qq-protocol-validate-account-presence
    (value &optional context error-symbol)
  "Return a copy of closed account presence VALUE after validation.

CONTEXT is included in the diagnostic.  ERROR-SYMBOL defaults to `error'."
  (unless (qq-protocol-account-presence-p value)
    (signal (or error-symbol 'error)
            (list
             (format "qq: %s requires a closed account presence, got %S"
                     (or context "protocol payload") value))))
  (copy-tree value))

(defun qq-protocol-emacs-session-locator-p (value)
  "Return non-nil when VALUE is a closed Emacs session locator."
  (pcase (and (consp value) (alist-get 'kind value))
    ("group"
     (and (qq-protocol--closed-object-p value '(kind group_id))
          (qq-protocol-group-uin-p (alist-get 'group_id value))))
    ("private"
     (and (qq-protocol--closed-object-p value '(kind user_id))
          (qq-protocol--nonzero-decimal-string-p
           (alist-get 'user_id value))))
    ("guild-channel"
     (and (qq-protocol--closed-object-p
           value '(kind guild_id channel_id))
          (qq-protocol--nonzero-decimal-string-p
           (alist-get 'guild_id value))
          (qq-protocol--nonzero-decimal-string-p
           (alist-get 'channel_id value))))
    ("dataline"
     (and (qq-protocol--closed-object-p
           value '(kind peer_uid variant))
          (let ((peer-uid (alist-get 'peer_uid value)))
            (and (stringp peer-uid)
                 (not (string-empty-p peer-uid))))
          (member (alist-get 'variant value) '("desktop" "mobile"))))
    ("service"
     (and (qq-protocol--closed-object-p value '(kind peer_uid))
          (let ((peer-uid (alist-get 'peer_uid value)))
            (and (stringp peer-uid)
                 (not (string-empty-p peer-uid))))))
    (_ nil)))

(defun qq-protocol-emacs-chat-locator-p (value)
  "Return non-nil when VALUE is a searchable group/private locator."
  (pcase (and (consp value) (alist-get 'kind value))
    ("group"
     (and (qq-protocol--closed-object-p value '(kind group_id))
          (qq-protocol-group-uin-p (alist-get 'group_id value))))
    ("private"
     (and (qq-protocol--closed-object-p value '(kind user_id))
          (qq-protocol--nonzero-decimal-string-p
           (alist-get 'user_id value))))
    (_ nil)))

(defun qq-protocol-validate-emacs-session-locator
    (value &optional context error-symbol)
  "Return a copy of closed Emacs session locator VALUE after validation.

CONTEXT is included in the diagnostic.  ERROR-SYMBOL defaults to `error'."
  (unless (qq-protocol-emacs-session-locator-p value)
    (signal (or error-symbol 'error)
            (list
             (format "qq: %s requires a closed Emacs session locator, got %S"
                     (or context "protocol payload") value))))
  (copy-tree value))

(defun qq-protocol-json-true-p (value)
  "Return non-nil only when wire VALUE explicitly represents JSON true."
  (or (eq value t)
      (and (numberp value) (not (zerop value)))
      (and (stringp value)
           (member (downcase value) '("true" "1" "yes")))))

(provide 'qq-protocol)

;;; qq-protocol.el ends here
