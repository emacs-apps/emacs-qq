;;; qq-server-wire-test.el --- Tests for service wire values -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-server)

(ert-deftest qq-server-wire-decode-preserves-json-kinds ()
  (let* ((decoded
          (qq-server-wire-decode
           "{\"null\":null,\"array\":[],\"object\":{}}"))
         (null-entry (assq 'null decoded))
         (array-entry (assq 'array decoded))
         (object-entry (assq 'object decoded)))
    (should (qq-server-wire-null-p (cdr null-entry)))
    (should (vectorp (cdr array-entry)))
    (should (= (length (cdr array-entry)) 0))
    (should (null (cdr object-entry)))
    (should (qq-server-wire-object-p (cdr object-entry)))
    (should-not (qq-server-wire-object-p (cdr array-entry)))
    (should-not (qq-server-wire-object-p (cdr null-entry)))))

(ert-deftest qq-server-wire-array-normalizes-only-its-own-level ()
  (let* ((text (copy-sequence "owned"))
         (wire (vector text (vector qq-server-wire-null)))
         (domain (qq-server-wire-array wire "test values")))
    (should (listp domain))
    (should (vectorp (cadr domain)))
    (should (qq-server-wire-null-p (aref (cadr domain) 0)))
    (should-not (eq (car domain) text))
    (aset (car domain) 0 ?O)
    (should (equal text "owned")))
  ;; Empty objects decode to nil with alist objects and must not masquerade as
  ;; empty arrays at a raw wire boundary.
  (should-error (qq-server-wire-array nil "empty test values")
                :type 'error)
  (should-error
   (qq-server-wire-array '((field . "object")) "test values")
   :type 'error)
  (should (equal (qq-server-wire-array nil "domain values" t) nil))
  (should (equal (qq-server-wire-array '("a" "b") "domain values" t)
                 '("a" "b")))
  (should-error (qq-server-wire-array qq-server-wire-null "test values")
                :type 'error))

(ert-deftest qq-server-wire-domain-copy-normalizes-and-owns-values ()
  (let* ((text (copy-sequence "nested"))
         (wire `((nullable . ,qq-server-wire-null)
                 (items . [,text [,qq-server-wire-null]])))
         (domain (qq-server-wire-domain-copy wire)))
    (should (null (alist-get 'nullable domain)))
    (should (equal (alist-get 'items domain) '("nested" (nil))))
    (should-not (eq (car (alist-get 'items domain)) text))
    (aset (car (alist-get 'items domain)) 0 ?N)
    (should (equal text "nested"))))

(ert-deftest qq-server-value-copy-recurses-through-supported-containers ()
  (let* ((text (copy-sequence "value"))
         (table (make-hash-table :test #'equal))
         (source (list (vector text) table)))
    (puthash "key" (cons text (vector text)) table)
    (let* ((copy (qq-server-value-copy source))
           (copy-vector (car copy))
           (copy-table (cadr copy))
           (copy-value (gethash "key" copy-table)))
      (should-not (eq copy source))
      (should-not (eq copy-vector (car source)))
      (should-not (eq copy-table table))
      (should-not (eq (aref copy-vector 0) text))
      (should-not (eq (car copy-value) text))
      (should-not (eq (aref (cdr copy-value) 0) text))
      (aset (aref copy-vector 0) 0 ?V)
      (aset (car copy-value) 0 ?K)
      (aset (aref (cdr copy-value) 0) 0 ?H)
      (should (equal text "value")))))

(provide 'qq-server-wire-test)
;;; qq-server-wire-test.el ends here
