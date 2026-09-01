;;; qq-presence-test.el --- Tests for account presence commands -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-presence)

(ert-deftest qq-presence-standard-commands-use-closed-kinds ()
  (dolist (entry
           '((qq-presence-online . "online")
             (qq-presence-q-me . "q_me")
             (qq-presence-away . "away")
             (qq-presence-busy . "busy")
             (qq-presence-do-not-disturb . "do_not_disturb")
             (qq-presence-invisible . "invisible")))
    (let (called)
      (cl-letf (((symbol-function 'qq-core-set-presence)
                 (lambda (presence callback _errback)
                   (setq called presence)
                   (funcall callback `((presence . ,presence)))
                   'presence-request)))
        (should (eq (call-interactively (car entry)) 'presence-request))
        (should (equal called `((kind . ,(cdr entry)))))))))

(ert-deftest qq-presence-custom-keeps-u32-face-id-and-wording ()
  (let (called delivered)
    (cl-letf (((symbol-function 'qq-core-set-presence)
               (lambda (presence callback _errback)
                 (setq called presence)
                 (funcall callback `((presence . ,presence)))
                 'custom-presence-request)))
      (should
       (eq (qq-presence-custom
            4294967295 "writing Emacs Lisp")
           'custom-presence-request))
      (should
       (equal called
              '((kind . "custom") (face_id . 4294967295)
                (wording . "writing Emacs Lisp"))))
      (setq delivered called))
    (should (equal (alist-get 'wording delivered) "writing Emacs Lisp"))))

(ert-deftest qq-presence-rejects-open-or-out-of-range-values-locally ()
  (let ((called nil))
    (cl-letf (((symbol-function 'qq-core-set-presence)
               (lambda (&rest _arguments) (setq called t))))
      (should-error
       (qq-presence-set '((kind . "away") (raw_status . 30)))
       :type 'user-error)
      (should-error
       (qq-presence-custom 4294967296 "too large")
       :type 'user-error)
      (should-not called))))

(provide 'qq-presence-test)

;;; qq-presence-test.el ends here
