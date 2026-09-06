;;; qq-modes-test.el --- Tests for QQ mode-line integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'qq-modes)

(ert-deftest qq-mode-line-counts-unmuted-and-priority-mentions ()
  (cl-letf (((symbol-function 'qq-state-sessions)
             (lambda ()
               '(((unread-badge-count . 3) (muted-p . nil))
                 ((unread-badge-count . 8) (muted-p . t)
                  (unread-at-me-message-id . "11"))
                 ((unread-badge-count . 2) (muted-p . nil)
                  (unread-at-all-message-seq . "12"))))))
    (should (equal '(5 . 2) (qq-mode-line--counts)))
    (should (equal " 5" (substring-no-properties
                         (qq-mode-line-unread-unmuted))))
    (should (equal " @2" (substring-no-properties
                          (qq-mode-line-mentions))))))

(ert-deftest qq-mode-line-does-not-sum-an-unavailable-badge-as-zero ()
  (cl-letf (((symbol-function 'qq-state-sessions)
             (lambda ()
               '(((unread-badge-count . 3) (muted-p . nil))
                 ((unread-badge-count . nil) (muted-p . nil))))))
    (should (equal '(nil . 0) (qq-mode-line--counts)))
    (should-not (qq-mode-line-unread-unmuted))))

(ert-deftest qq-mode-line-mode-installs-and-removes-state-hook ()
  (let ((mode-line-misc-info nil)
        (qq-state-change-hook nil))
    (unwind-protect
        (progn
          (qq-mode-line-mode 1)
          (should (memq 'qq-mode-line-format mode-line-misc-info))
          (should (memq #'qq-mode-line-update qq-state-change-hook)))
      (qq-mode-line-mode -1))
    (should-not (memq 'qq-mode-line-format mode-line-misc-info))
    (should-not (memq #'qq-mode-line-update qq-state-change-hook))))

(ert-deftest qq-mode-line-state-hook-does-not-force-redisplay ()
  (let ((qq-mode-line-mode t)
        (qq-mode-line-string "")
        (redisplays 0))
    (cl-letf (((symbol-function 'qq-mode-line--counts)
               (lambda () '(4 . 2)))
              ((symbol-function 'force-mode-line-update)
               (lambda (&rest _) (cl-incf redisplays))))
      (qq-mode-line-update)
      (should (stringp qq-mode-line-string))
      (should (zerop redisplays)))))

(provide 'qq-modes-test)

;;; qq-modes-test.el ends here
