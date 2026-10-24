;;; qq-timeline-test.el --- Chat Surfaces on Gateway timelines -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-message-test)
(require 'qq-chat-test)
(require 'qq-timeline)

(defvar qq-timeline-test--calls nil
  "Gateway calls made by the test, newest first, as (METHOD PARAMS CALLBACK).")

(defmacro qq-timeline-test-with-chat (&rest body)
  "Run BODY in a private chat whose Gateway calls collect in `qq-timeline-test--calls'."
  (declare (indent 0) (debug t))
  `(qq-message-test-with-state
     (let ((qq-chat-show-prompt-avatar nil)
           (qq-auto-mark-read nil)
           (qq-timeline-test--calls nil))
       (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                 ((symbol-function 'qq-server-capabilities)
                  (lambda () (append '("timeline.open" "timeline.paginate" "timeline.close")
                                     qq-message-test-capabilities)))
                 ((symbol-function 'qq-server-send)
                  (lambda (method params callback _errback &optional _early)
                    (push (list method params callback) qq-timeline-test--calls)
                    method)))
         (with-temp-buffer
           (qq-chat-mode)
           (qq-runtime-bind-account "slot-a")
           (setq qq-chat--session-key "private:10001")
           (qq-chat--ensure-view)
           ,@body)))))

(defun qq-timeline-test--item (row-key message-id)
  "Return a Gateway timeline item for ROW-KEY carrying MESSAGE-ID."
  (qq-message-test-native-row
   (qq-message-test-event :message-id message-id :sequence row-key) row-key))

(defun qq-timeline-test--call (method)
  "Return the newest Gateway call of METHOD."
  (seq-find (lambda (call) (equal (car call) method)) qq-timeline-test--calls))

(defun qq-timeline-test--reply (method result)
  "Answer the newest METHOD call with RESULT and settle every loop."
  (funcall (nth 2 (qq-timeline-test--call method)) result)
  (qq-chat-test--drain-history))

(defun qq-timeline-test--open (items &rest flags)
  "Open the current chat at latest and answer with ITEMS and FLAGS."
  (qq-chat--load-initial-history (current-buffer) qq-chat--session-key)
  (qq-chat-test--drain-history)
  (qq-timeline-test--reply
   "timeline.open"
   `((timeline_id . "1") (items . ,(vconcat items))
     (has_older . ,(if (plist-get flags :has-older) t :false))
     (has_newer . :false) (center_key))))

(defun qq-timeline-test--diff (ops)
  "Deliver Gateway diff OPS for timeline 1 and settle every loop."
  (qq-timeline--handle-event
   "timeline.diff"
   `((account_id . "slot-a") (timeline_id . "1") (revision . 1)
     (ops . ,(vconcat ops)) (has_older . :false) (has_newer . :false)))
  (qq-chat-test--drain-history))

(defun qq-timeline-test--rows ()
  (plist-get (qq-chat--model) :rows))

(ert-deftest qq-timeline-open-at-latest-renders-the-gateway-rows ()
  (qq-timeline-test-with-chat
    (qq-chat--load-initial-history (current-buffer) qq-chat--session-key)
    (qq-chat-test--drain-history)
    (let ((params (nth 1 (qq-timeline-test--call "timeline.open"))))
      (should (equal (alist-get 'focus params) '((kind . "latest"))))
      (should (equal (alist-get 'conversation params)
                     '((kind . "private") (peer_uin . "10001")))))
    (should (qq-chat--loading))
    (qq-timeline-test--reply
     "timeline.open"
     `((timeline_id . "1")
       (items . ,(vector (qq-timeline-test--item "100" "7348923749823749801")
                         (qq-timeline-test--item "101" "7348923749823749802")))
       (has_older . t) (has_newer . :false) (center_key)))
    (should-not (qq-chat--loading))
    (should (equal (qq-timeline-test--rows)
                   '(("100" . "7348923749823749801") ("101" . "7348923749823749802"))))
    (should (equal (mapcar #'qq-state-message-anchor (qq-chat--timeline-messages))
                   '("7348923749823749801" "7348923749823749802")))
    (should (plist-get (qq-chat--model) :has-older))))

(ert-deftest qq-timeline-diffs-insert-and-remove-rows-by-key ()
  (qq-timeline-test-with-chat
    (qq-timeline-test--open (list (qq-timeline-test--item "100" "7348923749823749801")
                                  (qq-timeline-test--item "101" "7348923749823749802")))
    (qq-timeline-test--diff
     `(((op . "remove") (key . "100"))
       ((op . "upsert") (after . "101")
        (item . ,(qq-timeline-test--item "102" "7348923749823749803")))))
    (should (equal (qq-timeline-test--rows)
                   '(("101" . "7348923749823749802") ("102" . "7348923749823749803"))))
    (should (equal (mapcar #'qq-state-message-anchor (qq-chat--timeline-messages))
                   '("7348923749823749802" "7348923749823749803")))))

(ert-deftest qq-timeline-diff-before-the-open-reply-is-applied-after-it ()
  (qq-timeline-test-with-chat
    (qq-chat--load-initial-history (current-buffer) qq-chat--session-key)
    (qq-chat-test--drain-history)
    (qq-timeline-test--diff
     `(((op . "upsert") (after . "100")
        (item . ,(qq-timeline-test--item "101" "7348923749823749802")))))
    (qq-timeline-test--reply
     "timeline.open"
     `((timeline_id . "1")
       (items . ,(vector (qq-timeline-test--item "100" "7348923749823749801")))
       (has_older . :false) (has_newer . :false) (center_key)))
    (should (equal (mapcar #'car (qq-timeline-test--rows)) '("100" "101")))))

(ert-deftest qq-timeline-older-page-at-the-edge-stops-paging ()
  (qq-timeline-test-with-chat
    (qq-timeline-test--open (list (qq-timeline-test--item "100" "7348923749823749801"))
                            :has-older t)
    (qq-chat-load-older-messages t)
    (qq-chat-test--drain-history)
    (should (equal (alist-get 'direction (nth 1 (qq-timeline-test--call "timeline.paginate")))
                   "older"))
    (should (qq-chat--loading))
    (appkit-surface-send (appkit-current-surface) '(qq-timeline-paged "retired" older edge nil))
    (should (qq-chat--loading))
    (should (plist-get (qq-chat--model) :has-older))
    (qq-timeline-test--reply "timeline.paginate" '((outcome . "edge")))
    (should-not (qq-chat--loading))
    (should-not (plist-get (qq-chat--model) :has-older))))

(ert-deftest qq-timeline-jump-opens-around-the-target-and-moves-to-it ()
  (qq-timeline-test-with-chat
    (qq-timeline-test--open (list (qq-timeline-test--item "200" "7348923749823749899")))
    (switch-to-buffer (current-buffer))
    (qq-chat-goto-message '((kind . "row") (row_key . "101")) t)
    (qq-chat-test--drain-history)
    (let ((params (nth 1 (qq-timeline-test--call "timeline.open"))))
      (should (equal (alist-get 'focus params)
                     '((kind . "around") (center . ((kind . "row") (row_key . "101")))))))
    ;; Opening a new focus closes the old timeline.
    (should (qq-timeline-test--call "timeline.close"))
    (qq-timeline-test--reply
     "timeline.open"
     `((timeline_id . "2")
       (items . ,(vector (qq-timeline-test--item "100" "7348923749823749801")
                         (qq-timeline-test--item "101" "7348923749823749802")))
       (has_older . :false) (has_newer . t) (center_key . "101")))
    (should-not qq-chat--pending-jump-center)
    (should (equal (get-text-property (point) 'qq-chat-message-anchor)
                   "7348923749823749802"))))

(ert-deftest qq-timeline-undeliverable-diff-closes-the-timeline ()
  (qq-timeline-test-with-chat
    (qq-timeline-test--open (list (qq-timeline-test--item "100" "7348923749823749801")))
    (setq qq-timeline-test--calls nil)
    (appkit-surface-stop (appkit-current-surface))
    (qq-timeline-test--diff
     `(((op . "upsert") (after . "100")
        (item . ,(qq-timeline-test--item "101" "7348923749823749802")))))
    (should (equal (alist-get 'timeline_id (nth 1 (qq-timeline-test--call "timeline.close")))
                   "1"))))

(ert-deftest qq-timeline-appends-only-unconfirmed-sends-after-the-rows ()
  (qq-timeline-test-with-chat
    (qq-timeline-test--open (list (qq-timeline-test--item "200" "7348923749823749899")))
    (let ((older-sent `((local-id . "local-old") (server-id . "7348923749823749001")
                        (session-key . "private:10001") (status . sent)
                        (sender-name . "Me") (time . 1) (raw-message . "older")))
          (pending `((local-id . "local-new") (session-key . "private:10001")
                     (status . pending) (sender-name . "Me") (time . 9)
                     (raw-message . "sending"))))
      (puthash "private:10001"
               (append (list older-sent) (qq-state-session-messages "private:10001")
                       (list pending))
               qq-state--messages-by-session)
      ;; A confirmed send older than the window is not shown after latest.
      (should (equal (mapcar #'qq-state-message-anchor (qq-chat--timeline-messages))
                     '("7348923749823749899" "local-new"))))))

(ert-deftest qq-timeline-stop-releases-idle-window-without-a-diff ()
  (qq-timeline-test-with-chat
    (qq-timeline-test--open nil)
    (appkit-surface-stop (appkit-current-surface))
    (sleep-for 0.001)
    (should (equal (alist-get 'timeline_id (nth 1 (qq-timeline-test--call "timeline.close")))
                   "1"))))

(ert-deftest qq-timeline-replaced-open-closes-its-own-remote-window ()
  (dolist (stop '(nil t))
    (qq-timeline-test-with-chat
      (qq-chat--load-initial-history (current-buffer) qq-chat--session-key)
      (qq-chat-test--drain-history)
      (let ((old (qq-timeline-test--call "timeline.open")))
        (when stop (appkit-surface-stop (appkit-current-surface)))
        (qq-timeline-test--open nil)
        (funcall (nth 2 old) '((timeline_id . "orphan") (items . [])
                              (has_older . :false) (has_newer . :false)))
        (qq-chat-test--drain-history)
        (should (equal (alist-get 'timeline_id (nth 1 (qq-timeline-test--call "timeline.close")))
                       "orphan"))
        (should (equal (plist-get (qq-chat--model) :timeline) "1"))))))

(provide 'qq-timeline-test)

;;; qq-timeline-test.el ends here
