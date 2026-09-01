;;; qq-read-test.el --- Tests for authoritative QQ read state -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-read)

(defun qq-read-test-state
    (&optional message-count badge-count first-unread mentions)
  "Return one group read state with four independent unread projections."
  (copy-tree
   `((conversation . ((kind . "group") (group_uin . "8209413637")))
     (read_sequence . "90")
     (latest_sequence . "100")
     (unread . ((message_count . ,(or message-count '((status . "unknown"))))
                (badge_count . ,(or badge-count '((status . "unknown"))))
                (first_unread . ,(or first-unread '((status . "unknown"))))
                (mentions . ,(or mentions '((status . "unknown")))))))))

(defun qq-read-test-exact-message-count (&optional count)
  "Return an exact ordinary Message Count projection."
  `((status . "exact") (count . ,(or count 3))))

(defun qq-read-test-exact-first-unread (&optional count)
  "Return an exact first-unread projection for COUNT ordinary messages."
  `((status . "exact")
    (position . ,(and (> (or count 3) 0)
                      '((sequence . "91")
                        (message_id . "7348923749823749823"))))))

(defun qq-read-test-exact-mentions (&optional count)
  "Return an exact mention projection for COUNT ordinary messages."
  `((status . "exact")
    (at_me . ,(and (> (or count 3) 0)
                   '((sequence . "93") (message_id . nil))))
    (at_all . nil)))

(defun qq-read-test-exact-badge (&optional count)
  "Return an exact Managed Account badge projection with COUNT items."
  `((status . "exact") (count . ,(or count 5))))

(defun qq-read-test-exact-state (&optional count badge-count)
  "Return one protocol-v19 state with independent Exact components."
  (let ((count (or count 3)))
    (qq-read-test-state
     (qq-read-test-exact-message-count count)
     (qq-read-test-exact-badge (or badge-count 5))
     (qq-read-test-exact-first-unread count)
     (qq-read-test-exact-mentions count))))

(defun qq-read-test-private-state (peer-uin read latest)
  "Return one private state for PEER-UIN with READ and LATEST cursors."
  `((conversation . ((kind . "private")
                     (peer_uid . "u_peer")
                     ,@(when peer-uin `((peer_uin . ,peer-uin)))))
    (read_sequence . ,read)
    (latest_sequence . ,latest)
    (unread . ((message_count . ((status . "unknown")))
               (badge_count . ((status . "unknown")))
               (first_unread . ((status . "unknown")))
               (mentions . ((status . "unknown")))))))

(ert-deftest qq-read-page-keeps-frontier-and-unread-materialization-distinct ()
  (let* ((unknown (qq-read-test-state))
         (exact (qq-read-test-exact-state))
         (page `((account_id . "slot-a") (states . (,unknown)))))
    (should (equal (qq-read--check-page page "slot-a") page))
    (should (qq-read--state-p exact))
    (should-not
     (qq-read--state-p
      (append (copy-tree unknown) '((unread_count . 10)))))
    (let ((backwards (copy-tree unknown)))
      (setf (alist-get 'read_sequence backwards) "101")
      (should-not (qq-read--state-p backwards)))
    (should-not
     (qq-read--state-p
      (qq-read-test-state
       (qq-read-test-exact-message-count 3)
       nil
       '((status . "exact") (position . nil)))))
    (should-not
     (qq-read--state-p
      (qq-read-test-state
       (qq-read-test-exact-message-count 0)
       nil
       (qq-read-test-exact-first-unread 0)
       '((status . "exact")
         (at_me . ((sequence . "93") (message_id . nil)))
         (at_all . nil)))))
    (let ((equal-unknown (qq-read-test-state)))
      (setf (alist-get 'read_sequence equal-unknown) "100")
      (should (qq-read--state-p equal-unknown)))
    (should-not
     (qq-read--state-p
      (qq-read-test-state
       (qq-read-test-exact-message-count 3)
       (qq-read-test-exact-badge 2))))
    (should-not
     (qq-read--state-p
      (qq-read-test-state
       '((status . "unknown") (at_least . 218)))))
    (should-not
     (qq-read--state-p
      (qq-read-test-state
       '((status . "unknown"))
       nil
       (qq-read-test-exact-first-unread 0)
       (qq-read-test-exact-mentions 3))))))

(ert-deftest qq-read-exact-and-unknown-map-to-one-internal-projection ()
  (let ((exact (qq-read--state-projection
                (qq-read-test-exact-state)))
        (unknown (qq-read--state-projection (qq-read-test-state))))
    (should (= (alist-get 'unread-message-count exact) 3))
    (should (= (alist-get 'unread-badge-count exact) 5))
    (should (equal (alist-get 'first-unread-message-seq exact) "91"))
    (should (equal (alist-get 'first-unread-message-id exact)
                   "7348923749823749823"))
    (should (equal (alist-get 'unread-at-me-message-seq exact) "93"))
    (should-not (alist-get 'read-latest-message-id exact))
    (should-not (alist-get 'unread-message-count unknown))
    (should-not (alist-get 'unread-badge-count unknown))
    (should-not (alist-get 'first-unread-message-seq unknown))))

(ert-deftest qq-read-position-completeness-is-independent-from-message-count ()
  (let* ((state
          (qq-read-test-state
           '((status . "unknown"))
           nil
           (qq-read-test-exact-first-unread 3)
           (qq-read-test-exact-mentions 3)))
         (projection (qq-read--state-projection state)))
    (should (qq-read--state-p state))
    (should-not (alist-get 'unread-message-count projection))
    (should (equal "91" (alist-get 'first-unread-message-seq projection)))
    (should (equal "93" (alist-get 'unread-at-me-message-seq projection)))))

(ert-deftest qq-read-newer-unknown-invalidates-older-exact-response ()
  (let ((qq-read--conversation-observations (make-hash-table :test #'equal))
        projections)
    (cl-letf (((symbol-function 'qq-runtime-call-with-account)
               (lambda (_account-id function) (funcall function)))
              ((symbol-function 'qq-state-upsert-session) #'ignore)
              ((symbol-function 'qq-state-apply-session-read-projection)
               (lambda (_session-key projection)
                 (push (copy-tree projection) projections))))
      (qq-read--apply-state
       "slot-a" (qq-read-test-exact-state) 1)
      (qq-read--apply-state "slot-a" (qq-read-test-state) 2)
      (qq-read--apply-state
       "slot-a" (qq-read-test-exact-state 1 1) 1)
      (should (= (length projections) 2))
      (should-not (alist-get 'unread-message-count (car projections)))
      (should-not (alist-get 'unread-badge-count (car projections)))
      (should (= (alist-get 'unread-message-count (cadr projections)) 3))
      (should (= (alist-get 'unread-badge-count (cadr projections)) 5)))))

(ert-deftest qq-read-list-and-evidence-refresh-use-distinct-rpcs ()
  (let (methods)
    (cl-letf (((symbol-function 'qq-rpc-call)
               (lambda (method _params &rest _arguments)
                 (push method methods)
                 method)))
      (qq-read-list-states "slot-a")
      (qq-read-refresh-states "slot-a"))
    (should
     (equal (nreverse methods)
            '("conversation.list_read_states"
              "conversation.refresh_read_states")))))

(ert-deftest qq-read-private-state-waits-for-public-chat-locator ()
  (let ((state (qq-read-test-private-state nil "10" "10")))
    (should (qq-read--state-p state))
    (should-not (qq-read--session-key state))))

(ert-deftest qq-read-unprojectable-private-event-still-wins-freshness ()
  (let ((qq-read--conversation-observations (make-hash-table :test #'equal))
        projections)
    (cl-letf (((symbol-function 'qq-runtime-call-with-account)
               (lambda (_account-id function) (funcall function)))
              ((symbol-function 'qq-state-upsert-session) #'ignore)
              ((symbol-function 'qq-state-apply-session-read-projection)
               (lambda (_session-key projection)
                 (push projection projections))))
      (let ((event (qq-read-test-private-state nil "10" "10"))
            (older (qq-read-test-private-state "10001" "9" "10")))
        (qq-read--apply-state "slot-a" event 2)
        (qq-read--apply-state "slot-a" older 1)
        (should-not projections)))))

(provide 'qq-read-test)
;;; qq-read-test.el ends here
