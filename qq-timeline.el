;;; qq-timeline.el --- Gateway timelines for QQ chat Surfaces -*- lexical-binding: t; -*-

;;; Commentary:

;; The Gateway keeps every open conversation window current and reports
;; key-addressed diffs.  Each account App opens, pages and closes timelines for
;; its chat Surfaces, merges every reported row into canonical state, and
;; forwards each diff to the Surface that opened the timeline as
;; `(remove ROW-KEY)' and `(upsert ROW-KEY ANCHOR AFTER-ROW-KEY)' operations.
;;
;; A diff can overtake the reply to its own open; the App holds such diffs
;; until the open lands.  A diff that cannot reach its Surface closes the
;; timeline.  DataLine chats are not Gateway timelines and keep loading pages.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-command)
(require 'appkit-effect)
(require 'qq-state)
(require 'qq-runtime)
(require 'qq-message)
(require 'qq-rpc)

(defun qq-timeline--effect (account-id key method params input)
  "Return the Effect calling Gateway METHOD with PARAMS for ACCOUNT-ID.
Its result arrives as `(qq-timeline-result INPUT RESULT REASON)'."
  (appkit-effect-create
   :key key :input input
   :start
   (lambda (_context _input _observe resolve reject)
     (let ((cancel
            (condition-case condition
                (qq-runtime-call-with-account
                 account-id
                 (lambda () (qq-runtime--rpc-fetch method params resolve reject)))
              (error (funcall reject nil (error-message-string condition)) nil))))
       (appkit-cancellation-create
        :kind 'logical :cancel (lambda () (when cancel (funcall cancel))))))
   :success (lambda (input result) (list 'qq-timeline-result input result nil))
   :failure (lambda (input _body reason) (list 'qq-timeline-result input nil reason))))

(defun qq-timeline--ops (owner session-key data)
  "Merge diff DATA's rows for OWNER into SESSION-KEY and return Surface ops."
  (let* ((ops (alist-get 'ops data))
         (upserts (seq-filter (lambda (op) (equal (alist-get 'op op) "upsert")) ops))
         (anchors (qq-message-merge-timeline-items
                   owner session-key (mapcar (lambda (op) (alist-get 'item op)) upserts))))
    (mapcar
     (lambda (op)
       (if (equal (alist-get 'op op) "remove")
           (list 'remove (alist-get 'key op))
         (let ((row (pop anchors)))
           (list 'upsert (car row) (cdr row) (alist-get 'after op)))))
     ops)))

(defun qq-timeline--forward (owner timeline-id entry data)
  "Return commands forwarding diff DATA for TIMELINE-ID to ENTRY's Surface."
  (list (appkit-command-post-message
         :target (plist-get entry :route)
         :message (list 'qq-timeline-changed timeline-id
                        (qq-timeline--ops owner (plist-get entry :session) data)
                        (eq (alist-get 'has_older data) t)
                        (eq (alist-get 'has_newer data) t))
         :delivery 'report)))

(defun qq-timeline--close (owner timelines timeline-id)
  "Forget TIMELINE-ID in TIMELINES and return the command closing it."
  (remhash timeline-id timelines)
  (list (appkit-command-start-effect
         (qq-timeline--effect
          owner (list 'qq-timeline-close timeline-id) "timeline.close"
          `((timeline_id . ,timeline-id)) (list :close timeline-id)))))

(defun qq-timeline-app-update (context model message)
  "Return the account App transition for timeline MESSAGE, or nil."
  (when (or (memq (car-safe message)
                  '(qq-timeline-open qq-timeline-paginate qq-timeline-close
                    qq-timeline-result qq-timeline-diff qq-timeline-forget))
            (and (appkit-delivery-report-p message)
                 (eq (car-safe (appkit-delivery-report-message message))
                     'qq-timeline-changed)))
    (let* ((owner (qq-state-partition-account-id model))
           (timelines (copy-hash-table (qq-state-partition-timelines model)))
           (route (appkit-transition-context-reply-route context))
           commands)
      (qq-runtime-call-with-account
       owner
       (lambda ()
         (let ((qq-state-change-hook nil))
           (pcase message
             (`(qq-timeline-open ,view ,session-key ,focus ,count)
              (setq commands
                    (list (appkit-command-start-effect
                           (qq-timeline--effect
                            owner (list 'qq-timeline view) "timeline.open"
                            `((conversation . ,(qq-message--conversation-params session-key))
                              (focus . ,(pcase focus
                                          ('(latest) '((kind . "latest")))
                                          (`(around ,center)
                                           `((kind . "around") (center . ,center)))))
                              (count . ,count))
                            (list :open focus :session session-key :route route))))))
             (`(qq-timeline-paginate ,view ,timeline-id ,direction ,count)
              (setq commands
                    (list (appkit-command-start-effect
                           (qq-timeline--effect
                            owner (list 'qq-timeline-page view) "timeline.paginate"
                            `((timeline_id . ,timeline-id)
                              (direction . ,(symbol-name direction))
                              (count . ,count))
                            (list :page direction :route route))))))
             (`(qq-timeline-close ,timeline-id)
              (setq commands (qq-timeline--close owner timelines timeline-id)))
             ((pred appkit-delivery-report-p)
              ;; The Surface is gone; nothing else will read this timeline.
              (setq commands (qq-timeline--close
                              owner timelines
                              (nth 1 (appkit-delivery-report-message message)))))
             ('(qq-timeline-forget)
              (clrhash timelines))
             (`(qq-timeline-result ,input ,result ,reason)
              (cond
               ((plist-get input :close))
               ((plist-get input :page)
                (setq commands
                      (list (appkit-command-post-message
                             :target (plist-get input :route) :delivery 'report
                             :message (list 'qq-timeline-paged (plist-get input :page)
                                            (if reason 'failed
                                              (intern (alist-get 'outcome result)))
                                            reason)))))
               (reason
                (setq commands
                      (list (appkit-command-post-message
                             :target (plist-get input :route) :delivery 'report
                             :message (list 'qq-timeline-failed (plist-get input :open) reason)))))
               (t
                (let* ((timeline-id (alist-get 'timeline_id result))
                       (session-key (plist-get input :session))
                       (rows (qq-message-merge-timeline-items
                              owner session-key (alist-get 'items result)))
                       (held (plist-get (gethash timeline-id timelines) :held))
                       (entry (list :route (plist-get input :route) :session session-key)))
                  (puthash timeline-id entry timelines)
                  (setq commands
                        (cons (appkit-command-post-message
                               :target (plist-get input :route) :delivery 'report
                               :message (list 'qq-timeline-loaded (plist-get input :open)
                                              timeline-id rows
                                              (eq (alist-get 'has_older result) t)
                                              (eq (alist-get 'has_newer result) t)
                                              (cdr (assoc (alist-get 'center_key result) rows))))
                              (mapcan (lambda (data)
                                        (qq-timeline--forward owner timeline-id entry data))
                                      (reverse held))))))))
             (`(qq-timeline-diff ,data)
              (let* ((timeline-id (alist-get 'timeline_id data))
                     (entry (gethash timeline-id timelines)))
                (if (plist-get entry :route)
                    (setq commands (qq-timeline--forward owner timeline-id entry data))
                  ;; The open reply has not landed yet.
                  (puthash timeline-id
                           (list :held (cons data (plist-get entry :held)))
                           timelines))))))))
      (let ((model (copy-qq-state-partition model)))
        (setf (qq-state-partition-timelines model) timelines)
        (appkit-next :model model :render appkit-render-none :commands commands)))))

(defun qq-timeline--handle-event (_event data)
  "Hand Gateway diff DATA to its account App."
  (when-let* ((runtime (qq-runtime-account (alist-get 'account_id data))))
    (appkit-app-send (qq-runtime-account-app runtime) (list 'qq-timeline-diff data))))

(defun qq-timeline--gateway-ready (&rest _)
  "Reopen every chat timeline after the Gateway (re)starts."
  (dolist (runtime (qq-runtime-accounts))
    (appkit-app-send (qq-runtime-account-app runtime) '(qq-timeline-forget)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* (((derived-mode-p 'qq-chat-mode))
                  (view (appkit-current-surface))
                  ((appkit-surface-live-p view))
                  ((plist-get (appkit-surface-model view) :timeline)))
        (appkit-surface-send view '(qq-timeline-start (latest)))))))

(qq-rpc-register-event "timeline.diff" #'qq-timeline--handle-event)
(add-hook 'qq-account-registry-ready-hook #'qq-timeline--gateway-ready)

(provide 'qq-timeline)

;;; qq-timeline.el ends here
