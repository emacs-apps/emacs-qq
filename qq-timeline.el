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
(require 'appkit-data)
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
                 (lambda () (qq-runtime--rpc-fetch (qq-runtime-current-account-id) method params resolve reject)))
              (error (funcall reject nil (error-message-string condition)) nil))))
       (appkit-cancellation-create
        :kind 'logical :cancel (lambda () (when cancel (funcall cancel))))))
   :success (lambda (input result) (list 'qq-timeline-result input result nil))
   :failure (lambda (input _body reason) (list 'qq-timeline-result input nil reason))))

(defun qq-timeline--ops (state owner session-key data)
  "Merge diff DATA's rows for OWNER into SESSION-KEY and return Surface ops."
  (let ((value (progn
                 (let* ((ops (alist-get 'ops data))
                        (upserts (seq-filter (lambda (op) (equal (alist-get 'op op) "upsert")) ops))
                        (anchors (qq-state--step state (qq-message-merge-timeline-items state
                                                                                        owner session-key (mapcar (lambda (op) (alist-get 'item op)) upserts)))))
                   (mapcar
                    (lambda (op)
                      (if (equal (alist-get 'op op) "remove")
                          (list 'remove (alist-get 'key op))
                        (let ((row (pop anchors)))
                          (list 'upsert (car row) (cdr row) (alist-get 'after op)))))
                    ops)))))
    (cons state value)))

(defun qq-timeline--forward (state owner timeline-id entry data)
  "Return commands forwarding diff DATA for TIMELINE-ID to ENTRY's Surface."
  (let ((value (progn
                 (list (appkit-command-post-message
                        :target (plist-get entry :route)
                        :message (list 'qq-timeline-changed timeline-id
                                       (qq-state--step state (qq-timeline--ops state owner (plist-get entry :session) data))
                                       (eq (alist-get 'has_older data) t)
                                       (eq (alist-get 'has_newer data) t))
                        :delivery 'report)))))
    (cons state value)))

(defun qq-timeline--close (owner timelines timeline-id)
  "Return (TIMELINES . COMMANDS) forgetting and closing TIMELINE-ID once."
  (if (not (appkit-data-map-get timelines timeline-id)) (cons timelines nil)
    (cons (appkit-data-map-remove timelines timeline-id)
          (list (appkit-command-cancel-effect (list 'qq-timeline-page timeline-id))
                (appkit-command-start-effect
                 (qq-timeline--effect
                  owner (list 'qq-timeline-close timeline-id) "timeline.close"
                  `((timeline_id . ,timeline-id)) (list :close timeline-id)))))))

(defun qq-timeline-app-update (context model message)
  "Return the account App transition for timeline MESSAGE, or nil."
  (when (or (memq (car-safe message)
                  '(qq-timeline-open qq-timeline-paginate qq-timeline-close
                    qq-timeline-result qq-timeline-diff qq-timeline-forget))
            (and (appkit-delivery-report-p message)
                 (memq (car-safe (appkit-delivery-report-message message))
                       '(qq-timeline-loaded qq-timeline-changed))))
    (let* ((owner (qq-state-partition-account-id model))
           (timelines (qq-state-partition-timelines model))
           (state (qq-state-partition-store model))
           (route (appkit-transition-context-reply-route context))
           commands)
      (pcase message
        (`(qq-timeline-open ,session-key ,focus ,count)
         (setq commands
               (list (appkit-command-start-effect
                      (qq-timeline--effect
                       ;; Retain superseded opens to close their remote timelines.
                       owner (make-symbol "qq-timeline-open") "timeline.open"
                       `((conversation . ,(qq-message--conversation-params session-key))
                         (focus . ,(pcase focus
                                     ('(latest) '((kind . "latest")))
                                     (`(around ,center)
                                      `((kind . "around") (center . ,center)))))
                         (count . ,count))
                       (list :open focus :session session-key :route route))))))
        (`(qq-timeline-paginate ,timeline-id ,direction ,count)
         (setq commands
               (list (appkit-command-start-effect
                      (qq-timeline--effect
                       owner (list 'qq-timeline-page timeline-id) "timeline.paginate"
                       `((timeline_id . ,timeline-id)
                         (direction . ,(symbol-name direction))
                         (count . ,count))
                       (list :page direction :timeline timeline-id :route route))))))
        (`(qq-timeline-close ,timeline-id)
         (let ((closed (qq-timeline--close owner timelines timeline-id)))
           (setq timelines (car closed) commands (cdr closed))))
        ((pred appkit-delivery-report-p)
         ;; The Surface is gone; nothing else will read this timeline.
         (let ((closed (qq-timeline--close
                        owner timelines
                        (nth 1 (appkit-delivery-report-message message)))))
           (setq timelines (car closed) commands (cdr closed))))
        ('(qq-timeline-forget)
         (setq timelines appkit-data-map-empty))
        (`(qq-timeline-result ,input ,result ,reason)
         (cond
          ((plist-get input :close))
          ((plist-get input :page)
           (setq commands
                 (list (appkit-command-post-message
                        :target (plist-get input :route) :delivery 'report
                        :message (list 'qq-timeline-paged (plist-get input :timeline)
                                       (plist-get input :page)
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
                  (rows (qq-state--step state (qq-message-merge-timeline-items state
                                                                               owner session-key (alist-get 'items result))))
                  (held (plist-get (appkit-data-map-get timelines timeline-id) :held))
                  (entry (list :route (plist-get input :route) :session session-key)))
             (setq timelines (appkit-data-map-set timelines timeline-id entry))
             (setq commands
                   (cons (appkit-command-post-message
                          :target (plist-get input :route) :delivery 'report
                          :message (list 'qq-timeline-loaded timeline-id (plist-get input :open) rows
                                         (eq (alist-get 'has_older result) t)
                                         (eq (alist-get 'has_newer result) t)
                                         (cdr (assoc (alist-get 'center_key result) rows))))
                         (mapcan (lambda (data)
                                   (qq-state--step state (qq-timeline--forward state owner timeline-id entry data)))
                                 (reverse held))))))))
        (`(qq-timeline-diff ,data)
         (let* ((timeline-id (alist-get 'timeline_id data))
                (entry (appkit-data-map-get timelines timeline-id)))
           (if (plist-get entry :route)
               (setq commands (qq-state--step state (qq-timeline--forward state owner timeline-id entry data)))
             ;; The open reply has not landed yet.
             (setq timelines
                   (appkit-data-map-set
                    timelines timeline-id
                    (list :held (cons data (plist-get entry :held)))))))))
      (appkit-next :model (appkit-data-with model (() :timelines timelines :store state))
                   :render appkit-render-none :commands commands))))

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

(defun qq-timeline-bind-surface (surface)
  "Release SURFACE's current Gateway timeline when its lifetime ends."
  (appkit-register-handle
   surface 'function
   (lambda ()
     (let ((app (appkit-surface-app surface))
           (id (plist-get (appkit-surface-model surface) :timeline))
           (instance (qq-server-gateway-instance-id)))
       (when (stringp id)
         ;; A Surface can stop inside a pass; release through the App afterwards.
         (run-at-time 0 nil
                      (lambda ()
                        (when (and (appkit-app-live-p app)
                                   (equal instance (qq-server-gateway-instance-id)))
                          (appkit-app-send app (list 'qq-timeline-close id))))))))))

(provide 'qq-timeline)

;;; qq-timeline.el ends here
