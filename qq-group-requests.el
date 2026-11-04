;;; qq-group-requests.el --- Native QQ group-request inbox -*- lexical-binding: t; -*-

;; Author: 0WD0 <me@0wd0.com>

;;; Commentary:

;; Account-scoped main and filtered group-request inboxes backed exclusively by
;; the native Gateway `group_request.*' contract.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-bindings)
(require 'appkit-projection)
(require 'appkit-position)
(require 'appkit-presentation)
(require 'appkit-ui)
(require 'qq-core)
(require 'qq-request)
(require 'qq-runtime)

(defconst qq-group-requests--view-id 'group-requests
  "Stable Appkit identity of the group-request inbox.")

(defun qq-group-requests--buffer-name (account-id)
  "Return ACCOUNT-ID-qualified group-request buffer name."
  (qq-runtime-account-buffer-name "group-requests" nil account-id))

(defun qq-group-requests--mailbox-label (mailbox)
  "Return display label for MAILBOX."
  (pcase mailbox
    ('main "主要申请")
    ('filtered "过滤申请")
    (_ (error "qq: unsupported group-request mailbox %S" mailbox))))

(defun qq-group-requests--tag-kind (value)
  "Return the string discriminator of tagged VALUE."
  (and (listp value) (alist-get 'kind value)))

(defun qq-group-requests--user-label (user fallback)
  "Return USER's display name, falling back to FALLBACK."
  (or (and (listp user)
           (let ((name (alist-get 'name user)))
             (and (stringp name) (not (string-empty-p name)) name)))
      (and (listp user) (alist-get 'uid user))
      fallback))

(defun qq-group-requests--event-label (request)
  "Return a human-readable event label for REQUEST."
  (let ((target (qq-group-requests--user-label
                 (alist-get 'target request) "未知用户"))
        (inviter (qq-group-requests--user-label
                  (alist-get 'inviter request) "未知邀请人")))
    (pcase (qq-group-requests--tag-kind (alist-get 'event request))
      ("user_join" (format "%s 申请加入" target))
      ("account_invited" (format "%s 邀请你加入" inviter))
      ("member_invited" (format "%s 邀请 %s 加入" inviter target))
      (kind (format "未支持的群事件 %s" (or kind "unknown"))))))

(defun qq-group-requests--state-label (request)
  "Return the native processing-state label for REQUEST."
  (pcase (qq-group-requests--tag-kind (alist-get 'state request))
    ("pending" "待处理")
    ("processed" "已处理")
    ("no_action" "无需处理")
    ("unknown" "未知状态")
    (_ "状态缺失")))

(defun qq-group-requests--request-count ()
  "Return the number of currently projected group requests."
  (cl-loop for (_mailbox . page) in (mapcar (lambda (mailbox) (cons mailbox (plist-get (qq-runtime-operation (list 'group-requests mailbox)) :value))) '(main filtered))
           sum (length (alist-get 'requests page))))

(defun qq-group-requests--header-line ()
  "Return the dynamic group-request header line."
  (format " QQ Group Requests · %d%s"
          (qq-group-requests--request-count)
          (if (> (cl-count-if (lambda (mailbox) (plist-get (qq-runtime-operation (list 'group-requests mailbox)) :pending)) '(main filtered)) 0) " · loading" "")))

(defun qq-group-requests--request-key (mailbox request)
  "Return stable presentation identity for REQUEST in MAILBOX."
  (list mailbox
        (alist-get 'sequence request)
        (qq-group-requests--tag-kind (alist-get 'event request))
        (alist-get 'group_uin request)))

(defun qq-group-requests--button-context (button)
  "Return (MAILBOX REQUEST) stored on BUTTON or point."
  (let* ((button (or button (button-at (point))))
         (mailbox (and button (button-get button 'qq-group-request-mailbox)))
         (request (and button (button-get button 'qq-group-request))))
    (unless (and (memq mailbox '(main filtered)) (listp request))
      (user-error "qq: no group request at point"))
    (list mailbox request)))

(defun qq-group-requests--insert-action-button (label action mailbox request)
  "Insert LABEL invoking ACTION for REQUEST in MAILBOX."
  (insert-text-button
   label
   'follow-link t
   'keymap appkit-ui-button-map
   'face 'button
   'help-echo label
   'action action
   'qq-group-request-mailbox mailbox
   'qq-group-request (copy-tree request)))

(defun qq-group-requests--insert-request (mailbox request)
  "Insert one REQUEST from MAILBOX."
  (let ((start (point))
        (group-name (alist-get 'group_name request))
        (group-uin (alist-get 'group_uin request))
        (comment (alist-get 'comment request)))
    (insert (propertize
             (format "%s (%s)" (or (and (stringp group-name)
                                        (not (string-empty-p group-name))
                                        group-name)
                                   "QQ群")
                     group-uin)
             'face 'bold)
            "\n  "
            (qq-group-requests--event-label request)
            " · "
            (propertize (qq-group-requests--state-label request) 'face 'shadow)
            "\n")
    (unless (string-empty-p (or comment ""))
      (insert "  " comment "\n"))
    (when (and (eq (alist-get 'actionable request) t)
               (equal (qq-group-requests--tag-kind
                       (alist-get 'state request))
                      "pending"))
      (insert "  ")
      (qq-group-requests--insert-action-button
       " 同意 " #'qq-group-requests-accept mailbox request)
      (insert "  ")
      (qq-group-requests--insert-action-button
       " 拒绝 " #'qq-group-requests-reject mailbox request)
      (when (plist-get (qq-runtime-operation '(group-decision)) :pending)
        (add-text-properties start (point) '(face shadow)))
      (insert "\n"))
    (insert "\n")
    (add-text-properties
     start (point)
     (list 'qq-group-request-key
           (qq-group-requests--request-key mailbox request)))))

(defun qq-group-requests-render ()
  "Render the current native group-request inbox."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t)
           (pending (cl-some
                     (lambda (mailbox)
                       (plist-get (qq-runtime-operation (list 'group-requests mailbox)) :pending))
                     '(main filtered))))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-group-requests--header-line)))
       (appkit-presentation-insert-note-line
        "g 刷新 · TAB/<backtab> 移动 · q 退出"
        :face 'shadow)
       (when pending
         (appkit-presentation-insert-note-line "正在刷新群申请…" :face 'shadow))
       (dolist (error (reverse (delq nil (mapcar (lambda (key) (plist-get (qq-runtime-operation key) :error)) '((group-requests main) (group-requests filtered) (group-decision))))))
         (appkit-presentation-insert-note-line error :face 'error))
       (dolist (mailbox '(main filtered))
         (appkit-presentation-insert-heading-line
          (qq-group-requests--mailbox-label mailbox))
         (let* ((page (plist-get (qq-runtime-operation (list 'group-requests mailbox)) :value))
                (requests (alist-get 'requests page)))
           (if requests
               (dolist (request requests)
                 (qq-group-requests--insert-request mailbox request))
             (appkit-presentation-insert-note-line
              (if pending
                  "等待服务器结果…"
                "暂无申请。")
              :face 'shadow))))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun qq-group-requests--render (surface _app-view _model change)
  "Render profile or inbox CHANGE owned by SURFACE."
  (when (and (appkit-surface-live-p surface)
             (or (appkit-projection-change-full-p change)
                 (appkit-projection-change-keys change)
                 (appkit-projection-change-resources change)
                 (appkit-projection-change-geometry-p change)))
    (appkit-with-content-update surface
      (qq-group-requests-render)))
  nil)

(defun qq-group-requests--ensure-view ()
  "Return the live Appkit view owning the current inbox buffer."
  (qq-runtime-ensure-account-surface
   :id qq-group-requests--view-id
   :mode 'qq-group-requests-mode
   :renderer-factory (qq-runtime-renderer-factory #'qq-group-requests--render)))

(defun qq-group-requests-refresh ()
  "Refresh both shared group-request mailboxes."
  (interactive)
  (qq-group-requests--ensure-view)
  (when (plist-get (qq-runtime-operation '(group-decision)) :pending)
    (user-error "qq: a group-request decision is still pending"))
  (dolist (mailbox '(main filtered))
    (qq-runtime-request (qq-runtime-request-create 'group-requests :mailbox mailbox) t)))

(defun qq-group-requests--decide (mailbox request decision refusal-message)
  "Submit DECISION; the App refreshes both mailboxes after confirmation."
  (qq-group-requests--ensure-view)
  (qq-runtime-request (qq-runtime-request-create 'group-decision :mailbox mailbox :request request :decision decision :refusal-message refusal-message)))

(defun qq-group-requests-accept (&optional button)
  "Accept the group request stored on BUTTON or at point."
  (interactive)
  (pcase-let ((`(,mailbox ,request)
               (qq-group-requests--button-context button)))
    (when (yes-or-no-p
           (format "同意 %s？" (qq-group-requests--event-label request)))
      (qq-group-requests--decide mailbox request 'accept ""))))

(defun qq-group-requests-reject (&optional button)
  "Reject the group request stored on BUTTON or at point."
  (interactive)
  (pcase-let ((`(,mailbox ,request)
               (qq-group-requests--button-context button)))
    (let ((reason (read-string "拒绝理由（可空）：")))
      (when (yes-or-no-p
             (format "拒绝 %s？" (qq-group-requests--event-label request)))
        (qq-group-requests--decide mailbox request 'reject reason)))))

(defvar qq-group-requests-mode-map
  (make-sparse-keymap)
  "Keymap for `qq-group-requests-mode'.")

(appkit-bindings-define 'qq-group-requests-mode-map 'read-only
  'activate #'push-button
  'refresh #'qq-group-requests-refresh
  'next-item #'forward-button
  'previous-item #'backward-button
  'next-button #'forward-button
  'previous-button #'backward-button
  'quit #'quit-window
  'help #'describe-mode)

(define-derived-mode qq-group-requests-mode special-mode "QQ-Group-Requests"
  "Major mode for native QQ group requests."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-group-requests--header-line))))

;;;###autoload
(defun qq-group-requests-open ()
  "Open the selected account's native group-request inbox."
  (interactive)
  (unless (qq-core-supports-p 'group-requests)
    (user-error "qq: Gateway does not support group requests"))
  (let* ((owner (qq-runtime-require-account-id "opening group requests"))
         (view
          (qq-runtime-open-account-surface
           :account-id owner
           :id qq-group-requests--view-id
           :mode 'qq-group-requests-mode
           :buffer-name (qq-group-requests--buffer-name owner)
           :renderer-factory (qq-runtime-renderer-factory #'qq-group-requests--render)
           :select t))
         (buffer (appkit-surface-buffer view)))
    (with-current-buffer buffer
      (when (and (not (qq-runtime-operation '(group-requests main)))
                 (= (cl-count-if (lambda (mailbox) (plist-get (qq-runtime-operation (list 'group-requests mailbox)) :pending)) '(main filtered)) 0))
        (qq-group-requests-refresh))
      (qq-runtime-invalidate view :full-p t))
    (pop-to-buffer buffer)
    buffer))

(provide 'qq-group-requests)

;;; qq-group-requests.el ends here
