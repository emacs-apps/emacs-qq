;;; qq-group-requests.el --- Native QQ group-request inbox -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Account-scoped main and filtered group-request inboxes backed exclusively by
;; the native Gateway `group_request.*' contract.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-presentation)
(require 'qq-core)
(require 'qq-request)
(require 'qq-runtime)

(defconst qq-group-requests--view-id 'group-requests
  "Stable Appkit identity of the group-request inbox.")

(defvar-local qq-group-requests--pages nil
  "Alist mapping `main' and `filtered' to their latest native pages.")

(defvar-local qq-group-requests--loading 0
  "Number of group-request mailbox reads still pending.")

(defvar-local qq-group-requests--errors nil
  "Mailbox-qualified group-request loading errors.")

(defvar-local qq-group-requests--requests nil
  "Active mailbox request objects.")

(defvar-local qq-group-requests--decision-request nil
  "Active group-request decision object.")

(defvar-local qq-group-requests--generation 0
  "Monotonic ownership generation for mailbox callbacks.")

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
  (cl-loop for (_mailbox . page) in qq-group-requests--pages
           sum (length (alist-get 'requests page))))

(defun qq-group-requests--header-line ()
  "Return the dynamic group-request header line."
  (format " QQ Group Requests · %d%s"
          (qq-group-requests--request-count)
          (if (> qq-group-requests--loading 0) " · loading" "")))

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
      (when qq-group-requests--decision-request
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
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-group-requests--header-line)))
       (appkit-presentation-insert-note-line
        "g 刷新 · TAB/<backtab> 移动 · q 退出" :face 'shadow)
       (when (> qq-group-requests--loading 0)
         (appkit-presentation-insert-note-line "正在刷新群申请…" :face 'shadow))
       (dolist (error (reverse qq-group-requests--errors))
         (appkit-presentation-insert-note-line error :face 'error))
       (dolist (mailbox '(main filtered))
         (appkit-presentation-insert-heading-line
          (qq-group-requests--mailbox-label mailbox))
         (let* ((page (alist-get mailbox qq-group-requests--pages))
                (requests (alist-get 'requests page)))
           (if requests
               (dolist (request requests)
                 (qq-group-requests--insert-request mailbox request))
             (appkit-presentation-insert-note-line
              (if (> qq-group-requests--loading 0)
                  "等待服务器结果…"
                "暂无申请。")
              :face 'shadow))))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun qq-group-requests--sync-invalidations (view invalidations _events)
  "Render VIEW after coalesced INVALIDATIONS."
  (when (and (appkit-invalidations-affect-p invalidations '(requests))
             (appkit-view-live-p view))
    (with-current-buffer (appkit-view-buffer view)
      (qq-group-requests-render))))

(defun qq-group-requests--request-sync (&optional view)
  "Request a coalesced sync for live VIEW."
  (when-let* ((view (or view (appkit-current-view))))
    (when (appkit-view-live-p view)
      (appkit-request-sync view :structure t :part 'requests))))

(defun qq-group-requests--cancel-work ()
  "Cancel this buffer's mailbox and decision requests."
  (dolist (request qq-group-requests--requests)
    (qq-request-cancel request))
  (when qq-group-requests--decision-request
    (qq-request-cancel qq-group-requests--decision-request))
  (setq qq-group-requests--requests nil
        qq-group-requests--decision-request nil
        qq-group-requests--loading 0))

(defun qq-group-requests--release-view-work (view buffer)
  "Release group-request work owned by VIEW and BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-group-requests-mode)
        (qq-group-requests--cancel-work)
        (setq qq-group-requests--pages nil
              qq-group-requests--errors nil)
        (setf (appkit-view-engine view) nil)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (set-buffer-modified-p nil))))))

(defun qq-group-requests--setup-view (view)
  "Register lifecycle cleanup for newly attached VIEW."
  (appkit-register-handle
   view 'function
   (apply-partially #'qq-group-requests--release-view-work
                    view (appkit-view-buffer view))))

(defun qq-group-requests--ensure-view ()
  "Return the live Appkit view owning the current inbox buffer."
  (qq-runtime-ensure-account-view
   :id qq-group-requests--view-id
   :mode 'qq-group-requests-mode
   :sync-function #'qq-group-requests--sync-invalidations
   :parts '(requests)
   :setup #'qq-group-requests--setup-view))

(defun qq-group-requests--callback-current-p (view buffer generation)
  "Return non-nil when callback ownership still matches VIEW and GENERATION."
  (and (appkit-view-live-p view)
       (eq (appkit-view-buffer view) buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-group-requests-mode)
              (eq view (appkit-current-view))
              (= generation qq-group-requests--generation)))))

(defun qq-group-requests--finish-mailbox
    (view buffer generation mailbox page error-text)
  "Settle MAILBOX callback for VIEW, BUFFER, and GENERATION."
  (when (qq-group-requests--callback-current-p view buffer generation)
    (with-current-buffer buffer
      (when page
        (setf (alist-get mailbox qq-group-requests--pages) page))
      (when error-text
        (push (format "%s：%s"
                      (qq-group-requests--mailbox-label mailbox)
                      error-text)
              qq-group-requests--errors))
      (setq qq-group-requests--loading
            (max 0 (1- qq-group-requests--loading)))
      (qq-group-requests--request-sync view))))

(defun qq-group-requests-refresh ()
  "Refresh both native group-request mailboxes."
  (interactive)
  (when qq-group-requests--decision-request
    (user-error "qq: a group-request decision is still pending"))
  (let* ((view (qq-group-requests--ensure-view))
         (buffer (current-buffer))
         (generation (cl-incf qq-group-requests--generation)))
    (dolist (request qq-group-requests--requests)
      (qq-request-cancel request))
    (setq qq-group-requests--requests nil
          qq-group-requests--loading 2
          qq-group-requests--errors nil)
    (qq-group-requests--request-sync view)
    (dolist (mailbox '(main filtered))
      (let ((request
             (qq-core-list-group-requests
              mailbox
              (lambda (page)
                (qq-group-requests--finish-mailbox
                 view buffer generation mailbox page nil))
              (lambda (_body reason)
                (qq-group-requests--finish-mailbox
                 view buffer generation mailbox nil
                 (or reason "未知错误"))))))
        (when (qq-request-active-p request)
          (push request qq-group-requests--requests))))))

(defun qq-group-requests--finish-decision
    (view buffer generation receipt error-text)
  "Settle a decision callback with RECEIPT or ERROR-TEXT."
  (when (qq-group-requests--callback-current-p view buffer generation)
    (with-current-buffer buffer
      (setq qq-group-requests--decision-request nil)
      (if error-text
          (progn
            (push (format "处理群申请失败：%s" error-text)
                  qq-group-requests--errors)
            (qq-group-requests--request-sync view))
        (ignore receipt)
        (qq-group-requests-refresh)))))

(defun qq-group-requests--decide (mailbox request decision refusal-message)
  "Apply DECISION to REQUEST in MAILBOX with REFUSAL-MESSAGE."
  (when qq-group-requests--decision-request
    (user-error "qq: a group-request decision is already pending"))
  (let* ((view (qq-group-requests--ensure-view))
         (buffer (current-buffer))
         (generation qq-group-requests--generation)
         (operation
          (qq-core-decide-group-request
           mailbox request decision refusal-message
           (lambda (receipt)
             (qq-group-requests--finish-decision
              view buffer generation receipt nil))
           (lambda (_body reason)
             (qq-group-requests--finish-decision
              view buffer generation nil (or reason "未知错误"))))))
    (when (qq-request-active-p operation)
      (setq qq-group-requests--decision-request operation))
    (qq-group-requests--request-sync view)))

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

(defun qq-group-requests-button-backward ()
  "Move point to the previous group-request button."
  (interactive)
  (forward-button -1))

(defvar qq-group-requests-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-group-requests-refresh)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-group-requests-button-backward)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-group-requests-mode'.")

(define-derived-mode qq-group-requests-mode special-mode "QQ-Group-Requests"
  "Major mode for native QQ group requests."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-group-requests--header-line)))
  (add-hook 'change-major-mode-hook #'qq-group-requests--cancel-work nil t)
  (add-hook 'kill-buffer-hook #'qq-group-requests--cancel-work nil t))

;;;###autoload
(defun qq-group-requests-open ()
  "Open the selected account's native group-request inbox."
  (interactive)
  (unless (qq-core-supports-p 'group-requests)
    (user-error "qq: Gateway does not support group requests"))
  (let* ((owner (qq-runtime-require-account-id "opening group requests"))
         (view
          (qq-runtime-open-account-view
           :account-id owner
           :id qq-group-requests--view-id
           :mode 'qq-group-requests-mode
           :buffer-name (qq-group-requests--buffer-name owner)
           :sync-function #'qq-group-requests--sync-invalidations
           :parts '(requests)
           :setup #'qq-group-requests--setup-view
           :select t))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (when (and (null qq-group-requests--pages)
                 (= qq-group-requests--loading 0))
        (qq-group-requests-refresh))
      (qq-group-requests--request-sync view)
      (appkit-sync-invalidations view))
    (pop-to-buffer buffer)
    buffer))

(provide 'qq-group-requests)

;;; qq-group-requests.el ends here
