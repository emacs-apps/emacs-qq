;;; qq-search-navigation.el --- Search within a QQ timeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Find adjacent saved messages without replacing the ordinary chat timeline.

;;; Code:

(require 'cl-lib)
(require 'qq-chat)
(require 'qq-search)

(defvar-local qq-chat-search--query nil)
(defvar-local qq-chat-search--account-id nil)
(defvar-local qq-chat-search--session-key nil)
(defvar-local qq-chat-search--instance-id nil)
(defvar-local qq-chat-search--view nil)
(defvar-local qq-chat-search--center nil)
(defvar-local qq-chat-search--owner nil)
(defvar-local qq-chat-search--request nil)
(defvar-local qq-chat-search--jump-owner nil)
(defvar-local qq-chat-search--jump-origin nil)
(defvar-local qq-chat-search--loading nil)
(defvar-local qq-chat-search--status nil)
(defvar-local qq-chat-search--overlays nil)

(defun qq-chat-search--context-current-p ()
  "Return non-nil while the active search belongs to this exact chat view."
  (and qq-chat-search--query
       (equal qq-chat-search--account-id qq-runtime--account-id)
       (equal qq-chat-search--session-key qq-chat--session-key)
       (equal qq-chat-search--instance-id (qq-server-gateway-instance-id))
       (qq-chat--captured-view-current-p qq-chat-search--view)))

(defun qq-chat-search-status ()
  "Return this chat's visible search state, or nil."
  (when qq-chat-search--query
    (format "搜索「%s」%s"
            (string-replace "%" "%%"
                            (qq-chat--single-line-presentation
                             qq-chat-search--query))
            (cond
             ((not (qq-chat-search--context-current-p)) " · 连接已变更，请重新搜索")
             (qq-chat-search--loading " · 正在查找…")
             (qq-chat-search--status (concat " · " qq-chat-search--status))
             (t " · M-s M-p / M-s M-n · M-s M-c 退出")))))

(defun qq-chat-search--clear-highlights ()
  "Remove only overlays owned by this chat search."
  (mapc #'delete-overlay qq-chat-search--overlays)
  (setq qq-chat-search--overlays nil))

(defun qq-chat-search--highlight ()
  "Refresh literal matches on rendered message text, preserving the composer."
  (qq-chat-search--clear-highlights)
  (when (qq-chat-search--context-current-p)
    (save-excursion
      (let ((position (point-min)))
        (while (< position (point-max))
          (let* ((anchor (get-text-property position 'qq-chat-message-anchor))
                 (end (next-single-property-change
                       position 'qq-chat-message-anchor nil (point-max))))
            (when anchor
              (dolist (range (qq-search-literal-match-ranges
                              (buffer-substring-no-properties position end)
                              qq-chat-search--query))
                (let ((overlay (make-overlay (+ position (car range))
                                             (+ position (cdr range)))))
                  (overlay-put overlay 'face 'match)
                  (overlay-put overlay 'evaporate t)
                  (push overlay qq-chat-search--overlays))))
            (setq position end)))))))

(defun qq-chat-search--cancel-pending ()
  "Revoke pending search and its own jump before cancelling their handles."
  (let ((request qq-chat-search--request)
        (jump qq-chat-search--jump-owner))
    (setq qq-chat-search--owner nil
          qq-chat-search--request nil
          qq-chat-search--jump-owner nil
          qq-chat-search--jump-origin nil
          qq-chat-search--loading nil)
    (when request (qq-request-cancel request))
    (when jump (qq-chat--cancel-jump jump))))

(defun qq-chat-search-cancel ()
  "Leave chat search, retaining point, ordinary history and the input draft."
  (interactive)
  (qq-chat-search--cancel-pending)
  (setq qq-chat-search--query nil
        qq-chat-search--account-id nil
        qq-chat-search--session-key nil
        qq-chat-search--instance-id nil
        qq-chat-search--view nil
        qq-chat-search--center nil
        qq-chat-search--status nil)
  (qq-chat-search--clear-highlights)
  (when (derived-mode-p 'qq-chat-mode)
    (qq-chat--header-line-update)))

(defun qq-chat-search--current-p (buffer view owner)
  "Return non-nil while BUFFER's VIEW still owns search request OWNER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq qq-chat-search--owner owner)
              (eq qq-chat-search--view view)
              (qq-chat-search--context-current-p)))))

(defun qq-chat-search--check-result
    (result account-id session-key query direction instance-id center)
  "Validate detached adjacent RESULT and return its exact hit, or nil."
  (unless (and (qq-server-wire-exact-object-keys-p
                result '(gateway_instance_id account_id conversation query direction hit))
               (equal (alist-get 'gateway_instance_id result) instance-id)
               (equal (alist-get 'account_id result) account-id)
               (equal (alist-get 'query result) query)
               (equal (alist-get 'direction result) direction)
               (equal (alist-get 'conversation result)
                      (qq-message--history-conversation-params session-key)))
    (error "qq: Invalid adjacent search envelope"))
  (when-let* ((item (alist-get 'hit result))
              (hit (qq-search--check-hit item account-id session-key)))
    (when (and center
               (qq-message-history-locator-matches-p center (plist-get hit :message)))
      (error "qq: Adjacent search repeated the center message"))
    hit))

(defun qq-chat-search--settle (buffer view owner direction hit)
  "Navigate to accepted HIT owned by BUFFER, VIEW and OWNER."
  (when (qq-chat-search--current-p buffer view owner)
    (with-current-buffer buffer
      (setq qq-chat-search--owner nil
            qq-chat-search--request nil
            qq-chat-search--loading nil
            qq-chat-search--status nil)
      (if (null hit)
          (setq qq-chat-search--status
                (if (equal direction "older") "没有更早的匹配" "没有更新的匹配"))
        (setq qq-chat-search--center (plist-get hit :center)
              qq-chat-search--jump-origin
              (qq-message-history-locator (qq-chat--message-at-point)))
        (condition-case error-data
            (setq qq-chat-search--jump-owner
                  (qq-chat-goto-message qq-chat-search--center))
          (error
           (setq qq-chat-search--status (error-message-string error-data)))))
      (qq-chat-search--highlight)
      (qq-chat--header-line-update))))

(defun qq-chat-search--fail (buffer view owner _body reason)
  "Show request failure REASON only for BUFFER's current VIEW and OWNER."
  (when (qq-chat-search--current-p buffer view owner)
    (with-current-buffer buffer
      (setq qq-chat-search--owner nil
            qq-chat-search--request nil
            qq-chat-search--loading nil
            qq-chat-search--status (format "查找失败：%s" reason))
      (qq-chat--header-line-update))))

(defun qq-chat-search--find (direction)
  "Find the adjacent saved matching message in DIRECTION."
  (unless (qq-chat-search--context-current-p)
    (qq-chat-search--cancel-pending)
    (qq-chat-search--clear-highlights)
    (qq-chat--header-line-update)
    (user-error "qq: Start a new search in this chat after the connection changes"))
  (let* ((buffer (current-buffer))
         (view qq-chat-search--view)
         (account-id qq-chat-search--account-id)
         (session-key qq-chat-search--session-key)
         (query qq-chat-search--query)
         (instance-id qq-chat-search--instance-id)
         (at-point (qq-message-history-locator (qq-chat--message-at-point)))
         (center
          (if (and qq-chat--pending-jump-center
                   qq-chat-search--jump-owner
                   (eq qq-chat-search--jump-owner qq-chat--jump-owner)
                   (equal qq-chat--pending-jump-center qq-chat-search--center)
                   (equal at-point qq-chat-search--jump-origin))
              qq-chat-search--center
            (or at-point qq-chat-search--center)))
         (owner (list 'chat-search query direction center)))
    (qq-chat-search--cancel-pending)
    (setq qq-chat-search--owner owner
          qq-chat-search--loading t
          qq-chat-search--status nil)
    (qq-chat--header-line-update)
    (condition-case error-data
        (let ((request
               (qq-request-start
                (lambda (success failure)
                  (qq-message--call
                   "message.search_adjacent" account-id
                   `((conversation . ,(qq-message--history-conversation-params session-key))
                     (query . ,query) (direction . ,direction)
                     ,@(when center `((center . ,center))))
                   :projector
                   (lambda (result)
                     (qq-chat-search--check-result
                      result account-id session-key query direction instance-id center))
                   :callback success :errback failure))
                :owner account-id :lifecycle-owner view
                :callback (apply-partially #'qq-chat-search--settle
                                           buffer view owner direction)
                :errback (apply-partially #'qq-chat-search--fail buffer view owner))))
          (when (qq-chat-search--current-p buffer view owner)
            (setq qq-chat-search--request request)))
      (error
       (qq-chat-search--fail buffer view owner nil
                             (error-message-string error-data))))))

(defun qq-chat-search (query &optional forward-p)
  "Search saved chat messages for literal QUERY, retaining the normal timeline.
Start before the current message, or at the newest match from the composer.
With prefix FORWARD-P, start after point or at the oldest saved match instead.
Use `qq-chat-search-previous' and `qq-chat-search-next' to continue."
  (interactive
   (list (read-string "搜索此会话（字面关键词）: " qq-chat-search--query
                      'qq-search-history)
         current-prefix-arg))
  (unless (and (derived-mode-p 'qq-chat-mode)
               (memq (qq-state-session-key-type qq-chat--session-key) '(private group)))
    (user-error "qq: Search navigation supports private and group chat buffers"))
  (setq query (qq-search--validate-query query))
  (let ((view (qq-chat--live-current-view))
        (account-id qq-runtime--account-id))
    (unless (and view account-id)
      (user-error "qq: Chat search requires a live account-owned chat view"))
    (qq-chat-search-cancel)
    (setq qq-chat-search--query query
          qq-chat-search--account-id account-id
          qq-chat-search--session-key qq-chat--session-key
          qq-chat-search--instance-id (qq-server-gateway-instance-id)
          qq-chat-search--view view)
    (add-hook 'qq-chat-after-render-hook #'qq-chat-search--highlight nil t)
    (add-hook 'qq-chat-message-jump-hook #'qq-chat-search--after-jump nil t)
    (add-hook 'kill-buffer-hook #'qq-chat-search-cancel nil t)
    (add-hook 'change-major-mode-hook #'qq-chat-search-cancel nil t)
    (qq-chat-search--highlight)
    (qq-chat-search--find (if forward-p "newer" "older"))))

(defun qq-chat-search--after-jump (_center)
  "Refresh query highlights after an ordinary full-context jump."
  (qq-chat-search--highlight))

(defun qq-chat-search-previous ()
  "Find the preceding matching message, or prompt for an initial query."
  (interactive)
  (if qq-chat-search--query
      (qq-chat-search--find "older")
    (call-interactively #'qq-chat-search)))

(defun qq-chat-search-next ()
  "Find the following matching message, or prompt for an initial query."
  (interactive)
  (if qq-chat-search--query
      (qq-chat-search--find "newer")
    (let ((current-prefix-arg t))
      (call-interactively #'qq-chat-search))))

(provide 'qq-search-navigation)
;;; qq-search-navigation.el ends here
