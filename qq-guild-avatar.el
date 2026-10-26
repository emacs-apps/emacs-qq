;;; qq-guild-avatar.el --- Owned Guild avatar resources -*- lexical-binding: t; -*-
;;; Code:
(require 'qq-guild)
(require 'appkit-resource)
(require 'appkit-media-effect)
(require 'appkit-chat-avatar)
(require 'qq-media)

(defvar qq-guild-avatar--epoch nil
  "Current committed account epoch, bound during projection.")

(defun qq-guild-avatar-key (guild &optional tiny url)
  "Return an account/session scoped resource identity for GUILD and TINY."
  (list 'qq-guild-avatar (qq-runtime-current-account-id)
        (qq-server-gateway-instance-id) qq-guild-avatar--epoch guild tiny url))

(defun qq-guild-avatar--load (context input resolve reject)
  "Resolve INPUT's native identity, then acquire the image outside rendering."
  (pcase-let ((`(,_ ,account ,_instance ,_epoch ,guild ,tiny ,url) input))
    (let (cancel-rpc image-cancel cancelled)
      (cl-labels
          ((download (locator)
             (unless cancelled
               (condition-case err
                   (if (null locator) (funcall resolve nil)
                     (unless (and (stringp locator) (string-prefix-p "https://" locator))
                       (error "无效的频道头像地址"))
                     (setq image-cancel
                           (appkit-media-image-resource-load
                            context
                            (appkit-media-image-acquisition-create
                             `((url . ,locator))
                             (expand-file-name
                              (secure-hash 'sha256 (prin1-to-string (list account guild tiny locator)))
                              (expand-file-name "qq/guild-avatars/" user-emacs-directory)))
                            resolve reject)))
                 (error (funcall reject (error-message-string err)))))))
        (condition-case err
            (if url (download url)
              (setq cancel-rpc
                    (qq-runtime-call-with-account
                     account
                     (lambda ()
                       (qq-runtime--rpc-fetch
                        "guild.member_profile"
                        `((member . ((guild_id . ,guild) (tiny_id . ,tiny))))
                        (lambda (result)
                          (unless cancelled
                            (condition-case err
                                (let* ((profile (alist-get 'profile result))
                                       (member (alist-get 'member profile)))
                                  (unless (and (equal account (alist-get 'account_id result))
                                               (equal guild (alist-get 'guild_id member))
                                               (equal tiny (alist-get 'tiny_id member)))
                                    (error "频道头像响应身份不匹配"))
                                  (download (alist-get 'avatar_url profile)))
                              (error (funcall reject (error-message-string err))))))
                        (lambda (_body reason)
                          (unless cancelled (funcall reject reason))))))))
          (error (funcall reject (error-message-string err))))
        (appkit-cancellation-create
         :kind 'logical
         :cancel (lambda ()
                   (setq cancelled t)
                   (when cancel-rpc (funcall cancel-rpc))
                   (when image-cancel (funcall (appkit-cancellation-cancel image-cancel)))))))))

(defun qq-guild-avatar-demand (key)
  "Create one shared acquisition demand for KEY."
  (appkit-resource-demand-create
   :key key :input key :loader #'qq-guild-avatar--load
   :acquisition-identity key :sharing-policy 'app-private :cache-policy 'while-interested))

(defun qq-guild-avatar-image (key)
  "Read KEY's prepared avatar without starting work."
  (when-let* ((state (appkit-resource-state (appkit-current-surface) key))
              ((eq (appkit-resource-state-status state) 'ready))
              (file (appkit-resource-state-value state)))
    (qq-media--avatar-image-from-file file qq-media-avatar-image-height)))

(defun qq-guild-avatar-result (pairs)
  "Retain avatar (ROW . KEY) PAIRS through the Resource broker."
  (let ((interests (make-hash-table :test #'equal)))
    (dolist (pair pairs)
      (puthash (cdr pair) (cons (car pair) (gethash (cdr pair) interests)) interests))
    (appkit-render-result-create
     :resource-demands (mapcar #'qq-guild-avatar-demand (hash-table-keys interests))
     :resource-interest-update
     (appkit-resource-interest-update-create
      :mode 'replace
      :entries (mapcar (lambda (key)
                         (appkit-resource-interest-create :key key :row-keys (gethash key interests)))
                       (hash-table-keys interests))))))

(provide 'qq-guild-avatar)
;;; qq-guild-avatar.el ends here
