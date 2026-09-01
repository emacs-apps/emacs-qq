;;; qq-gray-tip.el --- Typed QQ service-message presentation -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Presentation adapter for the producer-neutral GrayTip semantics exported by
;; nt-runtime.  Native protobuf evidence and producer routing never cross this
;; boundary.  UIN-only and UID-only identities remain distinct; only a real
;; UIN becomes an interactive profile link.

;;; Code:

(require 'subr-x)
(require 'qq-account)
(require 'qq-state)

(defun qq-gray-tip--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-gray-tip--text-part (text)
  "Return one GrayTip presentation part containing TEXT."
  `((type . "text") (text . ,text)))

(defun qq-gray-tip--identity (identity)
  "Return the closed local projection of typed Gateway IDENTITY."
  (pcase (alist-get 'kind identity)
    ("uin"
     (let ((uin (alist-get 'uin identity)))
       (unless (qq-protocol-uint64-decimal-p uin)
         (error "qq: Gateway returned an invalid GrayTip UIN identity"))
       `((kind . "uin") (uin . ,uin))))
    ("uid"
     (let ((uid (alist-get 'uid identity)))
       (unless (qq-protocol-non-empty-string-p uid)
         (error "qq: Gateway returned an invalid GrayTip UID identity"))
       `((kind . "uid") (uid . ,uid))))
    (_ (error "qq: Gateway returned an unsupported GrayTip identity"))))

(defun qq-gray-tip--user-part (role identity)
  "Return one GrayTip user part for ROLE and typed IDENTITY."
  (let ((identity (qq-gray-tip--identity identity)))
    (pcase (alist-get 'kind identity)
      ("uin"
       (let ((uin (alist-get 'uin identity)))
         `((type . "user")
           (role . ,role)
           (identity . ,identity)
           (user-id . ,uin)
           (name . ,(qq-state--gray-tip-user-name uin nil)))))
      ("uid"
       (let ((uid (alist-get 'uid identity)))
         `((type . "user")
           (role . ,role)
           (identity . ,identity)
           (user-uid . ,uid)
           (name . ,uid)))))))

(defun qq-gray-tip--joined-user-parts (role identities)
  "Return comma-separated user parts for ROLE and IDENTITIES."
  (unless (consp identities)
    (error "qq: Gateway returned an empty GrayTip user list"))
  (let ((first t)
        parts)
    (dolist (identity identities)
      (unless first
        (push (qq-gray-tip--text-part "、") parts))
      (push (qq-gray-tip--user-part role identity) parts)
      (setq first nil))
    (nreverse parts)))

(defun qq-gray-tip--parts-text (parts)
  "Return the plain-text projection of GrayTip presentation PARTS."
  (mapconcat
   (lambda (part)
     (pcase (alist-get 'type part)
       ("text" (alist-get 'text part))
       ("user" (alist-get 'name part))
       (_ (error "qq: Internal GrayTip presentation part is invalid"))))
   parts ""))

(defun qq-gray-tip--join-cause (cause)
  "Return the closed local projection of group-join CAUSE."
  (pcase (alist-get 'kind cause)
    ("added" '((kind . "added")))
    ("invited"
     `((kind . "invited")
       (inviter . ,(qq-gray-tip--identity (alist-get 'inviter cause)))))
    ("qr_code"
     `((kind . "qr_code")
       (shared_by . ,(qq-gray-tip--identity
                      (alist-get 'shared_by cause)))))
    (_ (error "qq: Gateway returned an unsupported group-join cause"))))

(defun qq-gray-tip--membership-parts (members cause)
  "Return presentation parts for MEMBERS joining because of CAUSE."
  (let ((members (qq-gray-tip--joined-user-parts "member" members)))
    (pcase (alist-get 'kind cause)
      ("added"
       (append members (list (qq-gray-tip--text-part " 加入群聊"))))
      ("invited"
       (append
        (list (qq-gray-tip--user-part "inviter"
                                      (alist-get 'inviter cause))
              (qq-gray-tip--text-part " 邀请 "))
        members
        (list (qq-gray-tip--text-part " 加入群聊"))))
      ("qr_code"
       (append
        (list (qq-gray-tip--user-part "shared_by"
                                      (alist-get 'shared_by cause))
              (qq-gray-tip--text-part " 分享二维码，"))
        members
        (list (qq-gray-tip--text-part " 加入群聊"))))
      (_ (error "qq: Gateway returned an unsupported group-join cause")))))

(defun qq-gray-tip--project-members-joined (payload)
  "Project one group-members-joined GrayTip PAYLOAD."
  (let* ((members (mapcar #'qq-gray-tip--identity
                          (alist-get 'members payload)))
         (cause (qq-gray-tip--join-cause (alist-get 'cause payload)))
         (attached-count (alist-get 'attached_message_count payload))
         (parts (qq-gray-tip--membership-parts members cause)))
    (when attached-count
      (unless (and (integerp attached-count) (> attached-count 0))
        (error "qq: Gateway returned an invalid attached-message count"))
      (setq parts
            (append parts
                    (list
                     (qq-gray-tip--text-part
                      (format "，并附带 %d 条聊天记录" attached-count))))))
    `((kind . "group_members_joined")
      (members . ,members)
      (cause . ,cause)
      ,@(when attached-count
          `((attached-message-count . ,attached-count)))
      (text . ,(qq-gray-tip--parts-text parts))
      (parts . ,parts))))

(defun qq-gray-tip--project-member-removed (payload)
  "Project one group-member-removed GrayTip PAYLOAD."
  (let* ((member-identity
          (qq-gray-tip--identity (alist-get 'member payload)))
         (member (qq-gray-tip--user-part "member" member-identity))
         (operator-identity
          (and (alist-get 'operator payload)
               (qq-gray-tip--identity (alist-get 'operator payload))))
         (parts
          (if operator-identity
              (list (qq-gray-tip--user-part "operator" operator-identity)
                    (qq-gray-tip--text-part " 将 ")
                    member
                    (qq-gray-tip--text-part " 移出群聊"))
            (list member (qq-gray-tip--text-part " 已被移出群聊")))))
    `((kind . "group_member_removed")
      (member . ,member-identity)
      ,@(when operator-identity
          `((operator . ,operator-identity)))
      (text . ,(qq-gray-tip--parts-text parts))
      (parts . ,parts))))

(defun qq-gray-tip--mute-duration-text (duration-seconds)
  "Return an exact presentation for positive DURATION-SECONDS."
  (format "%d 秒" duration-seconds))

(defun qq-gray-tip--mute-target (target)
  "Return the closed local projection of group-mute TARGET."
  (pcase (alist-get 'kind target)
    ("member"
     `((kind . "member")
       (member . ,(qq-gray-tip--identity (alist-get 'member target)))))
    ("whole_group" '((kind . "whole_group")))
    (_ (error "qq: Gateway returned an unsupported group-mute target"))))

(defun qq-gray-tip--project-mute-changed (payload)
  "Project one group-mute-changed GrayTip PAYLOAD."
  (let* ((target (qq-gray-tip--mute-target (alist-get 'target payload)))
         (target-kind (alist-get 'kind target))
         (operator-identity
          (and (alist-get 'operator payload)
               (qq-gray-tip--identity (alist-get 'operator payload))))
         (duration (alist-get 'duration_seconds payload)))
    (unless (qq-protocol-uint32-p duration)
      (error "qq: Gateway returned an invalid group-mute duration"))
    (let* ((operator (and operator-identity
                          (qq-gray-tip--user-part
                           "operator" operator-identity)))
           (parts
            (pcase target-kind
              ("member"
               (let ((member (qq-gray-tip--user-part
                              "member" (alist-get 'member target))))
                 (cond
                  ((and operator (zerop duration))
                   (list operator (qq-gray-tip--text-part " 解除了 ")
                         member (qq-gray-tip--text-part " 的禁言")))
                  (operator
                   (list operator (qq-gray-tip--text-part " 将 ")
                         member
                         (qq-gray-tip--text-part
                          (format " 禁言 %s"
                                  (qq-gray-tip--mute-duration-text duration)))))
                  ((zerop duration)
                   (list member (qq-gray-tip--text-part " 的禁言已解除")))
                  (t
                   (list member
                         (qq-gray-tip--text-part
                          (format " 被禁言 %s"
                                  (qq-gray-tip--mute-duration-text duration))))))))
              ("whole_group"
               (append
                (and operator (list operator))
                (list
                 (qq-gray-tip--text-part
                  (cond
                   ((and operator (zerop duration)) " 解除了全员禁言")
                   ((zerop duration) "全员禁言已解除")
                   (operator
                    (format " 开启了全员禁言（%s）"
                            (qq-gray-tip--mute-duration-text duration)))
                   (t
                    (format "已开启全员禁言（%s）"
                            (qq-gray-tip--mute-duration-text duration))))))))
              (_ (error "qq: Gateway returned an unsupported group-mute target")))))
      `((kind . "group_mute_changed")
        (target . ,target)
        ,@(when operator-identity
            `((operator . ,operator-identity)))
        (duration-seconds . ,duration)
        (text . ,(qq-gray-tip--parts-text parts))
        (parts . ,parts)))))

(defun qq-gray-tip-project (payload)
  "Project one closed Gateway GrayTip PAYLOAD into presentation data.

The negotiated Gateway owns the semantic schema.  This adapter derives only
Emacs presentation; native producer evidence never crosses this boundary."
  (pcase (alist-get 'kind payload)
    ("poke"
     (let* ((actor-uin (alist-get 'actor_uin payload))
            (target-uin (alist-get 'target_uin payload))
            (action (qq-gray-tip--present-string (alist-get 'action payload)))
            (detail (qq-gray-tip--present-string (alist-get 'suffix payload)))
            (actor-name
             (qq-state--gray-tip-user-name
              actor-uin (alist-get 'actor_name payload)))
            (target-name
             (qq-state--gray-tip-user-name
              target-uin (alist-get 'target_name payload))))
       `((kind . "poke")
         (actor-id . ,actor-uin)
         (target-id . ,target-uin)
         (actor-name . ,actor-name)
         (target-name . ,target-name)
         (image-url . ,(alist-get 'action_image_url payload))
         (action . ,action)
         (detail . ,detail)
         (texts . ,(delq nil (list action detail))))))
    ("group_members_joined"
     (qq-gray-tip--project-members-joined payload))
    ("group_member_removed"
     (qq-gray-tip--project-member-removed payload))
    ("group_disbanded"
     '((kind . "group_disbanded")
       (text . "群聊已解散")
       (parts . (((type . "text") (text . "群聊已解散"))))))
    ("group_mute_changed"
     (qq-gray-tip--project-mute-changed payload))
    ("group_name_changed"
     (let* ((name (alist-get 'name payload))
            (text (format "群名称已修改为「%s」" name)))
       `((kind . "group_name_changed")
         (name . ,name)
         (text . ,text)
         (parts . (((type . "text") (text . ,text)))))))
    ("general"
     (let ((summary (alist-get 'summary payload)))
       `((kind . "general")
         (text . ,summary)
         (business-type . ,(alist-get 'business_type payload))
         (business-id . ,(alist-get 'business_id payload))
         ,@(when (assq 'template_id payload)
             `((template-id . ,(alist-get 'template_id payload))))
         (parts . (((type . "text") (text . ,summary)))))))
    (_ (error "qq: Gateway returned an unsupported GrayTip subtype"))))

(provide 'qq-gray-tip)
;;; qq-gray-tip.el ends here
