;;; qq-evil.el --- Native Evil bindings for emacs-qq -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs-qq keeps its ordinary maps as the Emacs-state interface.  This
;; optional integration keeps native Evil motions and defines only deliberate
;; application actions.  It does not depend on evil-collection.

;;; Code:

(require 'appkit-evil)
(require 'appkit-bindings)
(require 'qq)




(defgroup qq-evil nil
  "Optional native Evil integration for emacs-qq."
  :group 'qq
  :prefix "qq-evil-")

(defcustom qq-evil-enable-integration t
  "If non-nil, install emacs-qq's Evil bindings automatically."
  :type 'boolean
  :group 'qq-evil)

(defcustom qq-evil-initial-state 'normal
  "Initial Evil state used for emacs-qq buffers.
When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
                 (const :tag "Normal" normal)
                 (const :tag "Motion" motion)
                 (const :tag "Emacs" emacs)
                 (symbol :tag "Custom state"))
  :group 'qq-evil)

(defconst qq-evil--application-modes
  '(qq-chat-mode
    qq-contacts-mode
    qq-forward-mode
    qq-group-mode
    qq-group-requests-mode
    qq-root-mode
    qq-search-mode
    qq-user-mode)
  "Major modes participating in emacs-qq's Evil integration.")

(defconst qq-evil--readonly-maps
  '(qq-contacts-mode-map
    qq-forward-mode-map
    qq-group-mode-map
    qq-group-requests-mode-map
    qq-root-mode-map
    qq-search-mode-map
    qq-user-mode-map)
  "Read-only emacs-qq keymaps with standard modal quit semantics.")

(defun qq-evil--set-initial-states ()
  "Register `qq-evil-initial-state' for all QQ application modes."
  (appkit-evil-set-initial-states
   qq-evil--application-modes qq-evil-initial-state))

(defun qq-evil--define-readonly-keys ()
  "Project shared read-only roles and retain QQ-specific actions."
  (dolist (map qq-evil--readonly-maps)
    (appkit-evil-define-readonly-keys map))
  (appkit-evil-map
    :map qq-root-mode-map
    :nm
    "c" #'qq-contacts-open
    "a" #'qq-root-open-avatar-at-point
    "g i" #'qq-root-open-info-at-point
    "I" #'qq-root-open-self-user
    :map qq-contacts-mode-map
    :nm
    "f" #'qq-contacts-show-friends
    "g G" #'qq-contacts-show-groups
    "g I" #'qq-contacts-show-not-recent-groups
    "g i" #'qq-contacts-open-info-at-point
    "a" #'qq-contacts-open-avatar-at-point
    "Z y" #'qq-contacts-copy-id-at-point
    "t" #'qq-contacts-toggle-category
    "g b" #'qq-contacts-open-root
    :map qq-group-mode-map
    :nm
    "m" #'qq-group-open-chat
    "a" #'qq-group-open-avatar
    "o" #'qq-group-open-owner
    "Z y" #'qq-group-copy-id
    :map qq-user-mode-map
    :nm
    "m" #'qq-user-open-chat
    "a" #'qq-user-open-avatar
    "Z y" #'qq-user-copy-id))

(defun qq-evil--define-chat-keys ()
  "Project shared chat roles and retain QQ-specific timeline actions."
  (appkit-bindings-enable-evil 'qq-chat-mode-map)
  (appkit-bindings-enable-evil 'qq-chat-timeline-mode-map)
  ;; Only the point-sensitive map owns message actions.  Poking is not pinning.
  (appkit-evil-map
    :map qq-chat-timeline-mode-map
    :nm
    "a" #'qq-chat-open-avatar-at-point
    "g u" #'qq-chat-open-user-at-point
    "K" #'qq-chat-open-peer-info
    "g P" #'qq-chat-poke-sender)
  (appkit-evil-map
    :map qq-chat-mode-map
    :nm
    "M-s m" #'qq-chat-search
    "M-s M-p" #'qq-chat-search-previous
    "M-s M-n" #'qq-chat-search-next
    "M-s M-c" #'qq-chat-search-cancel))

(defun qq-evil--refresh-live-buffers ()
  "Refresh Evil projections in existing QQ application buffers."
  (appkit-evil-normalize-buffers qq-evil--application-modes))

;;;###autoload
(defun qq-evil-setup ()
  "Install emacs-qq's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when (and (featurep 'evil) qq-evil-enable-integration)
    (qq-evil--set-initial-states)
    (qq-evil--define-readonly-keys)
    (qq-evil--define-chat-keys)
    (qq-evil--refresh-live-buffers)))

(with-eval-after-load 'evil
  (qq-evil-setup))




(provide 'qq-evil)

;;; qq-evil.el ends here
