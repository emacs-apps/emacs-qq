;;; qq-evil.el --- Native Evil bindings for emacs-qq -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs-qq keeps its ordinary maps as the Emacs-state interface.  This
;; optional integration keeps native Evil motions and defines only deliberate
;; application actions.  It does not depend on evil-collection.

;;; Code:

(require 'appkit-evil)
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
    qq-user-mode)
  "Major modes participating in emacs-qq's Evil integration.")

(defconst qq-evil--readonly-maps
  '(qq-contacts-mode-map
    qq-forward-mode-map
    qq-group-mode-map
    qq-group-requests-mode-map
    qq-root-mode-map
    qq-user-mode-map)
  "Read-only emacs-qq keymaps with standard modal quit semantics.")

(defun qq-evil--set-initial-states ()
  "Register `qq-evil-initial-state' for all QQ application modes."
  (appkit-evil-set-initial-states
   qq-evil--application-modes qq-evil-initial-state))

(defun qq-evil--define-readonly-keys ()
  "Install shared and surface-specific read-only bindings."
  (dolist (map qq-evil--readonly-maps)
    (appkit-evil-define-readonly-keys map))
  (appkit-evil-map
    (:map qq-root-mode-map
     :nm
     "RET" #'qq-root-open-at-point
     "<return>" #'qq-root-open-at-point
     "g r" #'qq-root-refresh
     "g j" #'qq-root-button-forward
     "g k" #'qq-root-button-backward
     "c" #'qq-contacts-open
     "g s" #'qq-root-open-session
     "a" #'qq-root-open-avatar-at-point
     "g ?" #'qq-root-open-info-at-point
     "I" #'qq-root-open-self-user
     "TAB" #'qq-root-tab-dwim
     "<backtab>" #'qq-root-button-backward
     "?" #'qq-root-transient)
    (:map qq-contacts-mode-map
     :nm
     "RET" #'qq-contacts-open-at-point
     "<return>" #'qq-contacts-open-at-point
     "g r" #'qq-contacts-refresh
     "g j" #'qq-contacts-next-item
     "g k" #'qq-contacts-previous-item
     "f" #'qq-contacts-show-friends
     "g G" #'qq-contacts-show-groups
     "g I" #'qq-contacts-show-not-recent-groups
     "g s" #'qq-contacts-search-group-members
     "_" #'qq-contacts-clear-search
     "m" #'qq-contacts-open-at-point
     "g ?" #'qq-contacts-open-info-at-point
     "a" #'qq-contacts-open-avatar-at-point
     "Z y" #'qq-contacts-copy-id-at-point
     "t" #'qq-contacts-toggle-category
     "TAB" #'forward-button
     "<backtab>" #'qq-contacts-button-backward
     "g b" #'qq-contacts-open-root)
    (:map qq-forward-mode-map
     :nm
     "g j" #'qq-forward-next-message
     "g k" #'qq-forward-previous-message)
    (:map qq-group-requests-mode-map
     :nm
     "g r" #'qq-group-requests-refresh
     "g j" #'forward-button
     "g k" #'qq-group-requests-button-backward
     "TAB" #'forward-button
     "<backtab>" #'qq-group-requests-button-backward)
    (:map qq-group-mode-map
     :nm
     "m" #'qq-group-open-chat
     "a" #'qq-group-open-avatar
     "g s" #'qq-group-search-members
     "o" #'qq-group-open-owner
     "Z y" #'qq-group-copy-id
     "TAB" #'forward-button
     "<backtab>" #'qq-group-button-backward)
    (:map qq-user-mode-map
     :nm
     "m" #'qq-user-open-chat
     "a" #'qq-user-open-avatar
     "Z y" #'qq-user-copy-id
     "TAB" #'forward-button
     "<backtab>" #'qq-user-button-backward)))

(defun qq-evil--define-chat-keys ()
  "Install chat-wide and timeline-only modal bindings."
  ;; The timeline map is inactive in the composer, so message actions never
  ;; steal input.  Poking is not pinning, and focusing a draft is not editing.
  (appkit-evil-map
    (:map qq-chat-mode-map
     :nm
     "RET" #'qq-chat-return-dwim
     "<return>" #'qq-chat-return-dwim
     "g j" #'qq-chat-next-message
     "g k" #'qq-chat-previous-message


     "Z a" #'qq-chat-attach
     "Z f" #'qq-chat-attach-file
     "Z v" #'qq-chat-attach-clipboard
     :i
     "RET" #'newline
     "<return>" #'newline)
    (:map qq-chat-timeline-mode-map
     :nm
     "q" #'quit-window
     "i" #'undefined
     "P" #'undefined
     "r" #'qq-chat-reply-to-message
     "R" #'qq-chat-forward-transient
     "D" #'qq-chat-delete-message
     "d d" #'qq-chat-delete-message
     "m" #'qq-chat-toggle-message-selection
     "U" #'qq-chat-clear-message-selection
     "a" #'qq-chat-open-avatar-at-point
     "g u" #'qq-chat-open-user-at-point
     "K" #'qq-chat-open-peer-info

     "g P" #'qq-chat-poke-sender
     "!" #'qq-chat-react-to-message
     "?" #'qq-chat-transient)))

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

(with-eval-after-load 'evil-snipe
  (dolist (mode qq-evil--application-modes)
    (add-hook (intern (format "%s-hook" mode)) #'turn-off-evil-snipe-mode)
    (add-hook (intern (format "%s-hook" mode)) #'turn-off-evil-snipe-override-mode)))

(with-eval-after-load 'qq-chat
  (when qq-evil-enable-integration
    (appkit-evil-define-keys '(normal motion) 'qq-chat-mode-map
      "g A" (lookup-key qq-chat-mode-map (kbd "M-g")))))

(provide 'qq-evil)

;;; qq-evil.el ends here
