;; -*- lexical-binding: t -*-

(use-package evil
  :init
  (setq evil-want-integration t ; optional since it's already set to t by default
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo ; add C-r redo functionality
        evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; use evil in the *Messages* buffer
(add-to-list 'evil-normal-state-modes 'messages-buffer-mode)

(provide 'lbr-evil)
