;; -*- lexical-binding: t -*-

(use-package markdown-mode
  :custom
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  :bind (:map markdown-mode-map
              ("C-c C-x C-v" . markdown-toggle-inline-images)
              ("M-<left>"    . markdown-promote)
              ("M-<right>"   . markdown-demote)
              ("M-<up>"      . markdown-move-up)
              ("M-<down>"    . markdown-move-down)))

(defun lbr/markdown-header-height-setup ()
  (set-face-attribute 'markdown-header-face-1 nil :height 1.8)
  (set-face-attribute 'markdown-header-face-2 nil :height 1.6)
  (set-face-attribute 'markdown-header-face-3 nil :height 1.4)
  (set-face-attribute 'markdown-header-face-4 nil :height 1.2)
  (set-face-attribute 'markdown-header-face-5 nil :height 1.0)
  (set-face-attribute 'markdown-header-face-6 nil :height 1.0))
(add-hook 'markdown-mode-hook #'lbr/markdown-header-height-setup)

;; Smart quotes and text objects for evil-surround
(add-hook 'markdown-mode-hook 'electric-quote-local-mode)
(add-hook 'markdown-mode-hook 'lbr/setup-smart-quotes)

;; Fix to allow my quotation config to work with Markdown and Quarto
(defun lbr/markdown-inhibit-electric-quote ()
  "Inhibit electric quotes inside Markdown code blocks."
  (and (derived-mode-p 'markdown-mode)
       (markdown-code-block-at-point-p)))

(add-hook 'electric-quote-inhibit-functions 'lbr/markdown-inhibit-electric-quote)

(provide 'lbr-markdown)
