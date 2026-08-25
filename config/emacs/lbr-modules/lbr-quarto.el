;; -*- lexical-binding: t -*-

(use-package quarto-mode
  :mode (("\\.qmd\\'" . poly-quarto-mode)))

;; Smart quotes and text objects for evil-surround
(add-hook 'poly-quarto-mode-hook 'electric-quote-local-mode)
(add-hook 'poly-quarto-mode-hook 'lbr/setup-smart-quotes)

(defun lbr/quarto-preview ()
  "Launch quarto preview for the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (async-shell-command
     (format "quarto preview %s --to revealjs --no-browser" (shell-quote-argument file))
     "*quarto-preview*")))

(provide 'lbr-quarto)
