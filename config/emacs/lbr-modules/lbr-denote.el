;; -*- lexical-binding: t -*-

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory "~/slips")
  (setq denote-known-keywords nil) 
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-rename-buffer-format "%t")
  (denote-rename-buffer-mode 1))

(setq denote-org-front-matter
  "#+title:      %1$s
#+date:       %2$s
#+modified:   %2$s
#+filetags:   %3$s
#+identifier: %4$s
\n")

;; required for denote-links dynamic blocks
(use-package denote-org
  :ensure t)

;; auto-update dynamic blocks (including denote-links) on save
(add-hook 'before-save-hook 'org-update-all-dblocks)

;; update the "modified" timestamp when edits are saved
(add-hook 'org-mode-hook (lambda ()
                           (setq-local time-stamp-active t
                                       time-stamp-line-limit 18
                                       time-stamp-start "^#\\+modified:[ \t]*"
                                       time-stamp-end "$"
                                       time-stamp-format "\[%Y-%m-%d %a %H:%M\]")
                           (add-hook 'before-save-hook #'time-stamp nil 'local)))

(use-package citar-denote
  :after (:any citar denote)
  :custom
  (citar-denote-title-format "title") ; default (use nil for citation key)
  (citar-denote-open-attachment nil)  ; don't open attachment when creating new note
  :init
  (citar-denote-mode))

(use-package consult-notes
  :config
  (consult-notes-denote-mode))

(provide 'lbr-denote)
