;; -*- lexical-binding: t -*-

(use-package pdf-tools
  :config
  (pdf-tools-install)
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-+" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink)))

(provide 'lbr-pdf)
