;; -*- lexical-binding: t -*-

(setq org-cite-global-bibliography '("~/.local/share/zotero/storage/my_library.bib"))
(setq org-cite-csl-styles-dir (expand-file-name "~/.local/share/zotero/styles"))
(setq org-cite-export-processors '((t csl "apa.csl")))

(use-package citeproc)

(use-package oc-csl-activate
  :straight (oc-csl-activate :type git :host github :repo "andras-simonyi/org-cite-csl-activate")
  :after oc
  :config
  (setq org-cite-csl-activate-use-document-style t
        org-cite-csl-activate-fallback-style "apa.csl"))

(use-package citar
  :straight (citar :type git :host github :repo "emacs-citar/citar" :includes (citar-org))
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths '("~/slips"))
  :hook
  (org-mode . citar-capf-setup))

(use-package citar-org
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(defun lbr/org-cite-noauthor ()
  "Insert citation with 'noauthor' style, e.g. for APA narrative citation"
  (interactive)
   (let ((start (point)))
    (call-interactively #'org-cite-insert)
    (when (looking-back "\\[cite:\\(.*?\\)\\]" (line-beginning-position))
      (let* ((match (match-string 0))
             (keys (match-string 1))
             (styled (format "[cite/na:%s]" keys)))
        (replace-match styled t t)))))

(provide 'lbr-citations)
