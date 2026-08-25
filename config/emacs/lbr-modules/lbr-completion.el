;; -*- lexical-binding: t -*-

(use-package vertico
  :init (vertico-mode 1))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :config
  ;; sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :custom
  (marginalia-align 'right)
  :init 
  (marginalia-mode 1))

(use-package embark
  :init)

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(global-set-key (kbd "C->") 'embark-act)

(use-package consult
  :init)

(defvar lbr/org-source
  (list :name     "Org Buffer"
        :category 'buffer
        :narrow   ?o
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :new
        (lambda (name)
          (with-current-buffer (get-buffer-create name)
            (insert "#+title: " name "\n\n")
            (org-mode)
            (consult--buffer-action (current-buffer))))
        :items
        (lambda ()
          (consult--buffer-query :mode 'org-mode :as #'consult--buffer-pair))))

(add-to-list 'consult-buffer-sources 'lbr/org-source 'append)

(use-package which-key
  :config (which-key-mode)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

(use-package cape
  :after corfu
  :config
  ;; complete file path
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; comlete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'lbr-completion)
