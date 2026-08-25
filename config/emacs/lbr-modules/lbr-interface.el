;; -*- lexical-binding: t -*-

(require 'lbr-theme)

;;; Fonts

(use-package fontaine
  :hook
  ;; keep last font preset when closing/starting Emacs
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                   ;; set last preset or fall back to regular preset
                   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :config 
  (setq fontaine-presets
        '((regular)
          (alternative
           :variable-pitch-family "Libertinus Serif")
          (xcharter
           :variable-pitch-family "XCharter"
           :variable-pitch-height 1.13)
          (presentation
           :default-height 180)
          (t
           :default-family "IosevkaTermSlab Nerd Font Mono"
           :default-height 115
           :fixed-pitch-family "IosevkaTermSlab Nerd Font Mono"
           :variable-pitch-family "Aporetic Serif"          
           :variable-pitch-height 1.0)))
  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

(use-package mixed-pitch
    :hook (text-mode . mixed-pitch-mode)
    :config
    (setq mixed-pitch-set-height t)
    (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-special-keyword)
    (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-date)
    (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-document-info))

;;; Theme

(use-package doric-themes
 :config
 (setq doric-themes-to-toggle '(doric-obsidian doric-marble))
(doric-themes-select 'doric-obsidian))

(defun lbr/switch-theme ()
    (interactive)
    (doric-themes-toggle)
    (lbr/org-font-setup))

;;; Interface general

(use-package spacious-padding
  :init 
  (setq spacious-padding-subtle-mode-line t)
  (setq spacious-padding-widths
        '( :right-divider-width 1
           :mode-line-width 0))
  (spacious-padding-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
   (setq doom-modeline-height 15
         doom-modeline-enable-word-count t
         doom-modeline-buffer-encoding nil
         doom-modeline-percent-position nil
         doom-modeline-total-line-number t
         doom-modeline-buffer-file-name-style 'buffer-name
         doom-modeline-mu4e nil
         doom-modeline-icon nil))

(use-package rainbow-mode
  :init
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)
  :hook prog-mode)

(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook 'hl-line-mode)
(setq hl-line-sticky-flag nil) ; only highlight line in active window
;; disable hl-line-mode for insert mode
(add-hook 'evil-insert-state-entry-hook (lambda () (when hl-line-mode (hl-line-mode -1))))
(add-hook 'evil-insert-state-exit-hook  (lambda () (when (derived-mode-p 'prog-mode) (hl-line-mode 1))))

(use-package pulsar
  :config
  (setq pulsar-pulse-on-window-change t)
  (pulsar-global-mode 1))

(use-package dired-preview)

;;; Olivetti mode

(use-package olivetti
  :init
  (setq olivetti-body-width 100)
  :config
  (setq olivetti-recall-visual-line-mode-entry-state t) 
  :hook (text-mode . olivetti-mode))

(add-hook 'text-mode-hook (lambda () 
                            (display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode 1)))

;;; Logos

(use-package logos
  :config
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . "^;;;+ ")
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5,\\}$\\)" ))
          (markdown-mode . "^\\#+ +"))))

(provide 'lbr-interface)
