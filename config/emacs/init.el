;; -*- lexical-binding: t -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package emacs
  :config
  (setq custom-file null-device) ; persistent settings need to be made with init.el
  (setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

  (setq-default 
    inhibit-startup-screen t
    inhibit-startup-message t)

  (menu-bar-mode -1) 
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (fringe-mode 1)

  (save-place-mode 1) 
  (savehist-mode 1)
  (recentf-mode 1)
  (global-auto-revert-mode 1) ; refresh buffers with file changes
  (setq auto-revert-verbose nil) ; don't clutter minibuffer with "Reverting..." messages

  (setq cursor-in-non-selected-windows nil) ; hide cursor in non-active windows

  (setq frame-inhibit-implied-resize t)

  (setq use-short-answers t)

  (setq scroll-step            1
    scroll-conservatively  10000)

  (setq display-line-numbers-type 'relative)
  (setq-default display-line-numbers-width 3) ; make line numbers column three digits wide

  (setq-default electric-indent-mode nil) ; no automatic identation
  (setq-default indent-tabs-mode nil) ; use spaces everywhere
  
  (setq truncate-string-ellipsis "…") ; unicode ellipsis rather than "..."
  (setq sentence-end-double-space nil)) ; make sure sentences end with one space

(use-package emacs
  :config
  (setq tab-always-indent 'complete)
  (setq-default tab-width 2
                indent-tabs-mode nil))

;; make copy and paste work on wayland (https://www.emacswiki.org/emacs/CopyAndPaste) 
(use-package emacs
  :config
  (setq wl-copy-process nil)

  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe
                                        :noquery t))
      (process-send-string wl-copy-process text)
      (process-send-eof wl-copy-process))

  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
         nil ; should return nil if we're the current paste owner
        (shell-command-to-string "wl-paste -n | tr -d \r")))

  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(use-package xclip
  :config
  (setq xclip-program "wl-copy")
  (setq xclip-select-enable-clipboard t)
  (setq xclip-mode t)
  (setq xclip-method (quote wl-copy)))

(defun copy-link-url-at-point ()
  "Copy URL of link at point."
  (interactive)
  (let ((url (or (thing-at-point 'url t)
                 (get-text-property (point) 'url)
                 (when (button-at (point))
                   (button-get (button-at (point)) 'url)))))
    (if url
        (progn
          (kill-new url)
          (message "Copied URL: %s" url))
      (message "No URL found at point"))))

(defun copy-current-file-path ()
  "Copy current buffer's file path and show it in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless file-name (user-error "Buffer not visiting a file"))
    (kill-new file-name)
    (minibuffer-message "%s" file-name)))

(use-package dired
  :straight nil
  :config
  (setq dired-vc-rename-file t))

(use-package dired-subtree
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("<C-tab>" . dired-subtree-cycle)))

(use-package magit)

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
           :inherit standard
           :variable-pitch-family "Libertinus Serif")
          (presentation
           :default-height 180)
          (t
           :default-family "Iosevka"
           :default-height 115
           :fixed-pitch-family "Iosevka"
           :variable-pitch-family "Aporetic Serif"          
           :variable-pitch-height 1.0)))
  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

(use-package mixed-pitch
    :hook (text-mode . mixed-pitch-mode))

(use-package ef-themes
  :config
  (setq ef-themes-common-palette-overrides
        '((prose-done fg-dim))))

(use-package doric-themes
 :config
 (setq doric-themes-to-toggle '(doric-earth doric-obsidian))
 (doric-themes-select 'doric-obsidian))

(defun my-switch-theme ()
    (interactive)
    (doric-themes-toggle)
    (my/org-font-setup))

(use-package spacious-padding
  :init 
  (setq spacious-padding-subtle-mode-line t)
  (spacious-padding-mode 1))

(setq spacious-padding-widths
        '( :right-divider-width 1
           :mode-line-width 0))

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

(use-package olivetti
  :config
  (setq olivetti-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t) 
  :hook (text-mode . olivetti-mode))

(add-hook 'text-mode-hook (lambda () 
                            (display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode 1)))

(use-package logos
  :config
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . "^;;;+ ")
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5,\\}$\\)" ))
          (markdown-mode . "^\\#+ +"))))

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

(defvar org-source
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

(add-to-list 'consult-buffer-sources 'org-source 'append)

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

(use-package flyspell
  :init (flyspell-mode)
  :custom
    (setq ispell-program-name "hunspell"
          ispell-really-hunspell t
          ispell-dictionary "en_GB"
          ispell-silently-savep t
          ispell-personal-dictionary "~/.hunspell_en_GB")
  :hook (text-mode . flyspell-mode)
  :hook (prog-mode . flyspell-prog-mode))
  
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
  :bind (:map flyspell-mouse-map ("RET" . flyspell-correct-at-point))
  :bind (:map flyspell-mouse-map ([mouse-1] . flyspell-correct-at-point)))

(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-S-v") 'yank) ; added this for pasting URLs into minibuffer

(use-package general
  :config
  (general-evil-setup)
  ;; use SPACE as global leader key
  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ; set leader
    :global-prefix "M-SPC") ; use leader in insert mode
  (my/leader-keys
    ;; buffers
    "b" '(:ignore t :wk "Buffers")
    "b b" '(consult-buffer :wk "Show buffers")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    ;; capture
    "c" '(:ignore t :wk "Capture")
    "c c" '(org-capture :wk "New capture")
    "c f" '(org-capture-finalize :wk "Finish")
    "c r" '(org-capture-refile :wk "Refile")
    "c k" '(org-capture-kill :wk "Abort")
    ;; dired/diff
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d e" '(ediff :wk "Ediff two files")
    "d f" '(diff :wk "Show differences between two files")
    "d j" '(dired-jump :wk "Dired jump to current")
    ;; eval
    "e" '(:ignore t :wk "Eval/Ediff")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e e" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    ;; files
    "f" '(:ignore t :wk "Files")
    "f a" '(consult-org-agenda :wk "Jump to org agenda heading")
    "f c" '(copy-current-file-path :wk "Copy current file path")
    "f d" '(kill-current-buffer :wk "Kill current buffer")
    "f f" '(basic-save-buffer :wk "Save buffer")
    "f g" `(,(general-simulate-key "C-x g") :wk "Magit status buffer")
    "f h" '(consult-org-heading :wk "Find org heading")
    "f l" '(consult-line :wk "Find line in current buffer")
    "f p" '(consult-yank-pop :wk "Search clipboard to paste")
    "f r" '(consult-recent-file :wk "Find recent files")
    "f s" '(find-file :wk "Find file")
    ;; comments
    "g c" '(comment-line :wk "Comment lines")
    ;; images
    "i" '(:ignore t :wk "Images")
    "i i" '(org-toggle-inline-images :wk "Org-toggle-inline-images")
    ;; jump/narrow
    "j" '(:ignore t :wk "Logos")
    "j j" '(logos-forward-page-dwim :wk "Logos next section")
    "j k" '(logos-backward-page-dwim :wk "Logos previous section")
    "j f" '(logos-narrow-dwim :wk "Logos narrow/widen")
    "j a" '(my/org-next-level-1-headline :wk "Next level 1 org")
    "j s" '(my/org-previous-level-1-headline :wk "Previous level 1 org")
    ;; links
    "l" '(:ignore t :wk "Links")
    "l c" '(copy-link-url-at-point :wk "Copy URL of link at point")
    "l l" '(org-insert-link :wk "Insert a link")
    "l s" '(org-store-link :wk "Store a link")
    ;; mail
    "m" '(:ignore t :wk "Mail")
    "m m" '(mu4e :wk "Start mu4e")
    "m s" '(message-send-and-exit :wk "Send email")
    ;; org
    "o" '(:ignore t :wk "Org")
    "o a" '(org-agenda :wk "Org agenda")
    "o A" '(org-archive-subtree :wk "Move current subtree to the archive")
    "o c" '(my/org-insert-str-template :wk "Insert Org source code block")
    "o d" `(,(general-simulate-key "C-c C-d") :wk "Org deadline")
    "o e" `(,(general-simulate-key "C-c '") :wk "Edit src block or exit edit")
    "o f" `(,(lambda() (interactive)(find-file "~/org/projects.org")) :wk "Open projects.org")
    "o g" '(org-set-tags-command :wk "Set Org tags")
    "o h" `(,(general-simulate-key "C-c .") :wk "Org timestamp")
    "o q" '(org-insert-structure-template :wk "Insert structure template")
    "o s" `(,(general-simulate-key "C-c C-s") :wk "Org schedule")
    "o t" '(org-todo :wk "Org todo")
    "o T" '(org-todo-list :wk "Org todo list")
    ;; references
    "q" '(:ignore t :wk "References")
    "q k" '(citar-org-kill-citation :wk "Kill citation")
    "q o" '(citar-open :wk "Open library, notes etc")
    "q p" '(org-cite-csl-activate-render-all :wk "Fontify citations in the buffer")
    "q q" '(citar-insert-citation :wk "Insert citation")
    "q r" '(citar-insert-reference :wk "Insert reference")
    "q u" '(citar-org-update-prefix-suffix :wk "Update citation prefix/suffix")
    "q w" '(my/org-cite-noauthor :wk "Insert narrative citation")
    ;; refile
    "r" '(:ignore t :wk "Refile")
    "r r" '(org-refile :wk "Org refile")
    "r c" '(org-refile-copy :wk "Org refile copy, original item stays in place")
    "r g" '(org-refile-goto-last-stored :wk "Jump to location of last refiled item")
    ;; slips
    "s" '(:ignore t :wk "Slips")
    "s a" '(citar-denote-dwim :wk "Access attachments etc. for bib. slip")
    "s b" '(denote-backlinks :wk "Backlinks for slip")
    "s f" '(denote-open-or-create :wk "Open or reate slip")
    "s l" '(denote-link :wk "Insert Denote link")
    "s m" '(denote-link-after-creating :wk "Create new slip and link")
    "s q" '(citar-denote-open-note :wk "Open bib. slip")
    "s r" '(citar-create-note :wk "New bib. slip")
    "s s" '(denote :wk "New slip with Denote")
    ;; toggle
    "t" '(:ignore t :wk "Toggle")
    "t e" '(my-switch-theme :wk "Toggle ef-themes")
    "t f" '(flyspell-mode :wk "Toggle flyspell")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    ;; elfeed
    "u" '(:ignore t :wk "Elfeed")
    "u r" '(elfeed-update :wk "Update elfeed")
    "u s" '(elfeed-protocol-fever-sync-unread-stat :wk "Sync unread RSS with elfeed")
    "u u" '(elfeed :wk "Open elfeed")
    ;; windows
    "w" '(:ignore t :wk "Windows")
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; move windows
    "w a" '(evil-window-rotate-upwards :wk "Switch windows around")))

(use-package evil
  :config 
  (with-eval-after-load 'message
    (evil-define-key 'insert message-mode-map (kbd "TAB") #'message-tab))
  ;; unmap keys in 'evil-maps, otherwise (setq org-return-follows-link t) will not work
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command)))

;; insert new line without moving cursor
(with-eval-after-load 'evil-maps
  (define-key evil-insert-state-map (kbd "C-<return>") 'open-line))

;; evil key configurations for org-agenda
(evil-set-initial-state 'org-agenda-mode 'normal)
(general-define-key
  :keymaps 'org-agenda-mode-map
  :prefix "SPC"
  :states '(normal motion)
    "a" '(:ignore t :wk "Agenda")
    "a e" '(org-agenda-earlier :wk "Earlier view")
    "a l" '(org-agenda-later :wk "Later view")
    "a m" '(org-agenda-month-view :wk "Month view")
    "a t" '(org-agenda-todo :wk "All todos")
    "a /" '(org-agenda-filter-by-tag :wk "Filter by tag"))

(defun my/org-next-level-1-headline ()
  "Jump to the next level 1 org headline."
  (interactive)
  (let ((found nil))
    (save-excursion
      (end-of-line)
      (while (and (not found) (outline-next-heading))
        (when (= (org-current-level) 1)
          (setq found (point)))))
    (when found (goto-char found))))

(defun my/org-previous-level-1-headline ()
  "Jump to the previous level 1 org headline."
  (interactive)
  (let ((found nil))
    (save-excursion
      (beginning-of-line)
      (while (and (not found) (outline-previous-heading))
        (when (= (org-current-level) 1)
          (setq found (point)))))
    (when found (goto-char found))))

;; use 'o' to view elfeed entry in vertical split
;; make sure 'q' deletes the split window
(with-eval-after-load 'elfeed
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    "o" #'my-elfeed-search-open-other-window)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "q" (lambda ()
          (interactive)
          (elfeed-kill-buffer)
          (when (> (count-windows) 1)
            (delete-window)))))

(use-package org
  :init
  (setq org-directory "~/org/")
  (setq org-archive-location "~/org/archive/%s::")
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-cycle-include-plain-lists nil)
  (setq org-pretty-entities t)
  (setq org-ellipsis "⮧")
  (setq org-use-sub-superscripts "{}")
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-return-follows-link t) ; use ENTER key to follow links
  (setq org-startup-with-inline-images t)
  (setq org-startup-folded t)
  (setq org-statup-indented t)    
  (setq org-enforce-todo-dependencies t)
  (setq org-insert-heading-respect-content t)
  (setq org-cycle-separator-lines -1)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t) ; e.g. to have an overline extend beyond the text
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t))

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org
  :config
  (setq org-tags-column 0) ; put tags one space after headline text
  (setq org-use-property-inheritance t)   
  (setq org-enforce-todo-dependencies t)

  (setq org-todo-keywords
    '((sequence "ONGO(o)" "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SKIP(s)")))
   
  (setq org-tag-alist
    '((:startgroup)
      ("Teaching" . ?t)
      (:grouptags)
      ("FA110" . ?i) ("FA205" . ?f) ("DES232" . ?h) ("DES240" . ?d) ("DES304" . ?c) ("PhD" . ?p)
      (:endgroup)
      (:startgroup)
      ("Service" . ?s)
      (:grouptags)
      ("Extern" . ?x) ("ReDes" . ?r) ("TnL" . ?l) ("AI" . ?a)       (:endgroup)
      (:startgroup)
      ("Perso" . ?u)
      (:grouptags)
      ("CV" . ?v) ("Divers" . ?y)
      (:endgroup)
      (:startgroup)
      ("Computing" . ?c)
      (:grouptags)
      ("Emacs" . ?e) ("FW13" . ?w) ("SprN" . ?n) ("Lab" . ?b)
      (:endgroup))))

;; org-insert-structure-template and create new line inside the block
(defun my/org-insert-str-template ()
  (interactive)
  (let ((pt (point)))
    (call-interactively #'org-insert-structure-template)
    (goto-char pt)
    (search-forward "#+begin_src")
    (forward-line 1)
    (insert "\n")
    (forward-line -1)))

;; options for source blocks when using org-insert-structure-template (SPC-o-s)
(use-package org
  :config
  (setq org-structure-template-alist
          '(("s" . "src")
            ("e" . "src emacs-lisp")
            ("b" . "src bash")
            ("j" . "src javascript")
            ("p" . "src python")
            ("q" . "quote")
            ("x" . "example")
            ("X" . "export"))))

(defun my/org-font-setup ()
  (set-face-attribute 'org-level-1 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold :overline t)
  (set-face-attribute 'org-level-2 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-5 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-6 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-7 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-8 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold))
(add-hook 'org-mode-hook #'my/org-font-setup)

(use-package org
  :config
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (setq org-agenda-window-setup 'only-window) ; agenda uses whole window
  (setq org-agenda-restore-windows-after-quit t) ; restore window configuration on exit
  
  (setq org-agenda-span 7
        org-agenda-start-day "+0d"
        org-agenda-block-separator nil
        org-agenda-compact-blocks nil) ; hides header when set to true ('t')

  (setq org-agenda-time-leading-zero t)
  
  (setq org-deadline-warning-days 3)
 
  ;; empty line between days in agenda to space things out 
  (setq org-agenda-format-date
        (lambda (date)
          (concat "\n"
                  (org-agenda-format-date-aligned date)))))

(use-package org
  :config
  (setq org-agenda-custom-commands
      '(;; done/skipped tasks to archive
        ("#" "To archive" todo "DONE|SKIP")
        
        ;; day view and scheduled/deadlines for next 7 days
        ("u" "Today and coming up"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %d %B %Y")
                      (org-agenda-overriding-header "\nToday\n")))
          (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil) ; show week ahead
                      (org-agenda-start-day "+1d") ; start from tomorrow
                      (org-agenda-span 7)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-format-date "\n%a %d %B %Y") ; new line before each day
                      (org-agenda-overriding-header "\nUpcoming (+7d)")))))

        ;; refile 
        ("r" "Refile"
         ((tags "CATEGORY=\"Refile\"" ((org-agenda-files '("~/org/agenda.org"))
                                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "^\\*+ Refile$")) ; exclude headline itself
                                       (org-agenda-time-grid nil)
                                       (org-agenda-prefix-format "  ")
                                       (org-agenda-remove-tags t)
                                       (org-agenda-overriding-header "\nRefile")))
          (tags-todo "TODO={TODO}" ((org-agenda-files '("~/org/inbox-phone.org"))
                                    (org-agenda-prefix-format "  ")
                                    (org-agenda-overriding-header "\nRefile (phone)")))))

        ;; views to export, e.g. for desktop widget
        ("z" . "Desktop views")
        ("zt" "Today"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%a %d %B %Y")
                      (org-agenda-time-grid nil)
                      (org-agenda-overriding-header "")
                      (org-agenda-remove-tags t)))))
        ("zu" "Upcoming"
         ((agenda "" ((org-agenda-span 1) ; separate first entry to avoid empty first line
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-show-all-dates nil)
                      (org-agenda-block-separator nil)
                      (org-agenda-format-date "%a %d %B %Y")
                      (org-agenda-time-grid nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-overriding-header "")
                      (org-agenda-remove-tags t)))
          (agenda "" ((org-agenda-span 6)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+2d")
                      (org-agenda-show-all-dates nil)
                      (org-agenda-block-separator nil)
                      (org-agenda-format-date "\n%a %d %B %Y")
                      (org-agenda-time-grid nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-overriding-header "")
                      (org-agenda-remove-tags t)))))

        ;; scheduled tasks for this week
        ("w" . "This week's scheduled/deadline tasks")
        ("we" "This week's tasks" agenda "Scheduled tasks for this week"
         ((org-agenda-files '("~/org/projects.org" "~/org/agenda.org"))
         (org-agenda-use-time-grid nil)))
        ("ww" "This week's work tasks" agenda "Scheduled work tasks for this week"
         ((org-agenda-category-filter-preset '("-Perso" "-Computing")) 
         (org-agenda-use-time-grid nil)))
        ("wp" "This week's perso tasks" agenda "Scheduled non-work tasks for this week"
         ((org-agenda-category-filter-preset '("+Perso" "+Computing")) 
         (org-agenda-use-time-grid nil)))

        ;; views for ONGO & NEXT tasks 
        ("n" . "What's next")
        ("nn" "All ONGO & NEXT" tags-todo "TODO={ONGO\\|NEXT}")
        ("nw" "Work ONGO & NEXT" tags-todo "TODO={ONGO\\|NEXT}"
         ((org-agenda-category-filter-preset '("-Perso" "-Computing"))))
        ("nu" "Perso/Comp ONGO & NEXT" tags-todo "TODO={ONGO\\|NEXT}"
         ((org-agenda-category-filter-preset '("+Perso" "+Computing"))))
        ("nr" "Research ONGO & NEXT" tags-todo "TODO={ONGO\\|NEXT}"
         ((org-agenda-category-filter-preset '("+Research"))))
        ("ns" "Service ONGO & NEXT" tags-todo "TODO={ONGO\\|NEXT}"
         ((org-agenda-category-filter-preset '("+Service"))))
        ("nt" "Teaching ONGO & NEXT" tags-todo "TODO={ONGO\\|NEXT}"
         ((org-agenda-category-filter-preset '("+Teaching"))))
        ("nc" "Computing ONGO & NEXT" tags-todo "TODO={ONGO\\|NEXT}"
         ((org-agenda-category-filter-preset '("+Computing"))))
        ("np" "Perso ONGO & NEXT" tags-todo "TODO={ONGO\\|NEXT}"
         ((org-agenda-category-filter-preset '("+Perso"))))
        
        ;; views for TODO tasks without SCHEDULED/DEADLINE
        (";" . "What is there to do?")
        (";;" "All TODOs" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\"")
        (";w" "Work TODOs" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\""
         ((org-agenda-category-filter-preset '("+Research" "+Teaching" "+Service"))))
        (";p" "Perso/Comp TODOs" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\""
         ((org-agenda-category-filter-preset '("-Research" "-Teaching" "-Service"))))

        ;; views for WAIT tasks without SCHEDULED/DEADLINE
        ("h" . "What is waiting?")
        ("hh" "All WAITs" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\"")
        ("hw" "Work WAITs" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\""
         ((org-agenda-category-filter-preset '("-Perso" "-Computing"))))
        ("hp" "Perso/Comp WAIT" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\""
         ((org-agenda-category-filter-preset '("+Perso" "+Computing"))))
        
        ;; views for deadlines within a range of 90 days +- of their warning period 
        ("!" . "Deadlines")
        ("!!" "All deadlines" agenda "Past and upcoming deadlines"
	       ((org-agenda-span 1)
	        (org-deadline-warning-days 90)
	        (org-agenda-entry-types '(:deadline))))
        ("!w" "Work deadlines" agenda "Past and upcoming work deadlines"
	       ((org-agenda-span 1)
	        (org-agenda-category-filter-preset '("-Perso" "-Computing"))
	        (org-deadline-warning-days 90)
	        (org-agenda-entry-types '(:deadline))))
        ("!p" "Perso/Comp deadlines" agenda "Past and upcoming perso/comp deadlines"
	       ((org-agenda-span 1)
	        (org-agenda-category-filter-preset '("+Perso" "+Computing"))
	        (org-deadline-warning-days 90)
	        (org-agenda-entry-types '(:deadline)))))))

;; show org-agenda list on startup
(add-hook 'server-after-make-frame-hook (lambda ()
                                          (setq olivetti-body-width 100)
                                          (olivetti-mode)
                                          (org-agenda nil "a")))

(add-hook 'org-agenda-mode-hook (lambda ()
                                  (setq olivetti-body-width 100)
                                  (olivetti-mode)))

;; automatically export two agenda views as .txt files whenever an org agenda file is saved 
(defun my-update-desktop-agenda-files ()
  (let ((inhibit-message t))
    (save-window-excursion
      (org-agenda nil "zt")
      (org-agenda-write "~/.local/share/agenda-today.txt")
      (org-agenda nil "zu")
      (org-agenda-write "~/.local/share/agenda-upcoming.txt"))))

(defun my-check-update-desktop-agenda-files ()
  (when (and buffer-file-name
             (member (file-truename buffer-file-name) 
                     (mapcar 'file-truename org-agenda-files)))
    (my-update-desktop-agenda-files)))

(add-hook 'after-save-hook #'my-check-update-desktop-agenda-files)

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(defun my/count-refile-items ()
  "Count items to refile: level-2 headlines under '* Refile' 
in agenda.org and level-1 headlines in inbox-phone.org."
  (let ((count 0))
    ;; Count level-2 headlines under '* Refile' in agenda.org
    (with-current-buffer (find-file-noselect "~/org/agenda.org")
      (org-with-wide-buffer
       (when-let ((pos (org-find-exact-headline-in-buffer "Refile")))
         (goto-char pos)
         (setq count (length (org-map-entries t "LEVEL=2" 'tree))))))
    ;; Count level-1 headlines in inbox-phone.org
    (with-current-buffer (find-file-noselect "~/org/inbox-phone.org")
      (setq count (+ count (length (org-map-entries t "LEVEL=1" 'file)))))
    count))

;; update waybar after org-capture and when agenda.org or inbox-phone.org are saved
;; covers orgzly-revived syncs provided global-auto-revert-mode is 1

(defun my/update-waybar ()
  (start-process-shell-command "waybar-update" nil "pkill -RTMIN+1 waybar"))

;; Update Waybar after capturing
(add-hook 'org-capture-after-finalize-hook #'my/update-waybar)

;; Update Waybar when these specific files are saved or reverted
(defun my/setup-waybar-update-hooks ()
  (when (member (expand-file-name (buffer-file-name))
                (list (expand-file-name "~/org/agenda.org")
                      (expand-file-name "~/org/inbox-phone.org")))
    (add-hook 'after-save-hook #'my/update-waybar nil t)
    (add-hook 'after-revert-hook #'my/update-waybar nil t)))

(add-hook 'org-mode-hook #'my/setup-waybar-update-hooks)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

(use-package org-bullets
  :init
  (setopt org-bullets-bullet-list '("◉" "○" "◆" "◇" "◇" "◇" "◇" "◇"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook 'electric-quote-local-mode)
(add-hook 'electric-quote-inhibit-functions 'org-in-src-block-p)
(setq electric-quote-replace-double t)

;; Configure evil-surround for org-mode: curly quotes and no spaces added to parentheses
(add-hook 'org-mode-hook
          (lambda ()
            ;; Insert curly quotes with evil-surround (using Unicode escapes)
            (push '(?\" . ("\u201c" . "\u201d")) evil-surround-pairs-alist)   ; " "
            (push '(?\' . ("\u2018" . "\u2019")) evil-surround-pairs-alist)   ; ' '
            ;; Override default parentheses to not add spaces
            (push '(?\( . ("(" . ")")) evil-surround-pairs-alist)
            (push '(?\[ . ("[" . "]")) evil-surround-pairs-alist)
            (push '(?\{ . ("{" . "}")) evil-surround-pairs-alist)
            
            ;; Define text objects that recognise both curly and straight quotes
            (evil-define-text-object evil-inner-smart-double-quote (count &optional beg end type)
              (or (ignore-errors (evil-select-paren "\u201c" "\u201d" beg end type count nil))
                  (ignore-errors (evil-select-paren "\"" "\"" beg end type count nil))))
            (evil-define-text-object evil-outer-smart-double-quote (count &optional beg end type)
              (or (ignore-errors (evil-select-paren "\u201c" "\u201d" beg end type count t))
                  (ignore-errors (evil-select-paren "\"" "\"" beg end type count t))))
            (evil-define-text-object evil-inner-smart-single-quote (count &optional beg end type)
              (or (ignore-errors (evil-select-paren "\u2018" "\u2019" beg end type count nil))
                  (ignore-errors (evil-select-paren "'" "'" beg end type count nil))))
            (evil-define-text-object evil-outer-smart-single-quote (count &optional beg end type)
              (or (ignore-errors (evil-select-paren "\u2018" "\u2019" beg end type count t))
                  (ignore-errors (evil-select-paren "'" "'" beg end type count t))))
            
            (define-key evil-inner-text-objects-map "\"" 'evil-inner-smart-double-quote)
            (define-key evil-outer-text-objects-map "\"" 'evil-outer-smart-double-quote)
            (define-key evil-inner-text-objects-map "'" 'evil-inner-smart-single-quote)
            (define-key evil-outer-text-objects-map "'" 'evil-outer-smart-single-quote)))

;; taken from org-modern
;; https://github.com/minad/org-modern

(defface my-org-horizontal-rule
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face for horizontal rules in org mode.")

(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("^[ \t]*-\\{5,\\}$" 0
                 '(face my-org-horizontal-rule 
                   display (space :width (- text 1))))))))

(setq calendar-holidays
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-fixed 2 6 "Waitangi Day")
        (holiday-fixed 2 14 "Valentine's Day")
        (holiday-fixed 4 25 "Anzac Day")
        (holiday-fixed 12 25 "Christmas Day")
        (holiday-fixed 12 26 "Boxing Day")
        (holiday-easter-etc -21 "UK Mother's Day")
        (holiday-easter-etc -2 "Good Friday")
        (holiday-easter-etc 0 "Easter Sunday")
        (holiday-easter-etc 1 "Easter Monday")
        (holiday-float 6 0 3 "UK Father's Day")
        (holiday-float 6 1 1 "King's Birthday")
        (holiday-float 10 1 4 "Labour Day")))

(use-package calfw)

(setq cfw:display-calendar-holidays nil)

(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

(use-package calfw-org)

(setq org-capture-templates
  '(;; todos to refile later
    ("t" "TODO to refile" entry
     (file+headline "agenda.org" "Refile")
     "* TODO %?"
     :empty-lines-after 1)
    ("e" "TODO email to refile" entry
     (file+headline "agenda.org" "Refile")
     "* TODO email from %:fromname\n :PROPERTIES:\n :SUBJECT: %:subject\n :EMAIL: %:fromaddress\n :THREAD: %l\n :DATE: %:date\n :NOTES: %?\n :END:"
     :empty-lines-after 1)
    ("q" "TODO email quick refile" entry
     (file+headline "agenda.org" "Refile")
     "* TODO email from %:fromname\n :PROPERTIES:\n :SUBJECT: %:subject\n :EMAIL: %:fromaddress\n :THREAD: %l\n :DATE: %:date\n :END:"
     :empty-lines-after 1
     :immediate-finish t)

    ;; RSS from elfeed
    ("r" "RSS from elfeed" entry
     (file+headline "agenda.org" "Refile")
     "* [[%:external-link][%:title]]\n"
     :empty-lines-after 1
     :immediate-finish t)

    ;; reminders
    ("d" "Schedule reminder")
    ("ds" "Schedule reminder for today" entry
     (file+headline "agenda.org" "Reminders")
     "* %^{Title for reminder}\nSCHEDULED: %t\n %?"
     :empty-lines-after 1)
    ("dl" "Schedule reminder for another day" entry
     (file+headline "agenda.org" "Reminders")
     "* %^{Title for reminder}\nSCHEDULED: %^t\n %?"
     :empty-lines-after 1)

    ;; appointments (scheduled and repeating events)
    ("m" "Appointments")
    ("mw" "Work appointment" entry
     (file+headline "agenda.org" "Work")
     "* %^{Title?}\n %?\n SCHEDULED: %^t"
     :empty-lines-after 1)
    ("me" "Work appointment from email" entry
     (file+headline "agenda.org" "Work")
     "* %^{Title?}\n :PROPERTIES:\n :SUBJECT: %:subject\n :EMAIL: %:fromaddress\n :THREAD: %l\n :DATE: %:date\n :NOTES: %?\n SCHEDULED: %^t\n :END:"
     :empty-lines-after 1)
    ("mp" "Personal appointment" entry
     (file+headline "agenda.org" "Personal")
     "* %^{Title?}\n %?\n SCHEDULED: %^t"
     :empty-lines-after 1)
    ("mm" "Personal appointment from email" entry
     (file+headline "agenda.org" "Personal")
     "* %^{Title?}\n :PROPERTIES:\n :SUBJECT: %:subject\n :EMAIL: %:fromaddress\n :THREAD: %l\n :DATE: %:date\n :NOTES: %?\n SCHEDULED: %^t\n :END:"
     :empty-lines-after 1)

    ;; todos in location
    ("a" "Add TODO in location")
    ("ar" "TODO for research" entry
     (file+headline "projects.org" "Research")
     "* TODO %?"
     :empty-lines-after 1)
    ("at" "TODO for teaching" entry
     (file+headline "projects.org" "Teaching")
     "* TODO %?"
     :empty-lines-after 1)
    ("as" "TODO for service" entry
     (file+headline "projects.org" "Service")
     "* TODO %?"
     :empty-lines-after 1)
    ("ap" "TODO for perso" entry
     (file+headline "projects.org" "Perso")
     "* TODO %?"
     :empty-lines-after 1)
    ("ac" "TODO for computing" entry
     (file+headline "projects.org" "Computing")
     "* TODO %?"
     :empty-lines-after 1)))

(setq org-capture-templates-contexts
      '(("e" ((in-mode . "message-mode")
              (in-mode . "mu4e-headers-mode")
              (in-mode . "mu4e-view-mode")))
        ("q" ((in-mode . "message-mode")
              (in-mode . "mu4e-headers-mode")
              (in-mode . "mu4e-view-mode")))
        ("me" ((in-mode . "message-mode")
              (in-mode . "mu4e-headers-mode")
              (in-mode . "mu4e-view-mode")))
        ("mm" ((in-mode . "message-mode")
              (in-mode . "mu4e-headers-mode")
              (in-mode . "mu4e-view-mode")))
        ("r" ((in-mode . "elfeed-show-mode")))))

;; Run capture command in a popup frame
;; Taken from Protesilaos Stavrou, with some minor modifications
;; https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/ 

(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'. Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))

(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame. Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter. Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)
                                (height . 18)
                                (title . "emacs-window-popup")))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

;; autoload 'prot-window-popup-org-capture "prot-window"
(prot-window-define-with-popup-frame org-capture)

(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

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

(defun my/org-cite-noauthor ()
  "Insert citation with 'noauthor' style, e.g. for APA narrative citation"
  (interactive)
   (let ((start (point)))
    (call-interactively #'org-cite-insert)
    (when (looking-back "\\[cite:\\(.*?\\)\\]" (line-beginning-position))
      (let* ((match (match-string 0))
             (keys (match-string 1))
             (styled (format "[cite/na:%s]" keys)))
        (replace-match styled t t)))))

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

(use-package pdf-tools
  :init
  (pdf-tools-install)
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-+" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink)))

(use-package elfeed
  :config
  (setq elfeed-db-directory "~/.local/share/elfeed"))

(use-package password-store
  :config
  (defconst fever-url-zt (password-store-get "zt_server_fever"))
  (defconst fever-api-url (password-store-get "zt_server_fever_api"))
  (defconst fever-password (password-store-get "freshrss_api")))

(use-package elfeed-protocol
  :config
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-curl-extra-arguments '("--insecure"))
  (setq elfeed-protocol-fever-fetch-category-as-tag t)
  (setq elfeed-protocol-fever-update-unread-only t)

  (setq elfeed-protocol-feeds `((,fever-url-zt
                                 :api-url ,fever-api-url
                                 :password ,fever-password)))

  (setq elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-enable))

;; workaround to sync unread status
;; https://github.com/fasheng/elfeed-protocol/issues/71
(defun elfeed-protocol-fever-sync-unread-stat ()
  "Set all entries in search view to read and fetch latest unread entries."
  (interactive)
  (mark-whole-buffer)
  (cl-loop for entry in (elfeed-search-selected)
           do (elfeed-untag-1 entry 'unread))
  (let ((clean-url (replace-regexp-in-string "^fever\\+" "" fever-url-zt)))
  (elfeed-protocol-fever--do-update clean-url 'update-unread)))

;; org-store-link for elfeed
(org-link-set-parameters "elfeed"
  :follow #'elfeed-link-open
  :store  #'elfeed-link-store-link
  :export #'elfeed-link-export-link)

(defun my-elfeed-search-open-other-window ()
  "Open elfeed entry in other window."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (win (selected-window)))
    (with-current-buffer (get-buffer "*elfeed-search*")
      (unless (one-window-p)
        (delete-other-windows win))
      (split-window-sensibly win)
      (other-window 1)
      (elfeed-search-show-entry entry))))

(use-package mu4e
  :straight
  (:local-repo "/usr/share/emacs/site-lisp/mu4e/"
               :type built-in)
  :commands (mu4e)
  :config
  (setq
    mu4e-maildir "~/mail"
    mu4e-get-mail-command "true" ; using cron job and goimapnotify to get mail
    mu4e-update-interval nil
    mu4e-change-filenames-when-moving t ; avoid syncing issues with mbsync
    mu4e-view-show-images t
    mu4e-view-show-addresses t
    mu4e-compose-context-policy nil
    mu4e-compose-complete-only-personal t
    mu4e-compose-dont-reply-to-self t
    mu4e-compose-format-flowed t
    mu4e-confirm-quit nil
    mu4e-hide-index-messages t
    mu4e-modeline-support nil

    ;; disable threading
    mu4e-headers-show-threads nil
    mu4e-headers-include-related nil

    ;; mu4e-header-highlight-face (underline nil)
    mu4e-headers-auto-update t
    mu4e-headers-advance-after-mark t

    mu4e-trash-without-flag t) ; otherwise trashing removes emails from server
    
  (setq mu4e-maildir-shortcuts
        '((:maildir "/perso/Inbox"       :key ?p)
          (:maildir "/perso/Sent"        :key ?w)
          (:maildir "/uoa/Inbox"         :key ?i)
          (:maildir "/uoa/Sent Items"    :key ?s)))

  ;; view messages in browser with 'aV'
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; spell check
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

  ;; bury buffer instead of quitting
  (evil-define-key 'normal mu4e-main-mode-map (kbd "q") 'bury-buffer))

(use-package mu4e
  :config
  (setq mu4e-contexts
       (list
         ;;uoa
         (make-mu4e-context
           :name "uoa"
           :match-func
             (lambda (msg)
               (when msg
                 (string-prefix-p "/uoa" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address . "l.baldwin-ramult@auckland.ac.nz")
                     (user-full-name . "Leo Baldwin-Ramult")
                     (mu4e-sent-folder . "/uoa/Sent Items")
                     (mu4e-drafts-folder . "/uoa/Drafts")
                     (mu4e-refile-folder . "/uoa/Archive")
                     (mu4e-trash-folder . "/uoa/Deleted Items")
                     (mu4e-sent-messages-behavior . delete))) ; IMAP takes care of this

         ;; perso
         (make-mu4e-context
           :name "perso"
           :match-func
             (lambda (msg)
               (when msg
                 (string-prefix-p "/perso" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address . "mail@leverarchfile.org")
                     (user-full-name . "Leo Baldwin-Ramult")
                     (mu4e-sent-folder . "/perso/Sent")
                     (mu4e-drafts-folder . "/perso/Drafts")
                     (mu4e-refile-folder . "/perso/Archive")
                     (mu4e-trash-folder . "/perso/Trash")
                     (mu4e-sent-messages-behavior . sent))))) ; IMAP doesn't take care of this
  
  ;; don't ask for context when starting mu4e (default to uoa) 
  (setq mu4e-context-policy 'pick-first))

(use-package mu4e
  :config
  (setq sendmail-program "/usr/bin/msmtp" 
        send-mail-function #'smtpmail-multi-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))

;; email alerts
(use-package mu4e
  :config
  (defun new-mail-alert ()
    (start-process-shell-command "mail-alert" nil "mail_alert")) ; calls script in ~/.local/bin
  (add-hook 'mu4e-index-updated-hook #'new-mail-alert))

;; Emacs Everywhere for Wayland, by Thanos Apollo
;; https://thanosapollo.org/posts/use-emacs-everywhere/
(defun thanos/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype -s 350 \"%s\""
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype -s 350 \"%s\" && wtype -k Return"
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(defun thanos/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (erase-buffer)
    (org-mode)
    (setq-local header-line-format
                (format " %s to insert text or %s to cancel."
                        (propertize "C-c C-c" 'face 'help-key-binding)
			(propertize "C-c C-k" 'face 'help-key-binding)))
    (local-set-key (kbd "C-c C-k")
		   (lambda () (interactive)
		     (kill-new (buffer-string))
		     (delete-frame)))
    (local-set-key (kbd "C-c C-c")
		   (lambda () (interactive)
		     (start-process-shell-command
		      "wtype" nil
		      (thanos/wtype-text (buffer-string)))
		     (delete-frame)))))
