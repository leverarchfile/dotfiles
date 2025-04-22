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
(setq straight-use-package-by-default t)

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

  (save-place-mode 1) 
  (savehist-mode 1)
  (recentf-mode 1)
  (global-auto-revert-mode 1) ; refresh buffers with file changes

  (setq frame-inhibit-implied-resize t)

  (setq use-short-answers t)

  (setq scroll-step            1
    scroll-conservatively  10000)

  (setq display-line-numbers-type 'relative)
  (setq-default display-line-numbers-width 3) ; make line numbers column three digits wide

  (setq-default electric-indent-mode nil) ; no automatic identation
  (setq-default indent-tabs-mode nil) ; use spaces everywhere
  
  (setq truncate-string-ellipsis "…") ; Unicode ellipsis rather than "..."
  (setq sentence-end-double-space nil)) ; Make sure sentences end with one space

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

(defun my/fonts ()
  (set-face-attribute 'default nil :family "Mononoki Nerd Font Mono" :weight 'light :height 120)
  (set-face-attribute 'fixed-pitch nil :family "Mononoki Nerd Font Mono" :weight 'light :height 120)
  (set-face-attribute 'variable-pitch nil :family "Atkinson Hyperlegible" :weight 'medium :height 120)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  ;; needed for fonts to show properly in emacsclient
  (add-to-list 'default-frame-alist '(font . "Mononoki Nerd Font Mono-12")))

(my/fonts)

(use-package mixed-pitch
    :hook (text-mode . mixed-pitch-mode))

(use-package ef-themes
  :config
  (setq ef-themes-common-palette-overrides
      '((prose-done fg-dim)))
  (setq ef-elea-dark-palette-overrides
      '((bg-main "#282828")
        (comment fg-dim)
        (overline-heading-1 red-cooler)
        (bg-inactive bg-alt))))

(defun my-ef-themes-custom-faces ()
  (ef-themes-with-colors
    (custom-set-faces
     `(org-block-begin-line ((,c :background ,bg-main :foreground ,fg-dim)))
     `(org-block-end-line ((,c :background ,bg-main :foreground ,fg-dim)))
     `(org-quote ((,c :background ,bg-main)))
     `(line-number-current-line ((,c :foreground ,fg-dim)))
     `(line-number ((,c :foreground ,border))))))

(add-hook 'ef-themes-post-load-hook #'my-ef-themes-custom-faces)

(ef-themes-select 'ef-dream)

(setq ef-themes-to-toggle '(ef-dream ef-summer))

(defun my-switch-theme ()
  (interactive)
  (ef-themes-toggle)
  (my/org-font-setup)
  (my/org-mode-face-edits))

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

(use-package olivetti
  :config
  (setq olivetti-body-width 100)
  ;; (setq olivetti-body-width 0.7)
  ;; (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t) 
  :hook (text-mode . olivetti-mode))

(add-hook 'text-mode-hook (lambda () 
                            (fringe-mode 1) ; needed for olivetti to work
                            (display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook (lambda ()
                            (fringe-mode -1)
                            (display-line-numbers-mode 1)))

(use-package vertico
  :init (vertico-mode 1))

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
    "f" '(:ignore t :wk "Files")
    "f a" '(consult-org-agenda :wk "Jump to org agenda heading")
    "f d" '(kill-current-buffer :wk "Kill current buffer")
    "f f" '(basic-save-buffer :wk "Save buffer")
    "f h" '(consult-org-heading :wk "Find org heading")
    "f l" '(consult-line :wk "Find line in current buffer")
    "f p" '(consult-yank-pop :wk "Search clipboard to paste")
    "f r" '(consult-recent-file :wk "Find recent files")
    "f s" '(find-file :wk "Find file")
    ;; links
    "l" '(:ignore t :wk "Links")
    "l l" '(org-insert-link :wk "Insert a link")
    "l s" '(org-store-link :wk "Store a link")
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
    ;; dired
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    ;; comments
    "g c" '(comment-line :wk "Comment lines")
    ;; mail
    "m" '(:ignore t :wk "Org")
    "m m" '(mu4e :wk "Start mu4e")
    ;; org
    "o" '(:ignore t :wk "Org")
    "o a" '(org-agenda :wk "Org agenda")
    "o s" '(my/org-insert-str-template :wk "Insert Org source code block")
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
    ;; refile
    "r" '(:ignore t :wk "Refile")
    "r r" '(org-refile :wk "Org refile")
    "r c" '(org-refile-copy :wk "Org refile copy, original item stays in place")
    "r g" '(org-refile-goto-last-stored :wk "Jump to location of last refiled item")
    ;; org-roam
    "s" '(:ignore t :wk "Org-roam")
    "s f" '(org-roam-node-find :wk "Open or create org-roam node")
    "s i" '(org-roam-node-insert :wk "Insert an org-roam node link") 
    "s t" '(org-roam-buffer-toggle :wk "Toggle buffer with org-roam backlinks")
    ;; toggle
    "t" '(:ignore t :wk "Toggle")
    "t e" '(my-switch-theme :wk "Toggle ef-themes")
    "t f" '(flyspell-mode :wk "Toggle flyspell")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
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

;; unmap keys in 'evil-maps, otherwise (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop))

;; evil key configurations for org-agenda
(evil-set-initial-state 'org-agenda-mode 'normal)
(defvar org-agenda-mode-map)
(general-define-key
  :keymaps 'org-agenda-mode-map
    "l" 'org-agenda-later
    "h" 'org-agenda-earlier
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "RET") 'org-agenda-switch-to
    [escape] 'org-agenda-quit
    "q" 'org-agenda-quit
    "s" 'org-save-all-org-buffers
    "t" 'org-agenda-todo
    "T" 'org-agenda-set-tags
    "g" 'org-agenda-redo
    "v" 'org-agenda-view-mode-dispatch
    "." 'org-agenda-goto-today
    "J" 'gs/org-agenda-next-section
    "K" 'gs/org-agenda-prev-section
    "c" 'org-agenda-goto-calendar
    "i" 'org-agenda-clock-in
    "o" 'org-agenda-clock-out
    "E" 'org-agenda-entry-text-mode)

(general-define-key
  :keymaps 'org-agenda-mode-map
  :prefix "SPC"
  :states '(normal motion)
    "" '(:ignore t :which-key "Agenda")
    "t" 'org-agenda-todo
    "/" 'org-agenda-filter-by-tag
    "b k" 'org-agenda-quit)

(use-package org
  :init
  (setq org-directory "~/org/")
  (setq org-archive-location "~/org/archive/%s::")
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-cycle-include-plain-lists nil)
  (setq org-pretty-entities t)
  (setq org-ellipsis " [+]")
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
  (setq org-todo-keywords
    '((sequence "ONGO(o)" "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SKIP(s)")))
  
  (setq org-tag-alist
          '((:startgroup)
	    ("Teaching" . ?t)
	    (:grouptags)
	    ("FA205" . ?f) ("DES102G" . ?g) ("DES303" . ?d) ("DES232" . ?s) ("PhD" . ?p)
	    (:endgroup)
            (:startgroup)
	    ("Service" . ?s)
	    (:grouptags)
	    ("Extern" . ?e) ("ReDes" . ?r) ("PRoT" . ?p) ("AIsc" . ?a) ("IJETA" . ?i)
	    (:endgroup)
            (:startgroup)
	    ("Perso" . ?p)
	    (:grouptags)
	    ("CV" . ?c) ("Divers" . ?d)
	    (:endgroup)
            (:startgroup)
	    ("Computing" . ?c)
	    (:grouptags)
	    ("Emacs" . ?e) ("FW13" . ?f) ("SprN" . ?n) ("Server" . ?s)
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

(defun my/org-mode-face-edits ()
  (set-face-attribute 'org-quote nil :italic nil :inherit 'variable-pitch)
  (with-eval-after-load 'org-modern
   (set-face-attribute 'org-block-begin-line nil
                       :height 0.8
                       :inherit 'fixed-pitch)
   (set-face-attribute 'org-modern-block-name nil
                       :inherit 'org-block-begin-line
                       :height 0.8)
   (set-face-attribute 'org-block-end-line nil
                       :height 0.8
                       :inherit 'fixed-pitch))
   (with-eval-after-load 'org-modern-indent
    (set-face-attribute 'org-modern-indent-bracket-line nil
                       :family "Font Awesome")))
(add-hook 'org-mode-hook #'my/org-mode-face-edits)

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
      org-agenda-compact-blocks t)
  
  ;; separator line between days in org-agenda calendar view
  (setq org-agenda-format-date (lambda (date) (concat "\n"
                                                    (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date)))))

  ;; (setq org-agenda-hide-tags-regexp ".*")) ; hide all agenda view tags

(use-package org
  :config
  (setq org-agenda-custom-commands
      '(;; done/skipped tasks to archive
        ("#" "To archive" todo "DONE|SKIP")

        ;; scheduled tasks for this week
        ("w" . "This week's scheduled/deadline tasks")
        ("we" "This week's tasks" agenda "Scheduled tasks for this week"
         (org-agenda-use-time-grid nil))
        ("ww" "This week's work tasks" agenda "Scheduled work tasks for this week"
         ((org-agenda-category-filter-preset '("-Perso" "-Computing")) 
         (org-agenda-use-time-grid nil)))
        ("wp" "This week's perso tasks" agenda "Scheduled non-work tasks for this week"
         ((org-agenda-category-filter-preset '("+Perso" "+Computing")) 
         (org-agenda-use-time-grid nil)))

        ;; view for ONGO & NEXT tasks 
        ("n" . "What's next")
        ("nn" "All ONGO & NEXT" tags-todo "TODO={ONGO\\NEXT}")
        ("nw" "Work ONGO & NEXT" tags-todo "TODO={ONGO\\NEXT}"
         ((org-agenda-category-filter-preset '("-Perso" "-Computing"))))
        ("nu" "Perso/Comp ONGO & NEXT" tags-todo "TODO={ONGO\\NEXT}"
         ((org-agenda-category-filter-preset '("+Perso" "+Computing"))))
        ("nr" "Research ONGO & NEXT" tags-todo "TODO={ONGO\\NEXT}"
         ((org-agenda-category-filter-preset '("+Research"))))
        ("ns" "Service ONGO & NEXT" tags-todo "TODO={ONGO\\NEXT}"
         ((org-agenda-category-filter-preset '("+Service"))))
        ("nt" "Teaching ONGO & NEXT" tags-todo "TODO={ONGO\\NEXT}"
         ((org-agenda-category-filter-preset '("+Teaching"))))
        ("nc" "Computing ONGO & NEXT" tags-todo "TODO={ONGO\\NEXT}"
         ((org-agenda-category-filter-preset '("+Computing"))))
        ("np" "Perso ONGO & NEXT" tags-todo "TODO={ONGO\\NEXT}"
         ((org-agenda-category-filter-preset '("+Perso"))))
        
        ;; view for TODO tasks without SCHEDULED/DEADLINE
        (";" . "What is there to do?")
        (";;" "All TODOs" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\"")
        (";w" "Work TODOs" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\""
         ((org-agenda-category-filter-preset '("-Perso" "-Computing"))))
        (";p" "Perso/Comp TODOs" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\""
         ((org-agenda-category-filter-preset '("+Perso" "+Computing"))))

        ;; view for WAIT tasks without SCHEDULED/DEADLINE
        ("h" . "What is waiting?")
        ("hh" "All WAITs" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\"")
        ("hw" "Work WAITs" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\""
         ((org-agenda-category-filter-preset '("-Perso" "-Computing"))))
        ("hp" "Perso/Comp WAIT" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\""
         ((org-agenda-category-filter-preset '("+Perso" "+Computing"))))

        ;; view for deadlines within a range of 60 days +- of their warning period 
        ("!" . "Deadlines")
	  ("!!" "All deadlines" agenda "Past and upcoming deadlines"
	   ((org-agenda-span 1)
	    (org-deadline-warning-days 60)
	    (org-agenda-entry-types '(:deadline))))
	  ("!w" "Work deadlines" agenda "Past and upcoming work deadlines"
	   ((org-agenda-span 1)
	    (org-agenda-category-filter-preset '("-Perso" "-Computing"))
	    (org-deadline-warning-days 60)
	    (org-agenda-entry-types '(:deadline))))
	  ("!p" "Perso/Comp deadlines" agenda "Past and upcoming perso/comp deadlines"
	   ((org-agenda-span 1)
	    (org-agenda-category-filter-preset '("+Perso" "+Computing"))
	    (org-deadline-warning-days 60)
	    (org-agenda-entry-types '(:deadline)))))))

;; show org-agenda list on startup
(add-hook 'server-after-make-frame-hook (lambda ()
                                          (fringe-mode 1)
                                          (setq olivetti-body-width 100)
                                          (olivetti-mode)
                                          (org-agenda nil "t")))

(add-hook 'org-agenda-mode-hook (lambda ()
                                  (fringe-mode 1)
                                  (setq olivetti-body-width 100)
                                  (olivetti-mode)))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  
(use-package org-modern
  :custom
  ;; (org-modern-todo-faces
  ;;  '(("ONGO" . (:inverse-video t))
  ;;    ("NEXT" . (:weight bold))
  ;;    ("TODO" . (:weight bold))
  ;;    ("WAIT" . (:inverse-video t))
  ;;    ("CAND" . (:inverse-video t))))
  (org-modern-table nil))
(with-eval-after-load 'org (global-org-modern-mode))

(setq org-modern-star 'replace
      org-modern-replace-stars '("◉" "○" "★" "◇" "◇" "◇" "◇" "◇"))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package calfw)

(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

(use-package calfw-org)

;; refile
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;; capture
(setq org-capture-templates
                   '(("t" "TODO for intray" entry
                      (file+headline "intray.org" "Refile")
                      "* TODO %?")
                     ("c" "TODO from quote for intray" entry
                      (file+headline "intray.org" "Refile")
                      "* TODO %^{Heading for TODO}\n%i %?")
                     ("e" "TODO from email for intray" entry
                      (file+headline "intray.org" "Refile")
                      "* TODO email from %:fromname\n :PROPERTIES:\n :SUBJECT: %:subject\n :EMAIL: %:fromaddress\n :THREAD: %l\n :DATE: %:date\n :NOTES: %?\n :END:")
                     ("r" "Schedule reminder for today" entry
                      (file+headline "intray.org" "Reminders")
                      "* %^{Title for reminder}\nSCHEDULED: %t\n %?")
                     ("l" "Schedule reminder for another day" entry
                      (file+headline "intray.org" "Reminders")
                      "* %^{Title for reminder}\nSCHEDULED: %^t\n %?")
                     ("m" "Appointments")
                     ("mw" "Work meeting" entry
                      (file+headline "meetings.org" "Work")
                      "* Meeting with %^{With?}\n %?\n SCHEDULED: %^t")
                     ("me" "Work meeting from email" entry
                      (file+headline "meetings.org" "Work")
                      "* Meeting with %^{With?}\n :PROPERTIES:\n :SUBJECT: %:subject\n :EMAIL: %:fromaddress\n :THREAD: %l\n :DATE: %:date\n :NOTES: %?\n SCHEDULED: %^t\n :END:")
                     ("mm" "Personal meeting from email" entry
                      (file+headline "meetings.org" "Personal")
                      "* Meeting %^{With/About?}\n :PROPERTIES:\n :SUBJECT: %:subject\n :EMAIL: %:fromaddress\n :THREAD: %l\n :DATE: %:date\n :NOTES: %?\n SCHEDULED: %^t\n :END:")
                     ("mp" "Personal appointment" entry
                      (file+headline "meetings.org" "Personal")
                      "* Meeting %^{Title?}\n %?\n SCHEDULED: %^t")
                     ("a" "Add TODO in location")
                     ("ar" "TODO for research" entry
                      (file+function "research.org" org-ask-location)
                      "* TODO %?")
                     ("at" "TODO for teaching" entry
                      (file+function "teaching.org" org-ask-location)
                      "* TODO %?")
                     ("as" "TODO for service" entry
                      (file+function "service.org" org-ask-location)
                      "* TODO %?")
                     ("ap" "TODO for perso" entry
                      (file+function "perso.org" org-ask-location)
                      "* TODO %?")
                     ("ai" "TODO for technology" entry
                      (file+function "technology.org" org-ask-location)
                      "* TODO %?")))

(defun org-ask-location (&optional prompt targets)
      (let* ((loc-prompt (or prompt "Headline"))
            (org-refile-targets (or targets '((nil :maxlevel . 1))))
            (hd (condition-case nil
                   (car (org-refile-get-location loc-prompt nil t))
                   (error (car org-refile-history)))))
        (goto-char (point-min))
        (outline-next-heading)
        (if (re-search-forward
             (format org-complex-heading-regexp-format (regexp-quote hd))
             nil t)
          (goto-char (point-at-bol))
        (goto-char (point-max)))))

(setq org-capture-templates-contexts
      '(("e" ((in-mode . "message-mode")
              (in-mode . "mu4e-headers-mode")
              (in-mode . "mu4e-view-mode")))
        ("me" ((in-mode . "message-mode")
              (in-mode . "mu4e-headers-mode")
              (in-mode . "mu4e-view-mode")))))

(setq org-cite-csl-styles-dir (expand-file-name "~/.local/share/zotero/styles"))

(setq org-cite-global-bibliography '("~/.local/share/zotero/storage/my_library.bib"))

(setq org-cite-export-processors '((t csl)))

(use-package citeproc)

(use-package oc-csl-activate
  :straight (oc-csl-activate :type git :host github :repo "andras-simonyi/org-cite-csl-activate") 
  :after oc
  :config
  (setq org-cite-csl-activate-use-document-style t))

(use-package citar
  :straight (citar :type git :host github :repo "emacs-citar/citar" :includes (citar-org))
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths '("~/slips/references"))
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

(use-package org-roam
  :custom
  (org-roam-directory "~/slips")
  :config
  (org-roam-db-autosync-mode))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${author} — ${title}"))

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :target (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org" 
                            "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
         :unnarrowed t)
         ("r" "reference" plain
         "%?"
         :target (file+head "references/${citar-citekey}.org"
                            "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
         :unnarrowed t)
        ))

(setq citar-org-roam-capture-template-key "r")

;; update last_modified timestamp for org-roam files
(add-hook 'org-mode-hook (lambda ()
                             (setq-local time-stamp-active t
                                         time-stamp-line-limit 18
                                         time-stamp-start "^#\\+last_modified: [ \t]*"
                                         time-stamp-end "$"
                                         time-stamp-format "\[%Y-%m-%d %a %H:%M:%S\]")
                             (add-hook 'before-save-hook 'time-stamp nil 'local)))

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
                     (mu4e-trash-folder . "/uoa/Deleted Items")))

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
                     (mu4e-trash-folder . "/perso/Trash")))))
  
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
  (add-hook 'mu4e-index-updated-hook
  (defun new-mail-alert ()
    (shell-command "mail_alert&"))) ; calls a script in ~/.local/bin
  ;; prevent buffer showing output
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))
