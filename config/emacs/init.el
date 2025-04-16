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
    (setq custom-file null-device) ;; persistent customize settings need to be made with init.el

    (setq inhibit-startup-screen t)
    
    (setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

    (menu-bar-mode -1) 
    (scroll-bar-mode -1)
    (horizontal-scroll-bar-mode -1)
    (tool-bar-mode -1)
    (tooltip-mode -1)
    (blink-cursor-mode -1)

    (save-place-mode 1) 
    (savehist-mode 1)
    (recentf-mode 1)
    (global-auto-revert-mode 1) ;; refresh buffers with file changes

    (setq frame-inhibit-implied-resize t)

    (setq use-short-answers t)

    (setq scroll-step            1
      scroll-conservatively  10000)

    (setq display-line-numbers-type 'relative)

    (setq-default tab-width 2) ;; visual tab width of 2
    (setq-default standard-indent 2) ;; insert 2 spaces with TAB
    (setq-default electric-indent-mode nil) ;; no automatic identation
    (setq-default indent-tabs-mode nil) ;; use spaces everywhere
        
    (setq-default display-line-numbers-width 3) ;; make line numbers column three digits wide

    (set-face-attribute 'default nil :family "Mononoki Nerd Font Mono" :weight 'light :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Mononoki Nerd Font Mono" :weight 'light :height 120)
    (set-face-attribute 'variable-pitch nil :family "Atkinson Hyperlegible" :weight 'medium :height 120)

    ;; needed for fonts to show properly in emacsclient
    (add-to-list 'default-frame-alist '(font . "Mononoki Nerd Font Mono-12"))

    ;; italics for comments (works on emacsclient)
    (set-face-attribute 'font-lock-comment-face nil
      :slant 'italic)
          
    ;; make copy and paste work on wayland (https://www.emacswiki.org/emacs/CopyAndPaste) 
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
    (setq interprogram-paste-function 'wl-paste)

    (setq truncate-string-ellipsis "…") ;; Unicode ellipsis rather than "..."
    (setq sentence-end-double-space nil) ;; Make sure sentences end with one space

    (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;; indentation
(defun my/indentation-config ()
  (setq tab-width 2)
  (setq standard-indent 2)
  (setq electric-indent-mode nil)
  (setq indent-tabs-mode nil))
(add-hook 'prog-mode-hook 'my/indentation-config)
(add-hook 'text-mode-hook 'my/indentation-config)
(add-hook 'org-mode-hook 'my/indentation-config)

(use-package xclip
  :config
  (setq xclip-program "wl-copy")
  (setq xclip-select-enable-clipboard t)
  (setq xclip-mode t)
  (setq xclip-method (quote wl-copy)))

(use-package doom-themes
  :config
   ;; Global settings (defaults)
   (setq doom-themes-enable-bold t) ;; if nil, bold is universally disabled
         doom-themes-enable-italic t) ;; if nil, italics is universally disabled
(load-theme 'doom-gruvbox t)
;; Corrects (and improves) org-mode's native fontification
(doom-themes-org-config)

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

(use-package doom-modeline
   :init (doom-modeline-mode 1)
   :config
    (setq doom-modeline-height 15
          doom-modeline-enable-word-count t
          doom-modeline-buffer-encoding nil
          doom-modeline-icon nil))

(use-package evil
  :init
    (setq evil-want-integration t ;; This is optional since it's already set to t by default.
          evil-want-keybinding nil
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-undo-system 'undo-redo ;; add C-r redo functionality
          evil-respect-visual-line-mode t)
          ;; evil-default-cursor "#2ecc71"
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

(use-package which-key
  :config (which-key-mode)
  :custom
    (which-key-max-description-length 40)
    (which-key-lighter nil)
    (which-key-sort-order 'which-key-description-order))

(use-package rainbow-mode
  :hook prog-mode)

(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

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

(add-hook 'prog-mode-hook 'hl-line-mode)
;; (add-hook 'prog-mode-hook 'visual-line-mode)

(global-set-key (kbd "C-S-v") 'yank) ;; added this for pasting URLs into minibuffer

(use-package general
  :config
    (general-evil-setup)
    ;; use SPACE as global leader key
    (general-create-definer my/leader-keys
      :states '(normal insert visual emacs)
      :keymaps 'override
      :prefix "SPC" ;; set leader
      :global-prefix "M-SPC") ;; use leader in insert mode
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
      "o s" '(org-insert-source-code-block :wk "Insert Org source code block")
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

(defun org-insert-source-code-block ()
  "Insert source code block and optionally set a lanugage"
  (interactive)
  (let ((col (current-column))
        (lang (read-from-minibuffer "Source block language (blank for none): ")))
    (insert (format "#+begin_src%s" (if (string-empty-p lang) "" (concat " " lang))))
    (newline)(newline)
    (move-to-column col t)(insert "#+end_src")(newline)
    (forward-line -2)(move-to-column col t)))

(use-package mixed-pitch
    :hook (text-mode . mixed-pitch-mode))

(use-package olivetti
  :config
    (setq olivetti-body-width 100)
  :hook (text-mode . olivetti-mode))

(add-hook 'text-mode-hook (lambda () 
                            (fringe-mode 1) ;; needed for olivetti to work
                            (display-line-numbers-mode -1)))

(add-hook 'prog-mode-hook (lambda ()
                            (fringe-mode -1)
                            (display-line-numbers-mode 1)))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :custom
    (org-modern-star nil)
    (org-modern-block-name nil))
(with-eval-after-load 'org (global-org-modern-mode))

(setq-default prettify-symbols-alist
                '(("#+begin_src"    . "≫")
                  ("#+end_src"      . "≫")))
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; unmap keys in 'evil-maps, otherwise (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop))
;; set ENTER key in org-mode to follow links
(setq org-return-follows-link t)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-directory "~/org/")

(setq org-startup-with-inline-images t)
(setq org-startup-folded t)
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(setq org-ellipsis " [+]")
;; (setq org-ellipsis " ▼")
(setq org-use-sub-superscripts "{}")
(setq org-M-RET-may-split-line '((default . nil)))

(setq org-cycle-separator-lines -1)

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)
      ;; org-edit-src-content-indentation 0)
      ;; org-src-preserve-indentation t)

(defun my/org-font-setup()
  (set-face-attribute 'org-level-1 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-5 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-6 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-7 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-8 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold))
(add-hook 'org-mode-hook #'my/org-font-setup)

;; agenda
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

(setq org-agenda-window-setup 'only-window) ;; agenda uses whole window
(setq org-agenda-restore-windows-after-quit t) ;; restore window configuration on exit

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

(defun my/org-agenda-font-setup()
  (set-face-attribute 'org-super-agenda-header nil :inherit 'outline-1 :height 1.2 :weight 'bold))
(add-hook 'org-agenda-mode-hook #'my/org-agenda-font-setup)

(setq org-agenda-span 7
      org-agenda-start-day "+0d"
      org-agenda-block-separator nil
      org-agenda-compact-blocks t)

;; separator line between days in org-agenda calendar view
(setq org-agenda-format-date (lambda (date) (concat "\n"
                                                    (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date))))

;; (setq org-agenda-tags-column 0) ;; put agenda view tags straight after items 
;; (setq org-agenda-hide-tags-regexp (regexp-opt '("Inbox-Phone" "Intray" "Research" "Teaching" "Service" "Perso" "Technology"))) ;; hide specific agenda view tags (filetags)
(setq org-agenda-hide-tags-regexp ".*") ;; hide all agenda view tags

(use-package org-super-agenda
    :hook (org-agenda-mode . org-super-agenda-mode))

(setq org-super-agenda-groups
      '(
        (:name "Today"
               :time-grid t
               :date today
               :scheduled today
               :order 1)
        (:name "Overdue"
               :scheduled past
               :order 2
               :face 'error)
        (:name "Refile"
               :tag "Intray" 
               :tag "Inbox-Phone"
               :order 3)
        (:name "Research"
              :tag "Research"
              :order 4)
        (:name "Teaching"
              :tag "Teaching"
              :order 5)
        (:name "Service"
              :tag "Service"
              :order 6)
        (:name "Perso"
              :tag "Perso"
              :order 7)
        (:name "Technology"
              :tag "Technology"
              :order 8)))

(setq org-agenda-custom-commands
      '(("z" "Teaching"
         ((todo "" ((org-agenda-span 'day)
          (org-super-agenda-groups
           '(
              (:name "FA205 Creative Computing"
               :tag "FA205"
               :order 1)
              (:name "DES102G Design for Sustainable Futures"
               :tag "DES102G"
               :order 2)
              (:name "DES303 Design Research Practice"
               :tag "DES303"
               :order 3)
              (:name "DES232 Smart Homes and Cities"
               :tag "DES232"
               :order 4)
              (:discard (:anything t))))))))
        ("u" "By headline"
         ((todo "" ((org-agenda-span 'day)
          (org-super-agenda-groups
           '((:auto-parent t)))))))
        ("A" "Week plan"
         ((agenda "" ((org-agenda-span 7)
          (org-agenda-start-day "+0d")
          (org-agenda-include-deadlines t)
          (org-super-agenda-groups nil)))))))

;; evil key configurations for org-agenda
(evil-set-initial-state 'org-agenda-mode 'normal)
(defvar org-agenda-mode-map)
(general-define-key
  ;; :keymaps 'org-agenda-mode-map
  :keymaps 'org-super-agenda-header-map
  ;; :states '(normal motion)
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
    "E" 'org-agenda-entry-text-mode
)
(general-define-key
  :keymaps 'org-agenda-mode-map
  :prefix "SPC"
  :states '(normal motion)
    "" '(:ignore t :which-key "Agenda")
    "t" 'org-agenda-todo
    "/" 'org-agenda-filter-by-tag
    "b k" 'org-agenda-quit
)

;; calendar
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

;; references
(setq org-cite-csl-styles-dir
      (expand-file-name "~/.local/share/zotero/styles"))

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

;; org-roam
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

;; email
(use-package mu4e
             :straight
             (:local-repo "/usr/share/emacs/site-lisp/mu4e/"
                          :type built-in)
             :commands (mu4e)
             :config
             (evil-define-key 'normal mu4e-main-mode-map (kbd "q") 'bury-buffer) ;; bury buffer instead of quitting
             (setq
               mu4e-change-filenames-when-moving t ;; avoid syncing issues with mbsync
               mu4e-view-show-images t
               mu4e-view-show-addresses t
               mu4e-compose-context-policy nil
               mu4e-compose-complete-only-personal t
               ;; mu4e-compose-in-new-frame t
               mu4e-compose-format-flowed t
               mu4e-confirm-quit nil
               mu4e-hide-index-messages t

               ;; disable threading
               mu4e-headers-show-threads nil
               mu4e-headers-include-related nil

               ;; mu4e-header-highlight-face (underline nil)
               mu4e-headers-auto-update t
               mu4e-headers-advance-after-mark t

               mu4e-trash-without-flag t ;; otherwise trashing removes emails from server

               mu4e-maildir "~/mail"
               mu4e-get-mail-command "true" ;; using cron job and goimapnotify to get mail
               mu4e-update-interval nil)
                           
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
                     
             ;; sending email
             (setq sendmail-program "/usr/bin/msmtp" 
                   send-mail-function #'smtpmail-multi-send-it
                   message-sendmail-f-is-evil t
                   message-sendmail-extra-arguments '("--read-envelope-from")
                   message-send-mail-function #'message-send-mail-with-sendmail)

             ;; don't ask for context when starting mu4e (default to uoa) 
             (setq mu4e-context-policy 'pick-first)

             (setq mu4e-maildir-shortcuts
                   '((:maildir "/perso/Inbox"       :key ?p)
                     (:maildir "/perso/Sent"        :key ?w)
                     (:maildir "/uoa/Inbox"         :key ?i)
                     (:maildir "/uoa/Sent Items"    :key ?s)))

             (mu4e t)
)
;; spell check
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;; (setq truncate-partial-width-windows nil) 

;; email alerts
(add-hook 'mu4e-index-updated-hook
  (defun new-mail-alert ()
    (shell-command "mail_alert&")))
;; prevent buffer showing output
(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
