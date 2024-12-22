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

    (setq use-short-answers t)

    (setq scroll-step            1
      scroll-conservatively  10000)

    (setq display-line-numbers-type 'relative)

    (global-visual-line-mode t)
    (setq-default display-line-numbers-width 3) ;; make line numbers column three digits wide

    (set-face-attribute 'default nil :family "Mononoki Nerd Font Mono" :weight 'light :height 120)
    (set-face-attribute 'fixed-pitch nil :family "Mononoki Nerd Font Mono" :weight 'light :height 120)
    (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :weight 'medium :height 120)

    ;; needed for fonts to show properly in emacsclient
    (add-to-list 'default-frame-alist '(font . "Mononoki Nerd Font Mono-12"))

    ;; italics for comments and keywords (works on emacsclient)
    (set-face-attribute 'font-lock-comment-face nil
      :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil
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

    (setq truncate-string-ellipsis "…")   ;; Unicode ellipsis rather than "..."

    (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(use-package xclip
  :config
  (setq xclip-program "wl-copy")
  (setq xclip-select-enable-clipboard t)
  (setq xclip-mode t)
  (setq xclip-method (quote wl-copy)))

(use-package doom-themes
  :ensure t
  :config
   ;; Global settings (defaults)
   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
   (load-theme 'doom-gruvbox t)
   ;; Corrects (and improves) org-mode's native fontification
   (doom-themes-org-config))

(use-package diminish)

(use-package vertico
  :init (vertico-mode 1))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package doom-modeline
   :init (doom-modeline-mode 1)
   :config
    (setq doom-modeline-height 15
          doom-modeline-enable-word-count t
          doom-modeline-buffer-encoding nil
          doom-modeline-icon nil))

(use-package evil
  :ensure t
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
  :ensure t
  :config
  (evil-collection-init))

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

(use-package ivy
  :diminish
  :config (ivy-mode))

(use-package counsel
  :after ivy
  :diminish
  :config 
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

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

(setq-default indent-tabs-mode nil
               electric-indent-mode nil
               tab-width 4)


(add-hook 'prog-mode-hook 'hl-line-mode)

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
      "f d" '(kill-current-buffer :wk "Kill current buffer")
      "f f" '(basic-save-buffer :wk "Save buffer")
      "f r" '(counsel-recentf :wk "Find recent files")
      "f s" '(find-file :wk "Find file")
      ;; links
      "l" '(:ignore t :wk "Links")
      "l l" '(org-insert-link :wk "Insert a link")
      ;; buffers
      "b" '(:ignore t :wk "Buffers")
      "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
      "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
      "b k" '(kill-current-buffer :wk "Kill current buffer")
      "b n" '(next-buffer :wk "Next buffer")
      "b p" '(previous-buffer :wk "Previous buffer")
      "b r" '(revert-buffer :wk "Reload buffer")
      ;; dired
      "d" '(:ignore t :wk "Dired")
      "d d" '(dired :wk "Open dired")
      "d j" '(dired-jump :wk "Dired jump to current")
      ;; comments
      "g c" '(comment-line :wk "Comment lines")
      ;; org
      "o" '(:ignore t :wk "Org")
      "o a" '(org-agenda :wk "Org agenda")
      "o t" '(org-todo :wk "Org todo")
      "o T" '(org-todo-list :wk "Org todo list")
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
      "w H" '(buf-move-left :wk "Buffer move left")
      "w J" '(buf-move-down :wk "Buffer move down")
      "w K" '(buf-move-up :wk "Buffer move up")
      "w L" '(buf-move-right :wk "Buffer move right")))
;;
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
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; set ENTER key in org-mode to follow links
(setq org-return-follows-link t)

(add-hook 'org-mode-hook 'org-indent-mode)

(require 'org-tempo) ;; source code block with <s TAB

(setq org-directory "~/org/")
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

(setq org-startup-with-inline-images t)
(setq org-startup-folded t)
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(setq org-ellipsis " [+]")
(setq org-use-sub-superscripts "{}")

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)
      ;; org-edit-src-content-indentation 0)
      ;; org-src-preserve-indentation t)

(setq org-cycle-separator-lines 0)

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

(setq org-agenda-span 1
      org-agenda-start-day "+0d"
      org-agenda-block-separator nil
      org-agenda-compact-blocks t)

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
        (:name "Intray"
               :tag "Intray"
               :order 3)
        (:name "Inbox-Phone"
               :tag "Inbox-Phone"
               :order 4)
        (:name "Research"
              :tag "Research"
              :order 5)
        (:name "Teaching"
              :tag "Teaching"
              :order 6)
        (:name "Service"
              :tag "Service"
              :order 7)
        (:name "Perso"
              :tag "Perso"
              :order 8)
        (:name "Technology"
              :tag "Technology"
              :order 9)))

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
           '((:auto-parent t)))))))))

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

;; email
(use-package mu4e
             :straight
             (:local-repo "/usr/share/emacs/site-lisp/mu4e/"
                          :type built-in)
             :commands (mu4e)
             :config
             (setq
               mu4e-change-filenames-when-moving t ;; avoid syncing issues with mbsync
               mu4e-view-show-images t
               mu4e-view-show-addresses t
                
               ;; disable threading
               mu4e-headers-show-threads nil
               mu4e-headers-include-related nil

               ;; mu4e-header-highlight-face (underline nil)

               mu4e-maildir "~/mail"
               mu4e-update-interval (* 10 60) ;; update with isync every 10 minutes
               mu4e-get-mail-command "mbsync -a -c ~/.config/mbsync/config")
                           
             (setq mu4e-contexts
                   (list
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
                                 (mu4e-trash-folder . "/perso/Trash")))
                     ;; uoa
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
                                 (mu4e-trash-folder . "/uoa/Trash")))))
             ;; sending email
             (setq sendmail-program "/usr/bin/msmtp" 
                   send-mail-function #'smtpmail-multi-send-it
                   message-sendmail-f-is-evil t
                   message-sendmail-extra-arguments '("--read-envelope-from")
                   message-send-mail-function #'message-send-mail-with-sendmail)

             ;; ask for context when sending mail, if context hasn't already been chosen
             (setq mu4e-compose-context-policy 'ask-if-none)

             (setq mu4e-maildir-shortcuts
                   '((:maildir "/perso/Inbox"       :key ?p)
                     (:maildir "/perso/Sent"        :key ?w)
                     (:maildir "/uoa/Inbox"         :key ?i)
                     (:maildir "/uoa/Sent Items"    :key ?s)))
)
;; spell check
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
(add-hook 'mu4e-view-mode-hook #'visual-line-mode)
