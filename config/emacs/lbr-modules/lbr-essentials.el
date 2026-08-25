;; -*- lexical-binding: t -*-

;;; Main settings

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

  ;; Disable the creation of lockfiles (e.g., .#filename).
  ;; Rely on `global-auto-revert-mode' to handle external file changes gracefully
  (setq create-lockfiles nil)
  
  (setq make-backup-files nil) ; disable backup files (filename~)

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

;;; Server mode

(require 'server)
(unless (server-running-p) (server-start))

;;; Tabs

(use-package emacs
  :config
  (setq tab-always-indent 'complete)
  (setq-default tab-width 2
                indent-tabs-mode nil))

;;; Copy and paste

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

(defun lbr/copy-link-url-at-point ()
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

(defun lbr/copy-current-file-path ()
  "Copy current buffer's file path and show it in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless file-name (user-error "Buffer not visiting a file"))
    (kill-new file-name)
    (minibuffer-message "%s" file-name)))

;;; Dired

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

;;; Magit

(use-package magit)

(provide 'lbr-essentials)
