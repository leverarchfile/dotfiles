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

(add-to-list 'load-path (locate-user-emacs-file "lbr-modules"))
(add-to-list 'load-path (locate-user-emacs-file "lbr-lisp"))

(require 'lbr-essentials)
(require 'lbr-interface)
(require 'lbr-completion)
(require 'lbr-evil)
(require 'lbr-spelling)
(require 'lbr-keybindings)
(require 'lbr-org)
(require 'lbr-org-agenda)
(require 'lbr-org-capture)
(require 'lbr-citations)
(require 'lbr-denote)
(require 'lbr-markdown)
(require 'lbr-quarto)
(require 'lbr-pdf)
(require 'lbr-elfeed)
(require 'lbr-email)

;; thanos-type must be loaded for the window manager's call to
;; "emacsclient -e '(thanos/type)'" to work
(require 'thanos-type)
