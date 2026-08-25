;; -*- lexical-binding: t -*-

;;; Org general

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

;;; Org todos and tags

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

;;; Org structure templates

;; org-insert-structure-template and create new line inside the block
(defun lbr/org-insert-str-template ()
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

;;; Org fonts

(defun lbr/org-font-setup ()
  (set-face-attribute 'org-document-title nil :font "Iosevka Etoile" :height 1.4 :weight 'bold)
  (set-face-attribute 'org-level-1 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold :overline t)
  (set-face-attribute 'org-level-2 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-5 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-6 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-7 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-8 nil :font "Iosevka Etoile" :height 1.2 :weight 'bold))
(add-hook 'org-mode-hook #'lbr/org-font-setup)

;;; Org interface

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

;;; Quotation marks

(defun lbr/setup-smart-quotes ()
  "Set up curly quote pairs and text objects for evil-surround."
  ;; Insert curly quotes with evil-surround (using Unicode escapes)
  (push '(?\" . ("\u201c" . "\u201d")) evil-surround-pairs-alist)
  (push '(?\' . ("\u2018" . "\u2019")) evil-surround-pairs-alist)
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
  (define-key evil-outer-text-objects-map "'" 'evil-outer-smart-single-quote))

(add-hook 'org-mode-hook 'electric-quote-local-mode)
(setq electric-quote-replace-double t)
(setq electric-quote-context-sensitive t) ; for single quotes
(add-hook 'org-mode-hook 'lbr/setup-smart-quotes)

;; Fix to allow my quotation config to work with Markdown and Quarto
(defun lbr/org-inhibit-electric-quote ()
  "Inhibit electric quotes inside Org source blocks."
  (and (derived-mode-p 'org-mode)
       (org-in-src-block-p)))
(add-hook 'electric-quote-inhibit-functions 'lbr/org-inhibit-electric-quote)

;;; Horizontal rules in org buffers

;; taken from org-modern
;; https://github.com/minad/org-modern

(defface lbr-org-horizontal-rule
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face for horizontal rules in org mode.")

(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("^[ \t]*-\\{5,\\}$" 0
                 '(face lbr-org-horizontal-rule 
                   display (space :width (- text 1))))))))

;;; Calendar

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

(provide 'lbr-org)
