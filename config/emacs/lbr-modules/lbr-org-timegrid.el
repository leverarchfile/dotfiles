;; -*- lexical-binding: t -*-

;;; org-timegrid

(use-package org-timegrid
  :straight (:host github :repo "Gleek/org-timegrid")
  :commands (org-timegrid-week)
  :init
  (setq org-timegrid-org-capture-file "~/org/timegrid.org"
        org-timegrid-org-capture-template
        '(:target datetree
          :template "* %{title}\n%{time-range}\n%?"))
  (setq org-timegrid-start-hour 6
        org-timegrid-end-hour 22)
  (setq org-timegrid-default-zoom 0.85)
  :config
  (require 'org-timegrid-org)
  (add-to-list 'display-buffer-alist
            '("\\*Org Time Grid\\*" (display-buffer-same-window))))

;;; Evil keybindings for the calendar buffer

(with-eval-after-load 'org-timegrid
  (evil-set-initial-state 'org-timegrid-mode 'normal)
  (evil-define-key 'normal org-timegrid-mode-map
    ;; cursor
    "j" #'org-timegrid-cursor-forward
    "k" #'org-timegrid-cursor-backward
    "h" #'org-timegrid-cursor-backward-day
    "l" #'org-timegrid-cursor-forward-day
    "gg" #'org-timegrid-cursor-day-start
    "G" #'org-timegrid-cursor-day-end
    (kbd "C-d") #'org-timegrid-cursor-page-down
    (kbd "C-u") #'org-timegrid-cursor-page-up
    "zz" #'org-timegrid-recenter
    (kbd "<escape>") #'org-timegrid-dismiss
    ;; blocks
    "n" #'org-timegrid-next-block
    "N" #'org-timegrid-previous-block
    (kbd "RET") #'org-timegrid-open-at-cursor
    "J" #'org-timegrid-move-later
    "K" #'org-timegrid-move-earlier
    "H" #'org-timegrid-move-previous-day
    "L" #'org-timegrid-move-next-day
    ">" #'org-timegrid-grow-end
    "<" #'org-timegrid-shrink-end
    "(" #'org-timegrid-grow-start
    ")" #'org-timegrid-shrink-start
    "r" #'org-timegrid-edit-selected-title
    "t" #'org-timegrid-retime-selected
    "x" #'org-timegrid-remove-selected
    "yy" #'org-timegrid-copy-selected
    "dd" #'org-timegrid-cut-selected
    "p" #'org-timegrid-yank
    "u" #'org-timegrid-undo
    (kbd "C-r") #'org-timegrid-redo
    ;; navigation
    "{" #'org-timegrid-previous-week
    "}" #'org-timegrid-next-week
    "gt" #'org-timegrid-goto-today
    "gd" #'org-timegrid-goto-date
    "gr" #'org-timegrid-refresh
    "/" #'org-timegrid-isearch-forward
    "?" #'org-timegrid-isearch-backward
    "q" #'quit-window
    ;; org actions on the selected entry
    "T" #'org-timegrid-org-todo
    "#" #'org-timegrid-org-set-tags
    "," #'org-timegrid-org-priority
    "I" #'org-timegrid-org-clock-in
    "O" #'org-timegrid-org-clock-out
    "A" #'org-timegrid-org-archive
    "R" #'org-timegrid-org-refile
    "E" #'org-timegrid-org-set-effort
    "Z" #'org-timegrid-org-add-note))

(provide 'lbr-org-timegrid)
