;; -*- lexical-binding: t -*-

;;; Init file for the background Emacs that exports org agenda files to ics

;; Loads org from straight's build
;; Settings come from lbr-org-agenda

(add-to-list 'load-path "~/.config/emacs/straight/build/org")
(require 'ox-icalendar)

(provide 'lbr-org-ics)
