;; -*- lexical-binding: t -*-

(require 'prot-window)

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

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

;; autoload 'prot-window-popup-org-capture "prot-window"
(prot-window-define-with-popup-frame org-capture)

(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

(provide 'lbr-org-capture)
