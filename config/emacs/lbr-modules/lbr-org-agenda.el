;; -*- lexical-binding: t -*-

;;; Org agenda general

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

;;; Org agenda custom commands

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

;;; Org agenda startup screen

(add-hook 'org-agenda-mode-hook (lambda ()
                                  (setq olivetti-body-width 100)
                                  (olivetti-mode)))

;; show org-agenda list on startup
(defun lbr/startup-agenda ()
  (org-agenda nil "a"))

(add-hook 'emacs-startup-hook #'lbr/startup-agenda)
(add-hook 'server-after-make-frame-hook #'lbr/startup-agenda)

;;; Org agenda export files

;; automatically export two agenda views as .txt files whenever an org agenda file is saved 
(defun lbr/update-desktop-agenda-files ()
  (let ((inhibit-message t))
    (save-window-excursion
      (org-agenda nil "zt")
      (org-agenda-write "~/.local/share/agenda-today.txt")
      (org-agenda nil "zu")
      (org-agenda-write "~/.local/share/agenda-upcoming.txt"))))

(defun lbr/check-update-desktop-agenda-files ()
  (when (and buffer-file-name
             (member (file-truename buffer-file-name) 
                     (mapcar 'file-truename org-agenda-files)))
    (lbr/update-desktop-agenda-files)))

(add-hook 'after-save-hook #'lbr/check-update-desktop-agenda-files)

;;; Org refile

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;;; Count refile items

(defun lbr/count-refile-items ()
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

(defun lbr/update-waybar ()
  (start-process-shell-command "waybar-update" nil "pkill -RTMIN+1 waybar"))

;; update Waybar after capturing
(add-hook 'org-capture-after-finalize-hook #'lbr/update-waybar)

;; update Waybar when these specific files are saved or reverted
(defun lbr/setup-waybar-update-hooks ()
  (when (and (buffer-file-name)
             (member (expand-file-name (buffer-file-name))
                (list (expand-file-name "~/org/agenda.org")
                      (expand-file-name "~/org/inbox-phone.org"))))
    (add-hook 'after-save-hook #'lbr/update-waybar nil t)
    (add-hook 'after-revert-hook #'lbr/update-waybar nil t)))

(add-hook 'org-mode-hook #'lbr/setup-waybar-update-hooks)

(provide 'lbr-org-agenda)
