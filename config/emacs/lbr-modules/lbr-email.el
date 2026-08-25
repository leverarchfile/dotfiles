;; -*- lexical-binding: t -*-

;;; General mu4e settings

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
    mu4e-modeline-support nil

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

;;; Email accounts/'contexts'

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
                     (mu4e-trash-folder . "/uoa/Deleted Items")
                     (mu4e-sent-messages-behavior . delete))) ; IMAP takes care of this

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
                     (mu4e-trash-folder . "/perso/Trash")
                     (mu4e-sent-messages-behavior . sent))))) ; IMAP doesn't take care of this
  
  ;; don't ask for context when starting mu4e (default to uoa) 
  (setq mu4e-context-policy 'pick-first))

;;; Sending email

(use-package mu4e
  :config
  (setq sendmail-program "/usr/bin/msmtp" 
        send-mail-function #'smtpmail-multi-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))

;;; Email alerts

(use-package mu4e
  :config
  (defun new-mail-alert ()
    (start-process-shell-command "mail-alert" nil "mail_alert")) ; calls script in ~/.local/bin
  (add-hook 'mu4e-index-updated-hook #'new-mail-alert))

(provide 'lbr-email)
