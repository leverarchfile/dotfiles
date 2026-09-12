;; -*- lexical-binding: t -*-

(use-package password-store
  :config
  (defvar lbr/freshrss-url (password-store-get "freshrss_greader_url"))
  (defvar lbr/freshrss-api-url (password-store-get "freshrss_greader_api"))
  (defvar lbr/freshrss-password (password-store-get "freshrss_api")))

(use-package elfeed
  :after password-store
  :config
  (setq elfeed-db-directory "~/.local/share/elfeed")
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-feeds `((,lbr/freshrss-url
                        :api-url ,lbr/freshrss-api-url
                        :password ,lbr/freshrss-password))))

(use-package elfeed-protocol
  :after elfeed
  :config
  (setq elfeed-protocol-enabled-protocols '(freshrss))
  (elfeed-protocol-enable))

(use-package elfeed-protocol-freshrss
  :straight (elfeed-protocol-freshrss :type git :host nil
             :repo "https://git.repetitions.de/elfeed-protocol-freshrss/")
  :after elfeed-protocol
  :config
  (elfeed-protocol-freshrss-register-protocol))

;; org-store-link for elfeed
(org-link-set-parameters "elfeed"
  :follow #'elfeed-link-open
  :store  #'elfeed-link-store-link
  :export #'elfeed-link-export-link)

(defun lbr/elfeed-search-open-other-window ()
  "Open elfeed entry in other window."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (win (selected-window)))
    (with-current-buffer (get-buffer "*elfeed-search*")
      (unless (one-window-p)
        (delete-other-windows win))
      (split-window-sensibly win)
      (other-window 1)
      (elfeed-search-show-entry entry))))

(provide 'lbr-elfeed)
