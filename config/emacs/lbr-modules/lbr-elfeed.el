;; -*- lexical-binding: t -*-

(use-package elfeed
  :config
  (setq elfeed-db-directory "~/.local/share/elfeed"))

(use-package password-store
  :config
  (defconst fever-url-zt (password-store-get "zt_server_fever"))
  (defconst fever-api-url (password-store-get "zt_server_fever_api"))
  (defconst fever-password (password-store-get "freshrss_api")))

(use-package elfeed-protocol
  :config
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-curl-extra-arguments '("--insecure"))
  (setq elfeed-protocol-fever-fetch-category-as-tag t)
  (setq elfeed-protocol-fever-update-unread-only t)

  (setq elfeed-feeds `((,fever-url-zt
                                 :api-url ,fever-api-url
                                 :password ,fever-password)))

  (setq elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-enable))

;; workaround to sync unread status
;; https://github.com/fasheng/elfeed-protocol/issues/71
(defun elfeed-protocol-fever-sync-unread-stat ()
  "Set all entries in search view to read and fetch latest unread entries."
  (interactive)
  (mark-whole-buffer)
  (cl-loop for entry in (elfeed-search-selected)
           do (elfeed-untag-1 entry 'unread))
  (let ((clean-url (replace-regexp-in-string "^fever\\+" "" fever-url-zt)))
  (elfeed-protocol-fever--do-update clean-url 'update-unread)))

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
