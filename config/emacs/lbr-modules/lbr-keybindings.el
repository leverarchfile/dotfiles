;; -*- lexical-binding: t -*-

;;; General key edits

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-S-v") 'yank) ; added this for pasting URLs into minibuffer

;;; Keybindings with SPC leader key

(use-package general
  :config
  (general-evil-setup)
  ;; use SPACE as global leader key
  (general-create-definer lbr/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ; set leader
    :global-prefix "M-SPC") ; use leader in insert mode
  (lbr/leader-keys
    ;; agenda
    "a" '(:ignore t :wk "Agenda")
    "a e" '(org-agenda-earlier :wk "Earlier view")
    "a l" '(org-agenda-later :wk "Later view")
    "a m" '(org-agenda-month-view :wk "Month view")
    "a t" '(org-agenda-todo :wk "All todos")
    "a /" '(org-agenda-filter-by-tag :wk "Filter by tag")
    ;; buffers
    "b" '(:ignore t :wk "Buffers")
    "b b" '(consult-buffer :wk "Show buffers")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    ;; capture
    "c" '(:ignore t :wk "Capture")
    "c c" '(org-capture :wk "New capture")
    "c f" '(org-capture-finalize :wk "Finish")
    "c r" '(org-capture-refile :wk "Refile")
    "c k" '(org-capture-kill :wk "Abort")
    ;; dired/diff
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d e" '(ediff :wk "Ediff two files")
    "d f" '(diff :wk "Show differences between two files")
    "d j" '(dired-jump :wk "Dired jump to current")
    ;; eval
    "e" '(:ignore t :wk "Eval/Ediff")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e e" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    ;; files
    "f" '(:ignore t :wk "Files")
    "f a" '(consult-org-agenda :wk "Jump to org agenda heading")
    "f c" '(lbr/copy-current-file-path :wk "Copy current file path")
    "f d" '(kill-current-buffer :wk "Kill current buffer")
    "f f" '(basic-save-buffer :wk "Save buffer")
    "f g" `(,(general-simulate-key "C-x g") :wk "Magit status buffer")
    "f h" '(consult-org-heading :wk "Find org heading")
    "f l" '(consult-line :wk "Find line in current buffer")
    "f o" '(consult-outline :wk "Find org/markdown headline in current buffer")
    "f p" '(consult-yank-pop :wk "Search clipboard to paste")
    "f r" '(consult-recent-file :wk "Find recent files")
    "f s" '(find-file :wk "Find file")
    "f /" '(consult-ripgrep :wk "Search whole directory")
    ;; comments
    "g c" '(comment-line :wk "Comment lines")
    ;; images
    "i" '(:ignore t :wk "Images")
    "i i" '(org-toggle-inline-images :wk "Org-toggle-inline-images")
    ;; jump/narrow
    "j" '(:ignore t :wk "Logos")
    "j j" '(logos-forward-page-dwim :wk "Logos next section")
    "j k" '(logos-backward-page-dwim :wk "Logos previous section")
    "j f" '(logos-narrow-dwim :wk "Logos narrow/widen")
    "j a" '(lbr/org-next-level-1-headline :wk "Next level 1 org")
    "j s" '(lbr/org-previous-level-1-headline :wk "Previous level 1 org")
    ;; links
    "l" '(:ignore t :wk "Links")
    "l c" '(lbr/copy-link-url-at-point :wk "Copy URL of link at point")
    "l l" '(org-insert-link :wk "Insert a link")
    "l s" '(org-store-link :wk "Store a link")
    ;; mail
    "m" '(:ignore t :wk "Mail")
    "m m" '(mu4e :wk "Start mu4e")
    "m s" '(message-send-and-exit :wk "Send email")
    ;; org
    "o" '(:ignore t :wk "Org")
    "o a" '(org-agenda :wk "Org agenda")
    "o A" '(org-archive-subtree :wk "Move current subtree to the archive")
    "o c" '(lbr/org-insert-str-template :wk "Insert Org source code block")
    "o d" `(,(general-simulate-key "C-c C-d") :wk "Org deadline")
    "o e" `(,(general-simulate-key "C-c '") :wk "Edit src block or exit edit")
    "o f" `(,(lambda() (interactive)(find-file "~/org/projects.org")) :wk "Open projects.org")
    "o g" '(org-set-tags-command :wk "Set Org tags")
    "o h" `(,(general-simulate-key "C-c .") :wk "Org timestamp")
    "o q" '(org-insert-structure-template :wk "Insert structure template")
    "o s" `(,(general-simulate-key "C-c C-s") :wk "Org schedule")
    "o t" '(org-todo :wk "Org todo")
    "o T" '(org-todo-list :wk "Org todo list")
    ;; references
    "q" '(:ignore t :wk "References")
    "q k" '(citar-org-kill-citation :wk "Kill citation")
    "q o" '(citar-open :wk "Open library, notes etc")
    "q p" '(org-cite-csl-activate-render-all :wk "Fontify citations in the buffer")
    "q q" '(citar-insert-citation :wk "Insert citation")
    "q r" '(citar-insert-reference :wk "Insert reference")
    "q u" '(citar-org-update-prefix-suffix :wk "Update citation prefix/suffix")
    "q w" '(lbr/org-cite-noauthor :wk "Insert narrative citation")
    ;; refile
    "r" '(:ignore t :wk "Refile")
    "r r" '(org-refile :wk "Org refile")
    "r c" '(org-refile-copy :wk "Org refile copy, original item stays in place")
    "r g" '(org-refile-goto-last-stored :wk "Jump to location of last refiled item")
    ;; slips
    "s" '(:ignore t :wk "Slips")
    "s a" '(citar-denote-dwim :wk "Access attachments etc. for bib. slip")
    "s b" '(denote-backlinks :wk "Backlinks for slip")
    "s f" '(denote-open-or-create :wk "Open or create slip")
    "s g" '(consult-notes :wk "Go to slip")
    "s l" '(denote-link :wk "Insert Denote link")
    "s m" '(denote-link-after-creating :wk "Create new slip and link")
    "s q" '(citar-denote-open-note :wk "Open bib. slip")
    "s r" '(citar-create-note :wk "New bib. slip")
    "s s" '(denote :wk "New slip with Denote")
    "s /" '(consult-notes-search-in-all-notes :wk "Search across all slips")
    ;; toggle
    "t" '(:ignore t :wk "Toggle")
    "t e" '(lbr/switch-theme :wk "Toggle ef-themes")
    "t f" '(flyspell-mode :wk "Toggle flyspell")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    ;; elfeed
    "u" '(:ignore t :wk "Elfeed")
    "u r" '(elfeed-update :wk "Update elfeed")
    "u s" '(elfeed-protocol-fever-sync-unread-stat :wk "Sync unread RSS with elfeed")
    "u u" '(elfeed :wk "Open elfeed")
    ;; windows
    "w" '(:ignore t :wk "Windows")
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; move windows
    "w a" '(evil-window-rotate-upwards :wk "Switch windows around")))

;;; Evil edits for TAB, RET, and SPC

(use-package evil
  :config 
  (with-eval-after-load 'message
    (evil-define-key 'insert message-mode-map (kbd "TAB") #'message-tab))
  ;; unmap keys in 'evil-maps, otherwise (setq org-return-follows-link t) will not work
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command)))

;;; Other Evil edits

;; insert new line without moving cursor
(with-eval-after-load 'evil-maps
  (define-key evil-insert-state-map (kbd "C-<return>") 'open-line))

;;; Org jump between level one headlines

(defun lbr/org-next-level-1-headline ()
  "Jump to the next level 1 org headline."
  (interactive)
  (let ((found nil))
    (save-excursion
      (end-of-line)
      (while (and (not found) (outline-next-heading))
        (when (= (org-current-level) 1)
          (setq found (point)))))
    (when found (goto-char found))))

(defun lbr/org-previous-level-1-headline ()
  "Jump to the previous level 1 org headline."
  (interactive)
  (let ((found nil))
    (save-excursion
      (beginning-of-line)
      (while (and (not found) (outline-previous-heading))
        (when (= (org-current-level) 1)
          (setq found (point)))))
    (when found (goto-char found))))

;;; Elfeed keybindings

;; use 'o' to view elfeed entry in vertical split
;; make sure 'q' deletes the split window
(with-eval-after-load 'elfeed
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    "o" #'lbr/elfeed-search-open-other-window)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "q" (lambda ()
          (interactive)
          (elfeed-kill-buffer)
          (when (> (count-windows) 1)
            (delete-window)))))

(provide 'lbr-keybindings)
