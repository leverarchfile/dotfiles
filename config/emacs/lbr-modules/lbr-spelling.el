;; -*- lexical-binding: t -*-

(use-package flyspell
  :init (flyspell-mode)
  :custom
    (setq ispell-program-name "hunspell"
          ispell-really-hunspell t
          ispell-dictionary "en_GB"
          ispell-silently-savep t
          ispell-personal-dictionary "~/.hunspell_en_GB")
  :hook (text-mode . flyspell-mode)
  :hook (prog-mode . flyspell-prog-mode))
  
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
  :bind (:map flyspell-mouse-map ("RET" . flyspell-correct-at-point))
  :bind (:map flyspell-mouse-map ([mouse-1] . flyspell-correct-at-point)))

(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

(provide 'lbr-spelling)
