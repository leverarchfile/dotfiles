;;; prot-window.el --- Run commands in a popup frame -*- lexical-binding: t -*-

;;; Commentary:

;; Run capture command in a popup frame
;; Taken from Protesilaos Stavrou, with some minor modifications
;; https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/ 

;;; Code:

(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'. Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))

(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame. Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter. Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)
                                (height . 18)
                                (title . "emacs-window-popup")))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(provide 'prot-window)
;;; prot-window.el ends here
