;;; lbr-theme.el --- Make any theme monochrome or inverted -*- lexical-binding: t -*-

;;; Commentary:

;; make any theme monochrome (and optionally, invert the colours)
;; taken from poet-theme (https://github.com/kunalb/poet) by kunalb (Kunal Bhalla)

;;; Code:

(defun lbr-theme-desaturate-color (color-hex)
  "Converts a color string to its desaturated equivalent hex string"
  (require 'color)
  (apply
   'color-rgb-to-hex
   (append (apply
            'color-hsl-to-rgb
            (apply
             'color-desaturate-hsl
             `(,@(apply 'color-rgb-to-hsl (color-name-to-rgb color-hex)) 100)))
           '(2))))

(defun lbr-theme-transform-colors (fn)
  "Apply FN to the colors on every active face.

   FN should accept the face symbol and the current color,
   and return the new color to be applied."
  (interactive)
  (mapc
   (lambda (face)
     (mapc
      (lambda (attr)
        (let ((current (face-attribute face attr)))
          (unless (or (not current)
                      (listp current)
                      (string= current "unspecified")
                      (string= current "t"))
            (set-face-attribute face nil attr (funcall fn face current)))))
      '(:foreground :background :underline :overline :box :strike-through
                    :distant-foreground))
     (mapc
      (lambda (complex-attr)
        (let* ((full (copy-tree (face-attribute face complex-attr)))
               (current (if (listp full) (member :color full))))
          (unless (or (not current)
                      (not (listp full)))
            (setcar (cdr current) (funcall fn face (cadr current)))
            (set-face-attribute face nil complex-attr full))))
      '(:underline :overline :box)))
   (face-list)))

(defun lbr-theme-desaturate ()
  "Desaturate all currently active face colors."
  (interactive)
  (lbr-theme-transform-colors
   (lambda (face color)
     (lbr-theme-desaturate-color color))))

(defun lbr-theme-invert ()
  "Take the complement of all currently active colors."
  (interactive)
  (require 'color)
  (lbr-theme-transform-colors
   (lambda (face color)
     (apply
      'color-rgb-to-hex
      (color-complement color))))
  (let ((current-ns-appearance (assoc 'ns-appearance default-frame-alist)))
    (cond ((eq (cdr current-ns-appearance) 'light)
           (setf (cdr current-ns-appearance) 'dark))
          ((eq (cdr current-ns-appearance) 'dark)
           (setf (cdr current-ns-appearance) 'light)))))

(provide 'lbr-theme)
;;; lbr-theme.el ends here
