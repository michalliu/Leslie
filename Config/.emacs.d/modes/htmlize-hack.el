(require 'htmlize)

(when (equal htmlize-version "1.34")
  (defun htmlize-face-size (face)
    ;; The size (height) of FACE, taking inheritance into account.
    ;; Only works in Emacs 21 and later.
    (let ((size-list
           (loop
            for f = face then (face-attribute f :inherit)
            until (or (null f) (eq f 'unspecified))
            for h = (face-attribute f :height)
            collect (if (eq h 'unspecified) nil h))))
      (reduce 'htmlize-merge-size (cons nil size-list)))))

(provide 'htmlize-hack)
