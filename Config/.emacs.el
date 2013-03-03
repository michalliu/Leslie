;; emacs configure
;; 2012-07-21
;; Leslie Chu
;; 

;; defined function
;;(defun hello()     (+ 1 2))
;;(global-set-key [(f4)] 'hello)

;; defined frame location
(setq default-frame-alist
      '((height . 35)(width . 100)(menubar-lines . 20)(tool-bar-lines . 0)))

;; add setting file
;;(load "base.el")

;; programming setting
;;(load "cycode.el")


;; base.el
;; display-time
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;; buffer setting
;; line-space
(setq default-line-spaceing 4)
;; page width
(setq default-fill-column 60)
;; default mode
(setq default-major-mode 'python-mode)
;; kill-ring-max
(setq kill-ring-max 200)
;; end with space
(setq require-final-newline t)
;; high-light
(global-font-lock-mode 1)
;; hight-light mark
(transient-mark-mode t)


;; display buff
;; flash alarm
(setq visible-bell 1)
;; y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; lock line hight
(setq resize-mini-windows nil)
;; minibuffer
(setq enable-recursive-minibuffers t)

;; default dir
;;(setq default-directory "~/KDS/")


;; gdb-ui
;;(setq gdb-many-windows t)
;;(load-library "multi-gud.el")
;;(load-library "multi-gdb-ui.el")


;; 
;;(define-key semantic-tag-folding-mode-map (kbd "C-_") 'semantic-tag-folding-fold-block)

;; 
;;(global-set-key [f3] 'grep-find)
;;(global-set-key [M-f3] 'kill-this-buffer)

;;(global-set-key [(f4)] 'speedbar-get-focus)
;;(global-set-key [C-f4] 'ecb-activate)
;;(global-set-key [f5] 'tool-bar-mode)
;;(global-set-key [C-f5] 'menu-bar-mode)
;;(global-set-key [f6] 'gdb)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defun re (num)
 ;; "multiply 2 and num"
;;  (interactive "p")
;;  (* 2 num))
;;(defun upbuffer ()
;;  "goto other buffer"
;;  (switch-to-buffer (other-buffer)))

;;(set 'command '(switch-to-buffer (other-buffer)))
;;(global-set-key [f6] command)
;;(point)
;;(point-min)
;;(point-max)
;;(buffer-size)
;;(upbuffer)


;;(add-to-list 'load-path "~/Emacs/")
;;(load "~/Emacs/2.el")




(add-to-list 'load-path "~/.emacs.d/modes")
(autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
(setq auto-mode-alist
    (cons '("\\.md" . markdown-mode) auto-mode-alist))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
