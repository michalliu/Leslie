;; 基本设置
(setq default-major-mode 'python-mode) ;; 设置默认模式
(setq default-line-spaceing 4)         ;; line-space
(setq default-fill-column 60)          ;; page width
(setq kill-ring-max 800)               ;;kill欢长度，越大则保存历史命令越多 
(setq visible-bell 1)                  ;; 
(setq resize-mini-windows nil)         ;; lock line hight
(setq enable-recursive-minibuffers t)  ;; minibuffer
(setq hs-allow-nesting t)


;; 设置时间显示
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(setq require-final-newline t)

(fset 'yes-or-no-p 'y-or-n-p)


;; 默认激活某些模式
(transient-mark-mode t)                ;; 语法高亮
(global-font-lock-mode 1)              ; for all buffers
;;(setq hs-minor-mode 1)                      ; 代码折叠模式
(global-font-lock-mode 1)              ;; 锁定字体

;; 添加加载目录
(add-to-list 'load-path "~/.emacs.d/modes")

;; 设置某个模式使用某个.el文件
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(autoload 'tsconf-mode "tsconf-mode.el" 
  "Major mode for editing timeseries.ini file" t)


;; 设置后缀名自动识别模式
(setq auto-mode-alist
    (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.ini" . tsconf-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.tks" . java-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist
             '("\\.org\\'" . org-mode))

;; 绑定键
(global-set-key "\C-xl" 'goto-line)     ;; 设置C-x l是跳至某一行
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;(global-set-key "[\C-f1]" 'hd-show-block)
;(global-set-key "[\C-f2]" 'hd-hide-block)


;; 增加hook
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only

(defun my-hs-minor-mode-map-setup ()
  "为hs-minor-mode设置自己的绑定键"
  (define-key hs-minor-mode-map [?\C-c ?h] 'hs-hide-block)    ; C-c h
  (define-key hs-minor-mode-map [?\C-c ?s] 'hs-show-block)    ; C-c s
  (define-key hs-minor-mode-map [?\C-c ?\M-h] 'hs-hide-all)   ; C-c M-h
  (define-key hs-minor-mode-map [?\C-c ?\M-s] 'hs-show-all)   ; C-c M-s
  (define-key hs-minor-mode-map [?\C-c ?H] 'hs-hide-all)      ; C-c H
  (define-key hs-minor-mode-map [?\C-c ?S] 'hs-show-all)      ; C-c S
  (define-key hs-minor-mode-map [?\C-c ?l] 'hs-hide-level)    ; C-c l
  (define-key hs-minor-mode-map [?\C-c ?t] 'hs-toggle-hiding) ; C-c t
  )

;; 关联hs-minor模式
(add-hook 'hs-minor-mode-hook 'my-hs-minor-mode-map-setup t)

;; 将hs-minor模式与其它主要模式关联起来
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)


;;;;; 设置tab为4个空格的宽度 
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil) 
(setq c-basic-offset 4) 
(setq indent-tabs-mode nil) 
(setq default-tab-width 4) 
(setq tab-width 4) 
(setq tab-stop-list ()) 
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96))

;;; for common lisp env
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl --noinform");; Replace "sbcl" with the path to your implementation
