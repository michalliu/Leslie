;; Emacs config of Leslie
;; pythonisland@gmail.com

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
(transient-mark-mode t)   ;; 语法高亮
(global-font-lock-mode 1) ; for all buffers
(global-font-lock-mode 1) ;; 锁定字体

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

;; set-buffer-file-eol-type
(global-set-key "\^Cu" (lambda () (interactive) (set-buffer-file-eol-type 'unix)))
(global-set-key "\^Cd" (lambda () (interactive) (set-buffer-file-eol-type 'dos)))
(global-set-key "\^Cm" (lambda () (interactive) (set-buffer-file-eol-type 'mac)))

;; Make the mode-line display the standard EOL-TYPE symbols (used above)...
(setq eol-mnemonic-undecided "(?)" ;; unknown EOL type
      eol-mnemonic-unix  "(unix)"  ;; LF
      eol-mnemonic-dos  "(dos)"    ;; CRLF
      eol-mnemonic-mac  "(mac)")   ;; CR

;; indent-according-to-mode

;; 增加hook
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only


;; 设置代码折叠
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
(add-hook 'html-mode 'hs-minor-mode)
(add-hook 'python-mode 'hs-minor-mode)
(add-hook 'javascript-mode 'hs-minor-mode)



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


;; 设置org-mode输出中文目录
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-export-language-setup
   (quote (("en" "Author" "Date" "Table of Contents" "Footnotes") 
           ("ca" "Autor" "Data" "&Iacute;ndex" "Peus de p&agrave;gina") 
           ("cs" "Autor" "Datum" "Obsah" "Pozn\341mky pod carou") 
           ("da" "Ophavsmand" "Dato" "Indhold" "Fodnoter") 
           ("de" "Autor" "Datum" "Inhaltsverzeichnis" "Fu&szlig;noten") 
           ("eo" "A&#365;toro" "Dato" "Enhavo" "Piednotoj") 
           ("es" "Autor" "Fecha" "&Iacute;ndice" "Pies de p&aacute;gina") 
           ("fi" "Tekij&auml;" "P&auml;iv&auml;m&auml;&auml;r&auml;" "Sis&auml;llysluettelo" "Alaviitteet")
           ("fr" "Auteur" "Date" "Table des mati&egrave;res" "Notes de bas de page") 
           ("hu" "Szerz&otilde;" "D&aacute;tum" "Tartalomjegyz&eacute;k" "L&aacute;bjegyzet") 
           ("is" "H&ouml;fundur" "Dagsetning" "Efnisyfirlit" "Aftanm&aacute;lsgreinar") 
           ("it" "Autore" "Data" "Indice" "Note a pi&egrave; di pagina") 
           ("nl" "Auteur" "Datum" "Inhoudsopgave" "Voetnoten") 
           ("no" "Forfatter" "Dato" "Innhold" "Fotnoter") 
           ("nb" "Forfatter" "Dato" "Innhold" "Fotnoter") 
           ("nn" "Forfattar" "Dato" "Innhald" "Fotnotar") 
           ("pl" "Autor" "Data" "Spis tre&sacute;ci" "Przypis") 
           ("sv" "F&ouml;rfattare" "Datum" "Inneh&aring;ll" "Fotnoter") 
           ("zh-CN" "作者" "日期" "目录" "脚注")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;(setq org-export-language-setup       (append org-export-language-setup               '(("zh-CN" "作者" "日期" "目录" "脚注"))))
;;(setq org-export-default-language "zh-CN")

;; 设置org-mode导出html
(setq org-export-htmlize-output-type 'inline-css)
(require 'htmlize)
;;(require 'org-html)
;;
(require 'color-theme)
(setq color-theme-is-global t)
;;(color-theme-robin-hood)


(add-hook 'message-mode-hook 'turn-on-orgstruct)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; emacs-w3m
(require 'w3m-load)
(setq w3m-home-page "http://lesliezhu.github.com")
;;(setq w3m-key-binding 'info)
;;(define-key w3m-mode-map [up] 'previous-line)
;;(define-key w3m-mode-map [down] 'next-line)
;;(define-key w3m-mode-map [left] 'backward-char)
;;(define-key w3m-mode-map [right] 'forward-char)


;; for python code
(defun py-outline-level ()
  "This is so that `current-column` DTRT in otherwise-hidden text"
  ;; from ada-mode.el
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))

;; this fragment originally came from the web somewhere, but the outline-regexp
;; was horribly broken and is broken in all instances of this code floating
;; around. Finally fixed by Charl P. Botha <<a href="http://cpbotha.net/">http://cpbotha.net/</a>>

(defun my-python-hook ()
  (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(def[ \t]+\\|class[ \t]+\\)")
  ;; enable our level computation
  (setq outline-level 'py-outline-level)
  ;; do not use their \C-c@ prefix, too hard to type. Note this overides
  ;;some python mode bindings
  (setq outline-minor-mode-prefix "\C-c")
  ;; turn on outline mode
  (outline-minor-mode t)
  ;; initially hide all but the headers
  (hide-body)
  (show-paren-mode 1))

;; C-c C-c hide code
;; C-c C-s show code
;; 设置代码折叠
(add-hook 'python-mode-hook 'my-python-hook)





;; for python code
;;(defun my-python-mode()
;;  (define-key python-mode-map [return] 'newline-and-indent)
;; 这种定义的方式与上一句那种不同的是当在注释的模式下按回车新的一行是对齐的注释
;;  (define-key python-mode-map [return] 'comment-indent-new-line)
;;  (define-key python-mode-map "\C-cc" 'comment-or-uncomment-region);
;;  (define-key mslk-c++-key "\C-o" 'hs-hide-all)
;;  (define-key mslk-c++-key "\C-s" 'hs-show-all)
;;  (define-key mslk-c++-key "\C-ch" 'hs-hide-block)
;;  (define-key mslk-c++-key "\C-cs" 'hs-show-block)
;;  (define-key mslk-c++-key "\C-l" 'hs-hide-level)
;;  (define-key mslk-c++-key "\C-m" 'hs-toggle-hiding)
;;  (interactive)
;;  (imenu-add-menubar-index) ;; 在菜单条里加入函数列表菜单
;;  (hs-minor-mode) ;; 打开可以折叠的模式
;;  (custom-set-variables
;;   '(python-honour-comment-indentation t)
    ;; 括号成对指示
;;   '(show-paren-mode t)))

;;(add-hook 'hs-minor-mode-hook 'my-hs-minor-mode-map-setup t)



;;(require 'pycomplete)


(add-to-list 'load-path	  "~/.emacs.d/plugins/yasnippet/")
(require 'yasnippet)
(yas-global-mode 1)
