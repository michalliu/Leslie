;;; my-org-settings.el --- My Org-Mode Settings

;; Copyright (C) 2011  Boyun Tang

;; Author: Boyun Tang
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see .

;;; Commentary:

;;

;;; Code:
(require 'org-install)
;;(require 'ob-ditaa)
;;(require 'google-weather)
;;(require 'org-google-weather)
(require 'org-latex)
;;-----------------------------------------------------------------------------
;; Folding Mode 相关
(load "folding" 'nomessage 'noerror)
;;(folding-mode-add-find-file-hook)
;;(folding-add-to-marks-list 'org-mode "# {{{" "# }}}" nil t)
(add-hook 'org-mode-hook 'folding-mode)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
(setq org-export-latex-listings t)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Options for \lset command（reference to listing Manual)
(setq org-export-latex-listings-options
'(
("escapeinside" "{(*@}{@*)}")
("basicstyle" "\\color{foreground}\\small\\mono")           ; 源代码字体样式
("keywordstyle" "\\color{function}\\bfseries\\small\\mono") ; 关键词字体样式
("identifierstyle" "\\color{doc}\\small\\mono")
("commentstyle" "\\color{comment}\\small\\itshape")         ; 批注样式
("stringstyle" "\\color{string}\\small")                    ; 字符串样式
("showstringspaces" "false")                                ; 字符串空格显示
("numbers" "left")                                          ; 行号显示
("numberstyle" "\\color{preprocess}")                       ; 行号样式
("stepnumber" "1")                                          ; 行号递增
("backgroundcolor" "\\color{background}")                   ; 代码框背景色
("tabsize" "4")                                             ; TAB等效空格数
("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
("breaklines" "true")                                       ; 自动断行
("breakatwhitespace" "true")                                ; 只在空格分行
("showspaces" "false")                                      ; 显示空格
("columns" "flexible")                                      ; 列样式
("frame" "single")                                          ; 代码框：阴影盒
("frameround" "tttt")                                       ; 代码框： 圆角
("framesep" "0pt")
("framerule" "8pt")
("rulecolor" "\\color{background}")
("fillcolor" "\\color{white}")
("rulesepcolor" "\\color{comdil}")
("framexleftmargin" "10mm")
))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; 使用xelatex一步生成PDF
(setq org-latex-to-pdf-process
'("xelatex -interaction nonstopmode %f"
"xelatex -interaction nonstopmode %f"))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; 默认主模式为org-mode
(setq default-major-mode 'org-mode)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Make Org use ido-completing-read for most of its completing prompts.
(setq org-completion-use-ido t)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; code执行免应答（Eval code without confirm）
(setq org-confirm-babel-evaluate nil)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; diaa path 考虑换成DitaaEps中
;;;(setq org-ditaa-jar-path "~/.emacs.d/ditaa0_9.jar")
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; 各种Babel语言支持
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (matlab . t)
   (C . t)
   (perl . t)
   (sh . t)
   (ditaa . t)
   (python . t)
   (haskell . t)
   (dot . t)
   (latex . t)
   (js . t)
   ))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; REFTEX
(defun org-mode-article-modes ()
(reftex-mode t)
(and (buffer-file-name)
(file-exists-p (buffer-file-name))
(reftex-parse-all)))
(add-hook 'org-mode-hook
(lambda ()
(if (member "REFTEX" org-todo-keywords-1)
(org-mode-article-modes))))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Latex Export
(unless (boundp 'org-export-latex-classes)
(setq org-export-latex-classes nil))

;; 'my-org-article' for export org documents to the LaTex 'article', using
;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
(add-to-list 'org-export-latex-classes
'("cn-article"
"\\documentclass[10pt,a4paper]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{lmodern}
\\usepackage{verbatim}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{geometry}
\\usepackage{algorithm}
\\usepackage{algorithmic}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{fancyhdr}
\\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
linkcolor=blue,
urlcolor=blue,
menucolor=blue]{hyperref}
\\usepackage{fontspec,xunicode,xltxtra}
\\setmainfont{Times New Roman}%英文字体
\\newcommand\\fontnamemono{DejaVu Sans YuanTi Mono}%等宽字体
\\newfontinstance\\MONO{\\fontnamemono}
\\newcommand{\\mono}[1]{{\\MONO #1}}
\\setCJKmainfont[Scale=0.9]{DejaVu Sans YuanTi Condensed}%中文字体
\\setCJKmonofont[Scale=0.9]{DejaVu Sans YuanTi Mono}
\\setCJKfamilyfont{mono}{YaHei Consolas Hybrid}
\\hypersetup{unicode=true}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
marginparsep=7pt, marginparwidth=.6in}
\\definecolor{foreground}{RGB}{220,220,204}%浅灰
\\definecolor{background}{RGB}{62,62,62}%浅黑
\\definecolor{preprocess}{RGB}{250,187,249}%浅紫
\\definecolor{var}{RGB}{239,224,174}%浅肉色
\\definecolor{string}{RGB}{154,150,230}%浅紫色
\\definecolor{type}{RGB}{225,225,116}%浅黄
\\definecolor{function}{RGB}{140,206,211}%浅天蓝
\\definecolor{keyword}{RGB}{239,224,174}%浅肉色
\\definecolor{comment}{RGB}{180,98,4}%深褐色
\\definecolor{doc}{RGB}{175,215,175}%浅铅绿
\\definecolor{comdil}{RGB}{111,128,111}%深灰
\\definecolor{constant}{RGB}{220,162,170}%粉红
\\definecolor{buildin}{RGB}{127,159,127}%深铅绿
\\punctstyle{kaiming}
\\title{}
\\fancyfoot[C]{\\bfseries\\thepage}
\\chead{\\MakeUppercase\\sectionmark}
\\pagestyle{fancy}
\\tolerance=1000
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;-----------------------------------------------------------------------------
;; allow for export=>beamer by placing
;; #+LaTeX_CLASS: beamer in org files
;;-----------------------------------------------------------------------------
(add-to-list 'org-export-latex-classes
;; beamer class, for presentations
'("beamer"
"\\documentclass[11pt,professionalfonts]{beamer}

\\mode
\\usetheme{{{{beamertheme}}}}
\\usecolortheme{{{{beamercolortheme}}}}

\\beamertemplateballitem
\\setbeameroption{show notes}
\\usepackage{graphicx}
\\usepackage{tikz}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{amsmath}
\\usepackage{lmodern}
\\usepackage{fontspec,xunicode,xltxtra}
\\usepackage{polyglossia}
\\setmainfont{Times New Roman}
\\setCJKmainfont{DejaVu Sans YuanTi}
\\setCJKmonofont{DejaVu Sans YuanTi Mono}
\\usepackage{verbatim}
\\usepackage{listings}
\\institute{{{{beamerinstitute}}}}
\\subject{{{{beamersubject}}}}"
("\\section{%s}" . "\\section*{%s}")
("\\begin{frame}[fragile]\\frametitle{%s}"
"\\end{frame}"
"\\begin{frame}[fragile]\\frametitle{%s}"
"\\end{frame}")))

;; letter class, for formal letters
;; (add-to-list 'org-export-latex-classes
;;              '("letter"
;;                "\\documentclass[11pt]{letter}\\usepackage{xcolor}"

;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; (add-to-list 'org-export-latex-classes
;;              '("book"
;;                "\\documentclass{book}"
;;                ("\\part{%s}" . "\\part*{%s}")
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
;; (add-to-list 'org-export-latex-classes
;;              '("koma-article"
;;                "\\documentclass{scrartcl}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq ps-paper-type 'a4
ps-font-size 16.0
ps-print-header nil
ps-landscape-mode nil)
;;-----------------------------------------------------------------------------

(provide 'my-org-settings)
;;; my-org-settings.el ends here
