# ASDF打包Common Lisp程序——学习笔记

前面有篇日志是看《实践Common Lisp编程》第三章“简单数据库”的笔记，根据书中的例子写出了一个CL脚本musicd.lisp：

```lisp
#!/usr/bin/sbcl --script
;; zhubuntu <pythonisland@gmail.com>
;;
;; program: musicdb.lisp
;; author : zhubuntu
;; email  : pythonisland@gmail.com
;;
;; version: 1.0
;; date   : 2012-04-15
;;
;; info   : run a common lisp script with sbcl
;;          a music cd database for test
;;
;; usage  :
;;          (load "musicdb.lisp")
;;          (in-package musicdb)
;;          (help)
;;          (help "add-cds")

;; 自定义包
(defpackage :musicdb
  (:use :common-lisp
        :common-lisp-user)
  (:export :add-cds
           :update
           :save-db
           :load-db
           :select
           :delete-rows
           :help
           :add-record))

;; 加载库common-lisp-user,这个包用到common-lisp
;; 查看包用*package*，或者common-lisp:*package*,或者cl:*package*
;;(in-package cl-user)
(in-package musicdb)

;; 全局变量*db*存储数据库
(defvar *db* nil)

;; 创建一条音乐记录
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; 添加音乐记录到数据库
(defun add-record (cd)
  (push cd *db*))

;; 从标准输入添加音乐记录
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)    
  (force-output *query-io*)        ;*query-io*绑定到标准输入
  (read-line *query-io*))       ;从标准输入读取一行

;; 连续添加多条音乐记录
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

;;添加音乐记录
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another? [y/n]: "))
        (return))))

;; 以易读形式打印数据库
;; t，绑定标准输出*standard-output*
;; ~a，以易读形式打印
;; ~{，列表的开始
;; ~t, 制表符
;; ~%, 换行
;; ~}, 列表的结束
(defun dump-db (&optional database)
  (if database
      ()
      (setf database *db*))
  (dolist (cd database)
    (format t "~{~a: ~10t~a~%~}~%" cd))) 

;; 保存数据库到文件
;; print保存为lisp可读懂格式
;; format打印为人类可读懂形式
(defun save-db (filename)
  (with-open-file (out filename
               :direction :output
               :if-exists :supersede)
          (with-standard-io-syntax
           (print *db* out))))  

;; 加载数据库文件
(defun load-db (filename)
  (with-open-file (in filename)
          (with-standard-io-syntax
           (setf *db* (read in)))))

;; 查询
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
;;  
;; 以artist关键字查询
;;(defun artist-selector (artist)
;;  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; 以title关键字查询
;;(defun title-selector (title)
;;  #'(lambda (cd) (equal (getf cd :title) title)))

;; 以rating关键字查询
;;(defun rating-selector (rating)
;;  #'(lambda (cd) (equal (getf cd :rating) rating)))

;; 以ripped关键字查询
;;(defun ripped-selector (ripped)
;;   #'(lambda (cd) (equal (getf cd :ripped) ripped)))

;; 自动匹配关键字查询，替换4个selector函数
;;(defun where (&key title artist rating (ripped nil ripped-p))
;;  #'(lambda (cd)
;;      (and
;;       (if title  (equal (getf cd :title) title) t)
;;       (if artist (equal (getf cd :artist) artist) t)
;;       (if rating (equal (getf cd :rating) rating) t)
;;       (if ripped (equal (getf cd :ripped) ripped) t))))

;; 更新数据库
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
    (mapcar
     #'(lambda (row)
         (when (funcall selector-fn row)
           (if title  (setf (getf row :title) title))
           (if artist (setf (getf row :artist) artist))
           (if rating (setf (getf row :rating) rating))
           (if ripped-p (setf (getf row :ripped) ripped)))
           row)
         *db*)))

;; 删除数据库记录
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
  
;; 生成表达式
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
  
;; 生成表达式列表
(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))
       
;; 定义宏where代替函数where，更抽象、易维护
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
  
;; 函数调用举例
(defun help (&optional cmd)
  (if (equal cmd "update")
      (format t "~a:~%~10t~a~%" "example" "(update (where :artist \"zhubuntu\") :artist \"leslie\")"))
  (if (equal cmd "delete-rows")
      (format t "~a:~%~10t~a~%" "example" "(delete-rows (where :artist \"zhubuntu\")"))
  (if (equal cmd "select")
      (format t "~a:~%~10t~a~%" "example" "(select (where :artist \"zhubuntu\")"))
  (if (equal cmd "add-cds")
      (format t "~a:~%~10t~a~%" "example" "(add-cds)"))
  (if (equal cmd "dump-db")
      (format t "~a:~%~10t~a~%" "example" "(dump-db)"))
  (if (equal cmd "save-db")
      (format t "~a:~%~10t~a~%" "example" "(save-db \"filename.db\")"))
  (if (equal cmd "load-db")
      (format t "~a:~%~10t~a~%" "example" "(load-db \"filename.db\")"))
  (if (equal cmd "add-record")
      (format t "~a:~%~10t~a~%" "example" "(add-record (make-cd \"Love Min\" \"zhubuntu\" 8 t)"))
  (if (not cmd)
      (format t "~a:~%~10t~a~%" "Usage" "commands: add-record,update,delete-rows,select,add-cds,dump-db,
      save-db,load-db")))
```

这个脚本文件定义了包musicdb，使用方法是：

```lisp
;; usage  :
;;          (load "musicdb.lisp")
;;          (in-package musicdb)
;;          (help)
;;          (help "add-cds") 
;;
```

加载这个脚本需要知道脚本的路径，很不方便，而且在文件比较多、存在各种依赖关系的时候，需要采用ASDF机制。一般的解释器都支持ASDF2，比如SBCL默认安装有asdf。

加载asdf的方法：

```lisp
(require "asdf") ;;或者
(load "/path/to/asdf.lisp") 
```


检查是否加载asdf的方法：

```lisp
(asdf:asdf-version) 
```

返回字符串则是版本号，返回错误信息说明没有安装asdf或者asdf版本太旧。

确认asdf版本是否最新的方法：

```lisp
(or #+asdf2 (asdf:asdf-version) #+asdf :old) 
```
如果返回字符串说明是最新版本的版本号，如果放那会:OLD说明是过时版本，如果返回NIL说明没有安装asdf。

升级asdf的方法（有一种旧风格的升级方法，这里不提）：

```lisp
(require "asdf")
(asdf:load-system :asdf) 
```


配置asdf的基本思路是告诉asdf到那个目录去搜寻common lisp软件，一般默认用户安装Common Lisp软件的目录是:

```bash
~/.local/share/common-lisp/source/
```

如果要添加一个其它目录的话，需要创建目录:

```bash
~/.config/common-lisp/source-registry.conf.d/
```

然后在此目录添加配置文件，文件名可以任意，这里我创建文件22-projects-example.conf:

```lisp
(:tree "/home/zhuchunlite/Leslie-Chu/Lisp/projects-example/") 
```

这样是把这个目录和子目录都作为common lisp软件的搜寻目录，而我的lisp方面的软件都打算放到这个目录下，这样以后我就不用再配置这个文件了。


如果不想把子目录也作为搜寻目录，毕竟有时候搜寻大量的目录需要耗费资源，可以把单个目录作为搜寻目录：

```lisp
(:directory "/home/zhuchunlite/Leslie-Chu/Lisp/projects-example/")
```

这个配置文件文件名是22-projects-example.conf，习惯把配置文件名以两个数据开头，然后以目录名结尾。

配置好后，可执行以下来清除并重新读取配置文件：

```lisp
(asdf:clear-source-registry) 
```

通过asdf加载软件的方法：

```text
* (asdf:load-system "musicdb")

; Loading system definition from
; /home/zhuchunlite/Leslie-Chu/Lisp/projects-example/musicdb/musicdb.asd into
; #<PACKAGE "ASDF0">
; Registering #<SYSTEM "musicdb">
T 
```


这里的musicdb.asd是为这个“软件”musicdb.lisp定义的.asd文件，文件musicdb.asd内容为：

```lisp
(defpackage :musicdb-system
  (:use :cl :asdf))
;
(in-package :musicdb-system)
;
(defsystem "musicdb"
    :description "musicdb: a sample Lisp system."
    :version "0.2.1"
    :author "Leslie Chu <pythonisland@gmail.com>"
    :licence "Public Domain"
    :components ((:file "musicdb")
                 (:file "packages" :depends-on ("musicdb"))))   
```

这里先定义包musicdb-system是为了避免包之间彼此冲突，通过defsystem来定义musicdb（我一直不知道这个system该怎么翻译，我翻译成“装置”）。

定义装置musicdb中，需要注意的是packages.lisp依赖于文件musicdb.lisp，而package.lisp文件内容是：

```lisp
(in-package musicdb)
```

这样在执行语句(asdf:load-system "musicdb")的时候，asdf会根据配置文件找到文件musicdb.asd，然后根据这个文件的定义去加载、编译各个.lisp文件。

为了确认这些.lisp的加载、编译顺序，可以加入打印信息：

```lisp
(format t "~a~%" "in file musicdb.lisp")  ;;---->musicdb.lisp
(format t "~a~%" "in file packages.lisp") ;;---->packages.lisp
```

这是加载、编译时打印信息，显示是先加载musicdb.lisp，后加载packages.lisp：

```text
* (asdf:load-system "musicdb")

; Loading system definition from
; /home/zhuchunlite/Leslie-Chu/Lisp/projects-example/musicdb/musicdb.asd into
; #<PACKAGE "ASDF0">
; Registering #<SYSTEM "musicdb">

; compiling file "/home/zhuchunlite/Leslie-Chu/Lisp/projects-example/musicdb/musicdb.lisp" (written 22 APR 2012 07:35:34 PM):
; compiling (FORMAT T ...)
; compiling (DEFPACKAGE :MUSICDB ...)
; compiling (IN-PACKAGE MUSICDB)
; compiling (DEFVAR *DB* ...)
; compiling (DEFUN MAKE-CD ...)
; compiling (DEFUN ADD-RECORD ...)
; compiling (DEFUN PROMPT-READ ...)
; compiling (DEFUN PROMPT-FOR-CD ...)
; compiling (DEFUN ADD-CDS ...)
; compiling (DEFUN DUMP-DB ...)
; compiling (DEFUN SAVE-DB ...)
; compiling (DEFUN LOAD-DB ...)
; compiling (DEFUN SELECT ...)
; compiling (DEFUN UPDATE ...)
; compiling (DEFUN DELETE-ROWS ...)
; compiling (DEFUN MAKE-COMPARISON-EXPR ...)
; compiling (DEFUN MAKE-COMPARISONS-LIST ...)
; compiling (DEFMACRO WHERE ...)
; compiling (DEFUN HELP ...)

; /home/zhuchunlite/.cache/common-lisp/sbcl-1.0.43-1.fc15-linux-x86/home/zhuchunlite/Leslie-Chu/Lisp/projects-example/musicdb/ASDF-T
MP-musicdb.fasl written
; compilation finished in 0:00:00.077
in file musicdb.lisp
; compiling file "/home/zhuchunlite/Leslie-Chu/Lisp/projects-example/musicdb/packages.lisp" (written 22 APR 2012 07:35:57 PM):
; compiling (FORMAT T ...)
; compiling (IN-PACKAGE MUSICDB)

; /home/zhuchunlite/.cache/common-lisp/sbcl-1.0.43-1.fc15-linux-x86/home/zhuchunlite/Leslie-Chu/Lisp/projects-example/musicdb/
ASDF-TMP-packages.fasl written
; compilation finished in 0:00:00.002
in file packages.lisp
T
*  

```

因为最后加载、编译的是文件packages.lisp，因此最后*package*变量绑定到了MUSICDB（我本来这样认为的）:

```text
* *package*

#<PACKAGE "COMMON-LISP-USER">
```

原来加载文件packages.lisp虽然加载包musicdb，但是文件加载完毕后脚本文件执行就结束了，所以定义包musicdb后，又回到默认顶层包CL中。

通过(in-package musicdb)来加载包musicdb后，可以调用包musicdb里面定义的函数（注意，加载包时不需要加双引号）：

```text
* (help)
Usage:
          commands: add-record,update,delete-rows,select,add-cds,dump-db,save-db,load-db
NIL
* (help "add-cds")
example:
          (add-cds)
NIL
* (load-db "cd.db")

((:TITLE "leslie" :ARTIST "zhubuntu" :RATING 4 :RIPPED T)
 (:TITLE "love" :ARTIST "min" :RATING 2 :RIPPED T))
* (dump-db)
TITLE:  leslie
ARTIST:   zhubuntu
RATING:   4
RIPPED:   T

TITLE:    love
ARTIST:   min
RATING:   2
RIPPED:   T

NIL 
```


我对asdf机制的学习笔记到这里就写这么多，更多的详细信息请看ASDF手册，我后续会选择性翻译部分内容，不求面面俱到，但求实用。

* 建议用Quicklisp安装软件；
* 下载quicklisp：wget http://beta.quicklisp.org/quicklisp.lisp ；
* sbcl加载：(load "quicklisp.lisp")；
* 安装：(quicklisp-quickstart:install)；
* 添加到启动脚本：(ql:add-to-init-file)；
* 安装图形开发库ltk：(ql:quickload "ltk")
