# Fedora 15环境下安装Common Lisp(转载)

需要用到的软件：

* sbcl，比较流行的一种common lisp实现；
* emacs，用作开发环境，虽然比较复杂，但是比原来的命令行好用多了；
* slime，emas下的一个交互式lisp开发环境；
* quicklisp，common lisp包管理系统。




## 先用yum安装emacs和sbcl。

##  然后按如下步骤安装quicklisp。



* 下载quicklisp安装脚本：

```bash
wget http://beta.quicklisp.org/quicklisp.lisp
```



* 启动sbcl：

```bash
sbcl --load quicklisp.lisp
```



* 安装quicklisp：

```lisp
(quicklisp-quickstart:install)
```



* 添加quicklisp到启动文件，以后启动sbcl会自动加载quicklisp

```lisp
(ql:add-to-init-file)
```



## 用quicklisp安装slime

```lisp
(ql:quickload "quicklisp-slime-helper")
```

安装脚本执行完之后会给出一段elisp代码，按照说明把代码加入.emacs文件。

代码示例：

```lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
;;改为==>
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
("quicklisp-slime-helper")
```



## 启动emacs，M-x slime启动slime，进入交互式cl编程环境，大功告成！



如果还嫌麻烦，直接用Lispbox。



原文：http://life-sucks.net/wp/2011/10/25/fedora15%E4%B8%8B%E5%AE%89%E8%A3%85common-lisp%E5%BC%80%E5%8F%91%E7%8E%AF%E5%A2%83/
