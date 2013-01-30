# CPython解释器简介

python - an interpreted, interactive, object-oriented programming language

## CPython

CPython是用C语言实现的Python解释器，也是官方的并且是最广泛使用的Python解释器。除了CPython以外，还有用JAVA实现的Jython和用.NET实现的IronPython，使Python方便地和JAVA程序、.NET程序集成。另外还有一些实验性的Python解释器比如PyPy。

CPython是使用字节码的解释器，任何程序源代码在执行之前先要编译成字节码。它还有和几种其它语言（包括C语言）交互的外部函数接口。

## PyPy

这个只知道是用Python实现Python语言解释器，属于元编程吧，再说。



## CPython参数

使用很久，却没有认真看过这些文档，也许正是忽略了这些，才会觉得用的不得劲吧。

```shell
$python --help

用法: python [option] ... [-c cmd | -m mod | file | -] [arg] ...

-B     : 不生成*.py[co]文件，也可设置PYTHONDONTWRITEBYTECODE=x
-c cmd : 以字符串形式执行命令，可以”；”分割多个命令
-d     : 激活调试日志，也可设置PYTHONDEBUG=x
-E     : 忽略系统PYTHON*变量
-h     : 打印说明信息后退出
-i     : 运行脚本后，进入解释器（即时标准输出不是终端），也可设置PYTHONINSPECT=x
-m mod : 以脚本形式运行模块
-O     : 优化字节码，也可设置PYTHONOPTIMIZE=x
-OO    : 在-O优化基础上，去除文档字符串信息
-Q arg : 除法选项: -Qold (default), -Qwarn, -Qwarnall, -Qnew
-s     : 不把用户模块加入sys.path中，也可设置PYTHONNOUSERSITE
-S     : 启动时无法import sys.path中的模块
-t     : 检查TAB不一致情况
-u     : 不缓存stdin、stdout，也可设置PYTHONUNBUFFERED=x
-v     : 输出冗长的加载过程信息，也可设置PYTHONVERBOSE=x
-V     : 打印版本信息后退出
-W arg : 警告控制，参数是：action:message:category:module:lineno，也可设置PYTHONWARNINGS=arg
-x     : 忽略第一行，可以不使用Unix风格的#!cmd
-3     : 对Python3不再兼容的警告
file   : 运行脚本文件
-      : 运行标准输入内容
arg ...: 环境变量在sys.argv[1:]


其它环境变量：

PYTHONSTARTUP: 解释器启动时执行的文件
PYTHONPATH   : 用“：”分割的模块前缀路径，结果加入sys.path
PYTHONHOME   : 解释器路径，默认格式是<prefix>/pythonX.X.
PYTHONCASEOK : import导入模块时不区分大小写（Linux上无效）

PYTHONIOENCODING:

stdin/stdout/stderr信息编码，可设置utf_8，gbk，从python3开始缺省是utf-8，python2缺省是ascii。
sys.getdefaultencoding()和sys.setdefaultcoding('xxxx')。
```

### -B 

加载模块会编译字节码，``-B``选项禁止产生字节码文件``*.pyc``,``*.pyo``;
也可以通过设置变量``export PYTHONDONTWRITEBYTECODE=x``来达到同样效果.

### -c cmd 

以运行字符串命令方式解释python语句。

比如将shell和python混合在一起：

```shell
$ python -c "import math;print math.pi"
3.14159265359
```

```shell
$ echo "import math;print math.pi" | python
3.14159265359
```

```shell
$ input="LeslieZhu";echo "import md5; print md5.new(\"$input\").hexdigest()"|python
bd3f46bb49a6d658604b9ce7c155c631
```

```shell
#!/usr/bin/env bash

python <<HI
import math
print math.pi
HI

```

### -d 

打开调试开关

### -E 


忽略系统中PYTHON*相关变量。

### -i 

运行脚本后，进入交互模式。主要用途是运行脚本报错后，立即转入交互模式，可以保留脚本的变量，并且屏蔽了解释器默认加载的模块，从而更好的debug。


### -m mod arg

将一个模块当作脚本运行，用以测试某个模块，方便就是不需要知道模块的路径，直接搜寻并运行，比如：``python -m SimpleHTTPServer``，还可以给模块传参数。

### -O   

开启优化，生成字节码。


### -OO 

在 -O 优化基础上，去除文档信息，减小字节码文件大小

### -Q arg 

对于除法，或许是Python在科学计算的发展，导致有多个版本；如果程序涉及大量数值计算，对取位很敏感的话，建议用-Qwarnall


```shell
$ python -Qold test.py

5/2   =  2
5/2.  =  2.5
5//2  =  2
5//2. =  2

$ python -Qwarn test.py

test.py:6: DeprecationWarning: classic int division
  print "5/2   = ",5/2
5/2   =  2
5/2.  =  2.5
5//2  =  2
5//2. =  2


$ python -Qwarnall test.py

test.py:6: DeprecationWarning: classic int division
  print "5/2   = ",5/2
5/2   =  2
test.py:7: DeprecationWarning: classic float division
  print "5/2.  = ",5/2.
5/2.  =  2.5
5//2  =  2
5//2. =  2


$ python -Qnew test.py

5/2   =  2.5
5/2.  =  2.5
5//2  =  2
5//2. =  2
```

### -s    


不加载用户自己的模块资源路径到变量sys.path中。

```python
>>> for i in sys.path:
...     print i
... 
/usr/bin
/usr/lib/python2.7/site-packages/numpy-1.6.2-py2.7-linux-i686.egg
/usr/lib/python2.7/site-packages/matplotlib-1.1.0-py2.7-linux-i686.egg
/usr/lib/python2.7/site-packages/openpyxl-1.5.8-py2.7.egg
/usr/lib/python2.7/site-packages/pyPdf-1.13-py2.7.egg
/usr/lib/python2.7/site-packages/reportlab-2.5-py2.7-linux-i686.egg
/usr/lib/python2.7/site-packages/rst2pdf-0.92-py2.7.egg
/usr/lib/python2.7/site-packages/docutils-0.9.1-py2.7.egg
/usr/lib/python2.7/site-packages/markdown2-2.1.0-py2.7.egg
/usr/lib/python2.7/site-packages/Sphinx-1.1.3-py2.7.egg
/usr/lib/python2.7/site-packages/bottle-0.11.4-py2.7.egg
/usr/lib/python27.zip
/usr/lib/python2.7
/usr/lib/python2.7/plat-linux2
/usr/lib/python2.7/lib-tk
/usr/lib/python2.7/lib-old
/usr/lib/python2.7/lib-dynload
/usr/lib/python2.7/site-packages
/usr/lib/python2.7/site-packages/PIL
/usr/lib/python2.7/site-packages/gst-0.10
/usr/lib/python2.7/site-packages/gtk-2.0
/usr/lib/python2.7/site-packages/setuptools-0.6c11-py2.7.egg-info
```

## -B : don't write .py[co] files on import; also PYTHONDONTWRITEBYTECODE=x

加载模块会编译字节码，``-B``选项禁止产生字节码文件``*.pyc``,``*.pyo``;
也可以通过设置变量``export PYTHONDONTWRITEBYTECODE=x``来达到同样效果.

=======
```python
>>> import site
>>> site.getusersitepackages()
'/home/user/.local/lib/python2.7/site-packages'
```

那么如果没有-S，文件/home/user/.local/lib/python2.7/site-packages/usercustomize.py会影响Python的每一次调用。


### -S   

启动时候，不运行类似``import site``的语句,注意，这里的site指的是sys.path变量里面的模块，从而不依赖第三方模块运行脚本，可以测试程序依赖哪些模块。比如reportlab：

```python

import reportlab

if __name__ == "__main__":
    print reportlab.Version
```

```shell
$ python test.py

2.5

$ python -S test.py
Traceback (most recent call last):
  File "test.py", line 4, in <module>
    import reportlab
ImportError: No module named reportlab
```

### -t    

检查TAB代表空格个数是否一致，防止TAB既是4个空格，又是8个空格，影响移植性，建议用4个空格。


### -u    

不缓存stdout和stderr


### -v   

输出冗长的python加载、处理的过程细节，如果你用过strace，就知道是用来查看程序加载、调用了哪些资源、过程、细节，还包括资源回收。

### -W arg 

警告控制：action:message:category:module:lineno

### -x

忽略第一行，比如第一行写的是python2.5，但现在用python2.7运行，就用-x跳过第一行，从而测试2.5版本程序是否在2.7下可用。

### -3

对Python3以后版本不再支持的特性进行警告，因为Python3对Python2一些特性不再兼容。




### PYTHONSTARTUP

类似shell的文件.prefile，解释器启动时都会调用此文件，但只是交互式会话才会调用。

```python
import os
filename = os.environ.get('PYTHONSTARTUP')
if filename and os.path.isfile(filename):
    exec(open(filename).read())
```

可以在这个文件中改变sys.ps1,sys.ps2的值。

### PYTHONIOENCODING:

从python3开始缺省是utf-8，python2缺省是ascii。sys.getdefaultencoding()和sys.setdefaultcoding('xxxx')。





## Python Shell

* IPython
* bpython


## IDE

* IDLE
* NINJAIDE



## Python应用

### 系统管理

Python的系统管理应用，就是拿Python当shell用，功能会更强大些。常用的模块是os,system,shutil,glob,tempfile,pwd,grp,commands,subprocess.

### WEB应用

* Django
* Douban
* Yupoo

### 科学计算

* scipy
* pylab
* numpy
* matplotlib

### 游戏开发

* pygame
>>>>>>> b9b2162cc0bb429f9d9b04afc7c255e6aac9493d
