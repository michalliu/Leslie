没有ssh-key
===========

我一般更新github上的项目，采用下列过程：
* mkdir project-dir
* cd project-dir
* git init
* git remote add origin https://github.com/LeslieZhu/Leslie.git
* git pull origin master
* ....
* git commit -m "reason"
* git push -u origin master （需要输入密码）

在推送更新到github的时候常常因为输入密码而懊恼，今天按照[说明信息](https://help.github.com/articles/generating-ssh-keys)配置了ssh-key，以后就不需要密码了。

生成公钥
==========

* cd ~/.ssh
* ssh-keygen -t rsa -C "your_email@youremail.com"
* xclip -sel clip < ~/.ssh/id_rsa.pub
* ssh -T git@github.com （测试）
* 将id_rsa.pub复制到www.github.com的SSH-KEY

用ssh推送
==========

* git remote add origin git@github.com:LeslieZhu/Leslie.git
* git push -u origin master（不需要输入密码）


xclip
=======

