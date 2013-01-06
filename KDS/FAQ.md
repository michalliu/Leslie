FAQ
======

MOA团队常见问题集锦。

POD1
------

* obi22是POD的SysG，包括node：obi23 ~  obi40
* x22  是POX的SysG，包括node：  x23 ~  x40
* obi22和x22互为主、备关系

POD2
------

* w109是DOD的SysG，  包括node：w1 ～ w108
* y71 是Y-Node的SysG，包括node：y72 ~ y107
* w109和y71互为主、备关系


修改文件
==========

* 不可以直接修改文件，保存为临时文件
* 修改临时文件
* vimdiff比较临时文件、正式文件，确认修改正确
* 备份正式文件，命名加入日期
* rollout，用临时文件替换，refresh


临时表命名
===========

* 不要用201208_temp命名
* 采用temp_201208，可用性更强


导入表
=======

* 要考虑是否需要将所有的数据都导入数据库，有时仅需要将部分数据导入
* 如果可以，尽量只导入本次工作需要的数据
* 如果脚本中需要对数据进行特殊处理，或者已有表对字段是处理后的，可以对源数据进行预处理，从而节省效率
* 没有进行QA确认之前，不要覆盖任何表，表以_temp_${dt}命名临时表


修改脚本
=========

* 在自己所有的目录创建临时脚本，命令以_temp.tks命名
* 临时表使用临时layout
* 如果脚本运行时间超过10分钟，立即Start，除非确认时间会很长（很长应该优化），否则不要让命令占有太久资源
* 系统一次只能有一个查询运行
* 脚本开头要注释脚本用法、修改历史、简单描述、必要的注释


修改ts.ini
==========

* layout决定数据在数据库中的存在形式，y_value决定数据在外部接口（web、报告）的显示结果
* by_variable是用来决定其它字段的置信区间
* 能有注释尽量注释，注释的要求是别人能看懂为何要修改
* 修改后，一定要make refresh

Makefile
==========

* 如果有什麽不知如何下手，首先查看Makefile，make all就是解
* Makefile是入口，也是参考，要确认目前是什麽状态
* 系统是时间积累的，文件关系复杂，逻辑混乱，有些处于无人维护状态

后台运行cohort
==============

* 相对前台的cohort，正常cohort，前面要加上“trimed_memo,15,1,timeSeries,”字符
* 运行cohort获取request-id:

``RunUBT -nb XXX.ubt``

``scripts/TST.sh XXX.ubt``

``./UBClient -u username -p password -t 'cohort'``

* 获取结果，结果以xml格式保存在~/bin/Requests/all/requst-id.ub7：

``rsp -S request-id``

``rsp request-id 0，获取cohort具体在系统中信息``

``rsp request-id 1，获取输出page num 1结果``


ubc
=====

* -b，以批处理作业形式运行kscript脚本
* -N  显示查询所有者名字
* -Q id，获取id对应的kscripts脚本内容
* -n 查看发送的字符串，dry run利器
* -T <second>  超时时间
* -q <(m|my|a|all|username)>[,number[,skipcnt]]查询，其中m查询状态，a查询所有，username查询用户所有，number查询结果最后number条记录

州的缩写
========

* 表zip_msa中存储了州缩写的数据
* 源数据中有州全称的，一般在脚本中处理时是要map成缩写，因此可以对源数据进行预处理



用户对应网站
===========

* 不同网站对应不同用户，使用不同用户的ts.ini，不同的ts.ini提供不同的服务
* 网站测试cohort后，要确定网站是使用哪个用户的ts.ini配置文件
* 命令ll /home/*/bin/Requests/id*，则找到request-id属于哪个用户，则使用哪个用户的ts.ini
* 同时知道网站对应哪个用户
* 因为网站对应用户常改变，不可盲目凭经验判断

DOD备份
=======

* 更新表后，运行命令``cd bin;make sync-failover year=2012``
* 查看日志``ssh kdsd03@y71;cd DOD;tail -f ND/1/kdsd03/UBBackup2.log``
