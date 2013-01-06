Pygame程序的建立
============================

  这是对Hello World程序的分析，大致介绍了建立pygame程序的基本流程。

1、导入pygame模块
=======================

``1. import pygame, sys``


2、导入特殊变量到全局名字空间
===========================

``2. from pygame.locals import *``

   之所以这样导入，是因为pygame.locals名字空间里面的变量，即使没有pygame.locals前缀，也很容易识别。

3、初始化
=============

``4. pygame.init()``

调用任何pygame函数前，必须有初始化过程。

4、设置窗口大小
================

``5. DISPLAYSURF = pygame.display.set_mode((400, 300))``

传入参数是元组（400，300），从而决定游戏界面的大小，分别是（宽度，高度）。
返回对象pygame.Surface赋值为DISPLAYSURF。

5、设置界面标题
================

``6. pygame.display.set_caption('Hello World!')``

6、游戏循环和状态
==================
<pre>
7. while True: # main game loop
8.     for event in pygame.event.get():
</pre>

程序主循环是个死循环，除非触发事件而执行sys.exit()，主循环主要做：
* 捕获事件
* 更新游戏状态
* 绘制图像

7、事件
=========

当用户有操作时，程序通过pygame.event.get()捕获事件，返回一个pygame.event.Event对象列表，并赋值给event。

8、退出
=========

<pre>
9. if event.type == QUIT:
10.    pygame.quit()
11.    sys.exit()
</pre>

Event对象属性type表示事件的性质，而pygame.locals定义了一系列变量来划分事件的性质，如QUIT代表“退出”。

9、更新画面
=============
``12. pygame.display.update()``

