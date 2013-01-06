#!/usr/bin/env python
#-*- coding:utf-8 -*-

from PyQt4 import QtGui
from PyQt4 import QtCore,Qt
import sys,os

class SettingMenuBar(object):
   u"显示菜单栏信息的类"
   
   def __init__(self):
      object.__init__(self)      # 基类初始化
   def settingMenuBar(self):
      u"设置菜单栏"
      self.menubar=self.menuBar()      #菜单栏
      #增加菜单
      self.file=self.menubar.addMenu(u"文件(&F)")
      self.edit=self.menubar.addMenu(u"编辑(&E)")
      self.view=self.menubar.addMenu(u"查看(&V)")
      self.goto=self.menubar.addMenu(u"转到(&G)")
      self.sound=self.menubar.addMenu(u"声音(&S)")
      self.help=self.menubar.addMenu(u"帮助(&H)")

      #工具栏
      self.toolbar=self.addToolBar(u"工具栏(&T)")

      # 对菜单项进行设置
      self.setMenuFile()
      self.setMenuEdit()
      self.setMenuView()
      self.setMenuGoto()
      self.setMenuSound()
      self.setMenuHelp()
      
   def setMenuEdit(self):
      u"设置编辑菜单"
      
      self.loop=QtGui.QAction(u"循环模式",self)
      self.loop.setShortcut("Alt+L")
      self.loop.setStatusTip(u"循环模式")
      self.connect(self.loop,QtCore.SIGNAL("triggered()"),self.loop_dialog)
      self.edit.addAction(self.loop)
      
      self.noloop=QtGui.QAction(u"随机模式",self)
      self.noloop.setShortcut("Alt+N")
      self.noloop.setStatusTip(u"随机播放")
      self.connect(self.noloop,QtCore.SIGNAL("triggered()"),self.noloop_dialog)
      self.edit.addAction(self.noloop)
      
      self.nosound=QtGui.QAction(u"静音",self)
      self.nosound.setShortcut("Alt+K")
      self.nosound.setStatusTip(u"静音")
      self.connect(self.nosound,QtCore.SIGNAL("triggered()"),self.nosound_dialog)
      self.edit.addAction(self.nosound)

   def nosound_dialog(self):
      u"静音事件处理函数"
      
      if self.nosound_bool:
         self.process.write("mute 0\n") # 静音
         self.nosound_bool=False
      else:
         self.nosound_bool=True
         self.process.write("mute 1\n")

   def noloop_dialog(self):
      u"随机播放模式"
      self.process.write("set_property loop -1\n")

   def loop_dialog(self):
      u"循环播放模式"
      self.process.write("set_property loop 0\n")
         
   def setMenuView(self):
      u"设置查看菜单"

      # 全屏菜单项
      self.fullscreen=QtGui.QAction(QtGui.QIcon(self.icon_fullscreen),u"全屏",self)
      self.fullscreen.setShortcut("F")
      self.fullscreen.setStatusTip(u"全屏")
      self.connect(self.fullscreen,QtCore.SIGNAL("triggered()"),self.fullscreen_event)
      self.view.addAction(self.fullscreen)
      self.toolbar.addAction(self.fullscreen)

      # 显示控制菜单项
      self.control=QtGui.QAction(u"显示控制",self)
      self.control.setShortcut("Ctrl+J")
      self.control.setStatusTip(u"显示控制")
      self.connect(self.control,QtCore.SIGNAL("triggered()"),self.control_dialog)
      self.view.addAction(self.control)

      # 侧边栏菜单项
      self.right_side=QtGui.QAction(QtGui.QIcon(self.icon_close),u"侧边栏",self)
      self.right_side.setShortcut("E")
      self.right_side.setStatusTip(u"侧边栏")
      self.connect(self.right_side,QtCore.SIGNAL("triggered()"),self.dock_right_true)
      self.view.addAction(self.right_side)
      self.toolbar.addAction(self.right_side)
      
   def control_dialog(self):
      u"显示控制栏事件处理函数"
      
      if self.control_bool is True:
         not self.dock_right.close()
         not self.dock_bottom.close()
         not self.toolbar.close()
         self.control_bool=False
      else:
         self.control_bool = True
         self.dock_bottom.show()

   
   def setMenuGoto(self):
      u"设置转到菜单"

      #快进菜单项
      self.quick_play = QtGui.QAction(QtGui.QIcon(self.icon_quick),u"前进",self)
      self.quick_play.setShortcut(QtGui.QKeySequence(QtCore.Qt.Key_Right))
      self.quick_play.setStatusTip(u"前进")
      self.connect(self.quick_play,QtCore.SIGNAL("triggered()"),self.button_forward_event)

      self.goto.addAction(self.quick_play)

      #倒退菜单项
      self.slow_play = QtGui.QAction(QtGui.QIcon(self.icon_slow),u"倒退",self)
      self.slow_play.setShortcut(QtGui.QKeySequence(QtCore.Qt.Key_Left))
      self.slow_play.setStatusTip(u"倒退")
      self.connect(self.slow_play,QtCore.SIGNAL("triggered()"),self.button_back_event)

      self.goto.addAction(self.slow_play)
      self.toolbar.addAction(self.slow_play)
      self.toolbar.addAction(self.quick_play)

      
   def setMenuSound(self):
      u"设置声音菜单"
      
      #增加声音
      self.add_sound = QtGui.QAction(QtGui.QIcon(self.icon_add_sound),u"增大音量",self)
      self.add_sound.setShortcut(QtGui.QKeySequence(QtCore.Qt.Key_At))
      self.add_sound.setStatusTip(u"增大音量")
      self.connect(self.add_sound,QtCore.SIGNAL("triggered()"),self.add_sound_dialog)
      
      self.sound.addAction(self.add_sound)

      #减小声音
      self.sub_sound = QtGui.QAction(QtGui.QIcon(self.icon_sub_sound),u"减小音量",self)
      self.sub_sound.setShortcut(QtGui.QKeySequence(QtCore.Qt.Key_Slash))
      self.sub_sound.setStatusTip(u"减小音量")
      self.connect(self.sub_sound,QtCore.SIGNAL("triggered()"),self.sub_sound_dialog)

      self.sound.addAction(self.sub_sound)
      self.toolbar.addAction(self.sub_sound)
      self.toolbar.addAction(self.add_sound)

   def sub_sound_dialog(self):
      u"减小声音处理函数"
      
      self.process.write("volume -1\n")
      self.sound_slider.setValue(self.sound_slider.value()-1)
      self.sound_slider.setStatusTip(u"声音大小"+str(self.sound_slider.value()))

   def add_sound_dialog(self):
      u"增加声音处理函数"
      
      self.process.write("volume +1\n")
      self.sound_slider.setValue(self.sound_slider.value()+1)
      self.sound_slider.setStatusTip(u"声音大小"+str(self.sound_slider.value()))

   def setMenuFile(self):
      u"设置文件菜单"
      #打开文件菜单项
      self.open = QtGui.QAction(QtGui.QIcon(self.icon_open),u"打开(&O)",self)
#      self.open.setIcon(QtGui.QIcon(self.icon_open))
      self.open.setShortcut("Ctrl+O")
      self.open.setStatusTip(u"打开文件")
      self.connect(self.open,QtCore.SIGNAL("triggered()"),self.showFile_dialog)
      
      self.file.addAction(self.open)
      self.toolbar.addAction(self.open)
      
      #open location
      self.open_location = QtGui.QAction(QtGui.QIcon(self.icon_open_location),u"打开文件位置(&L)",self)
      self.open_location.setShortcut("Ctrl+G")
      self.open_location.setStatusTip(u"打开文件位置")
      self.connect(self.open_location,QtCore.SIGNAL("triggered()"),self.open_location_dialog)
      self.file.addAction(self.open_location)
      self.toolbar.addAction(self.open_location)

      #退出菜单项
      self.exit = QtGui.QAction(QtGui.QIcon(self.icon_exit),u"退出(&Q)",self)
      self.exit.setShortcut("Ctrl+Q")
      self.exit.setStatusTip(u"退出程序")
      self.connect(self.exit,QtCore.SIGNAL("triggered()"),self,QtCore.SLOT("close()"))

      self.file.addAction(self.exit)
      self.toolbar.addAction(self.exit)

   def setMenuHelp(self):
      u"设置帮组菜单"
      
      #help
      self.helpinfo = QtGui.QAction(QtGui.QIcon(self.icon_ask),u"帮助(&H)",self)
      self.helpinfo.setShortcut("F1")
      self.helpinfo.setStatusTip(u"帮助信息")
      self.connect(self.helpinfo,QtCore.SIGNAL("triggered()"),self.helpinfo_dialog)
      
      self.help.addAction(self.helpinfo)
      self.toolbar.addAction(self.helpinfo)
      
      #version
      self.version = QtGui.QAction(QtGui.QIcon(self.icon_version),u"关于程序(&A)",self)
      self.version.setShortcut("Ctrl+A")
      self.version.setStatusTip(u"本程序信息")
      self.connect(self.version,QtCore.SIGNAL("triggered()"),self.version_dialog)
      
      self.help.addAction(self.version)
      self.toolbar.addAction(self.version)

   def helpinfo_dialog(self):
      u"帮组菜单项事件处理函数"
      
      self.helpinfo_frame=QtGui.QFrame()
      self.helpinfo_edit=QtGui.QTextBrowser()
      
      helpinfo=u"""帮助信息:
            UPlayer是由Mplayer自由软件作后台，用PyQt做前台UI实现的一个属于你自己的视频播放器！
            
            使用手册简要如下（详细手册用man uplayer查看）:
            
            1.播放影音文件：uplayer filename.avi
            2.打开新文件：【文件】【打开】选择新文件即可，快捷键Ctrl+O
            3.通过位置打开新文件：【文件】【打开位置】【输入文件地址】，Ctrl+G
            4.退出，Ctrl+Q
            5.全屏，快捷键F
            6.显示/隐藏控制栏，Ctrl+J
            7.显示侧边栏，快捷键E
            8.静音，快捷键Alt+K,暂停，空格（Space）
            9.快进，Right（->），快退，Left（<-）
            10.增加声音，快捷键“*”，减小声音，快捷键“/”
            11.此帮组信息，F1
            12.版本信息，Ctrl+A
            
            作者：朱春来
            
            联系方式：pythonisland@gmail.com
            """
         
      self.helpinfo_edit.setText(helpinfo)
      
      self.helpinfo_layout_1=QtGui.QHBoxLayout()
      self.helpinfo_layout_1.addWidget(self.helpinfo_edit)
      
      self.helpinfo_button_yes=QtGui.QPushButton(u"确认")
      self.connect(self.helpinfo_button_yes,QtCore.SIGNAL("clicked()"),self.helpinfo_frame,QtCore.SLOT("close()"))
      
      self.helpinfo_layout_2=QtGui.QHBoxLayout()
      self.helpinfo_layout_2.addWidget(self.helpinfo_button_yes)
      
      self.helpinfo_layout=QtGui.QVBoxLayout()
      self.helpinfo_layout.addLayout(self.helpinfo_layout_1)
      self.helpinfo_layout.addLayout(self.helpinfo_layout_2)
      self.helpinfo_frame.setLayout(self.helpinfo_layout)
      self.helpinfo_frame.show()
      self.helpinfo_frame.resize(500,500)

      
      screen = QtGui.QDesktopWidget().screenGeometry()
      size =  self.geometry()
      self.helpinfo_frame.move((screen.width()-size.width())/2, (screen.height()-size.height())/2)

   def version_dialog(self):
      u"版本菜单项事件处理函数"
      
      self.version_frame=QtGui.QFrame()
      self.version_edit=QtGui.QTextBrowser()
      
      versioninfo=u"""版本信息:
            UPlayer是由Mplayer自由软件作后台，用PyQt做前台UI实现的一个属于你自己的视频播放器！
            
            作者：朱春来
            
            版本号：UPlayer-1.0-beta
            
            日期：2011年05月25日
            
            邮箱：pythonisland@gmail.com
            """
         
      self.version_edit.setText(versioninfo)
      
      self.version_layout_1=QtGui.QHBoxLayout()
      self.version_layout_1.addWidget(self.version_edit)
      
      self.version_button_yes=QtGui.QPushButton(u"确认")
      self.connect(self.version_button_yes,QtCore.SIGNAL("clicked()"),self.version_frame,QtCore.SLOT("close()"))
      
      self.version_layout_2=QtGui.QHBoxLayout()
      self.version_layout_2.addWidget(self.version_button_yes)
      
      self.version_layout=QtGui.QVBoxLayout()
      self.version_layout.addLayout(self.version_layout_1)
      self.version_layout.addLayout(self.version_layout_2)
      self.version_frame.setLayout(self.version_layout)
      self.version_frame.show()
      self.version_frame.resize(500,500)

      
      screen = QtGui.QDesktopWidget().screenGeometry()
      size =  self.geometry()
      self.version_frame.move((screen.width()-size.width())/2, (screen.height()-size.height())/2)

   def keyPress_dialog(self, event):
      u"设置ESC键事件处理函数"
      
      #设置按键事件处理
      if event.key() == QtCore.Qt.Key_Escape:
         self.process.close()
         self.close()

   def open_location_dialog(self):
      u"打开位置文件事件处理函数"
      
      self.help_frame=QtGui.QFrame()
      self.help_edit=QtGui.QLineEdit("/home/")
      self.help_button_yes=QtGui.QPushButton(u"确认")
      self.help_button_no=QtGui.QPushButton(u"取消")

      self.connect(self.help_button_yes,QtCore.SIGNAL("clicked()"),self.open_newfile_dialog)
      self.connect(self.help_button_no,QtCore.SIGNAL("clicked()"),self.help_button_no_dialog)     
      
      self.help_layout1=QtGui.QHBoxLayout()
      self.help_layout1.addWidget(self.help_edit)
      
      self.help_layout2=QtGui.QHBoxLayout()
      self.help_layout2.addWidget(self.help_button_yes)
      self.help_layout2.addWidget(self.help_button_no)
      
      self.help_layout=QtGui.QVBoxLayout()
      self.help_layout.addLayout(self.help_layout1)
      self.help_layout.addLayout(self.help_layout2)
      self.help_frame.setLayout(self.help_layout)
      self.help_frame.show()

      screen = QtGui.QDesktopWidget().screenGeometry()
      size =  self.geometry()
      self.help_frame.move((screen.width()-size.width())/2, (screen.height()-size.height())/2)
      

   def help_button_no_dialog(self):
      self.help_frame.close()
   def open_newfile_dialog(self):
      u"打开位置文件后对文件进行处理"
      
      thefile=self.help_edit.text()
      self.change_movie_file(thefile)
      self.help_frame.close()


class SettingIcons(object):
    u"设置图标显示的类"
    
    def __init__(self):


       self.media_icons_path = "/usr/share/uplayer/"
       # 控制栏图标
       self.icon_uplayer = self.media_icons_path + "icons/SeaHare.jpeg"
       self.icon_add = self.media_icons_path + "icons/add.png"
       self.icon_del = self.media_icons_path + "icons/del.png"
       self.icon_save = self.media_icons_path + "icons/save.png"
       self.icon_up = self.media_icons_path + "icons/up.png"
       self.icon_down = self.media_icons_path + "icons/down.png"
        
       self.icon_back = self.media_icons_path + "icons/back.png"
       self.icon_quickback = self.media_icons_path + "icons/quickback.png"
       self.icon_play = self.media_icons_path + "icons/play.png"
       self.icon_pause = self.media_icons_path + "icons/pause.png"
       self.icon_forward = self.media_icons_path + "icons/forward.png"
       self.icon_quickforward = self.media_icons_path + "icons/quickforward.png"
        
       self.icon_sound = self.media_icons_path + "icons/sound.png"
       self.icon_close = self.media_icons_path + "icons/close.png"
       self.icon_version = self.media_icons_path + "icons/version.png"
       self.icon_fullscreen = self.media_icons_path + "icons/ubuntu.png"
       self.icon_getscreen = self.media_icons_path + "incons/getscreen.png"
    
       self.icon_ask = self.media_icons_path + "icons/ask.png"
       self.icon_open = self.media_icons_path + "icons/open.png"
       self.icon_open_location = self.media_icons_path + "icons/open_location.png"
       self.icon_exit = self.media_icons_path + "icons/shutdown.png"
        
       self.icon_goto = self.media_icons_path + "icons/goto.png"
       self.icon_quick = self.media_icons_path + "icons/quick2.png"
       self.icon_slow = self.media_icons_path + "icons/slow2.png"
        
       self.icon_add_sound = self.media_icons_path + "icons/add_sound.png"
       self.icon_sub_sound = self.media_icons_path + "icons/sub_sound.png"

       self.icon_splash = self.media_icons_path + "icons/splash.JPG"
       self.icon_cash = self.media_icons_path + "icons/cash.png"
        
       self.movieFile = ""
       if len(sys.argv) > 1:
          self.movieFile = QtCore.QString(QtCore.QString.fromUtf8(sys.argv[1]))
       else:
          self.movieFile = QtCore.QString(QtCore.QString.fromUtf8( self.media_icons_path +"media/default_2.mp4"))
            
       self.side_bool = False          # 侧边栏控制
       self.playorpause = True         # 播放/暂停图标控制
       self.control_bool=True          # 控制栏显示控制
       self.fullscreen_bool=True       # 全屏控制
       self.nosound_bool=True          # 静音控制

       self.movieFile_dict = {}        # 记录播放列表

class SettingMovieScreen(object):
    def update_movie(self):
        u"改变视频文件进行播放"
        
        self.program = "/usr/bin/mplayer"

        self.text = " -slave -quiet –ac –mad -input file=/tmp/cmd "+self.movieFile+" -wid "+str(self.frame_center.winId())+" &"
        
                
        self.process = QtCore.QProcess(self.frame_center)

        self.process.start(self.program+self.text,QtCore.QIODevice.ReadWrite)
        self.connect(self.process,QtCore.SIGNAL("readyReadStandardOutput()"),self.back_message_dialog)
        
        self.list_action = QtGui.QAction(self.movieFile,self)
        self.listwidget.addAction(self.list_action)
        self.connect(self.list_action,QtCore.SIGNAL("clicked()"),self.list_action_event)

        # 文件名以及侧边栏?
        self.movieFile_basename=os.path.basename(unicode(self.movieFile.toUtf8(),'utf8','ignore').encode('utf8'))
        self.movieFile_pathname=os.path.dirname(unicode(self.movieFile.toUtf8(),'utf8','ignore').encode('utf8'))+"/"
                                                
        self.setWindowTitle(self.movieFile_basename)
        self.movieFile_dict[self.movieFile_basename]=self.movieFile_pathname
        self.listwidget.addItem(self.movieFile_basename)
        self.connect(self.listwidget,QtCore.SIGNAL("itemClicked()"),self.play_item_dialog)

        # 设置时间进度等信息
        self.length = self.process.write("get_time_length\n")
        self.pos = self.process.write("get_time_pos\n")
        self.percent_pos=self.process.write("get_percent_pos\n")

    def play_item_dialog(self):
       print 'in play_item_dialog'
       
    def back_message_dialog(self):
        while(self.process.canReadLine()):
            line=self.process.readLine() # QByteArray

            if line.startsWith("ANS_TIME_POSITION="):
                self.pos=line[19:]
                self.slider.setValue(self.pos)
            if line.startsWith("ANS_LENGTH="):
                self.length=line[11:].chop(2)

            self.slider.setRange(0,int(self.length))
            self.length_label.setText(str(self.pos)+"/"+str(self.length))

    def settingMovieScreen(self):
        u"设置播放画面"
        
        self.frame_center = QtGui.QFrame(self)
        self.setCentralWidget(self.frame_center) # 位置是中央区域
        self.frame_center.show()

        self.splashscreen=QtGui.QSplashScreen()
        self.splashscreen.setPixmap(QtGui.QPixmap(self.icon_splash))
        self.splashscreen.show()
        
        
        #侧边区域设置
        self.dock_right = QtGui.QDockWidget(self)
        self.dock_right.setAllowedAreas(QtCore.Qt.RightDockWidgetArea)
        self.dock_right.setShown(False)

        # 文件列表
        self.frame_right=  QtGui.QFrame(self)

        self.dock_right.setWidget(self.frame_right)
        self.dock_right.setWindowTitle(u"侧边栏")
        self.dock_right.setStatusTip(u"侧边栏")
        self.addDockWidget(QtCore.Qt.RightDockWidgetArea,self.dock_right)

        self.listwidget = QtGui.QListWidget(self.frame_right)
        
        
        self.layout_1 = QtGui.QHBoxLayout()
        self.layout_1.addWidget(self.listwidget)

        # 文件列表控制栏
        self.button_add = QtGui.QPushButton(QtGui.QIcon(self.icon_add),"")
        self.button_add.setStatusTip(u"增加文件")
        
        self.button_del = QtGui.QPushButton(QtGui.QIcon(self.icon_del),"")
        self.button_del.setStatusTip(u"删减文件")
        
        self.button_save = QtGui.QPushButton(QtGui.QIcon(self.icon_cash),"")
        self.button_save.setStatusTip(u"清空")
        
        self.button_up = QtGui.QPushButton(QtGui.QIcon(self.icon_up),"")
        self.button_up.setStatusTip(u"文件次序上升")
        
        self.button_down = QtGui.QPushButton(QtGui.QIcon(self.icon_down),"")
        self.button_down.setStatusTip(u"文件次序下降")
        
        self.layout_2 = QtGui.QHBoxLayout()
        self.layout_2.addWidget(self.button_add)
        self.layout_2.addWidget(self.button_del)
        self.layout_2.addWidget(self.button_save)
        self.layout_2.addWidget(self.button_up)
        self.layout_2.addWidget(self.button_down)

        self.layout_3 = QtGui.QVBoxLayout(self.frame_right)
        self.layout_3.addLayout(self.layout_1)
        self.layout_3.addLayout(self.layout_2)
        self.dock_right.adjustSize()
        
        #下方控制栏区域设置
        self.dock_bottom=QtGui.QDockWidget(self)
        self.frame_bottom=QtGui.QFrame(self)

        #?
        self.time_label=QtGui.QLabel(u"时间")
        self.slider = QtGui.QSlider(QtCore.Qt.Horizontal) # ?
      
        self.slider.setRange(0,100)     # ?
        self.slider.setValue(0)
        self.slider.setStatusTip(u"时间进度"+str(self.slider.value()))
        self.connect(self.slider, QtCore.SIGNAL("valueChanged(int)"), self.slider_time_dialog)

        self.length_label=QtGui.QLabel("")
        self.layout = QtGui.QHBoxLayout()
        self.layout.addWidget(self.time_label)
        self.layout.addWidget(self.slider)
        self.layout.addWidget(self.length_label)

        # 添加控制按钮
        self.button_back = QtGui.QPushButton(QtGui.QIcon(self.icon_back),"")
        self.button_back.autoFillBackground() # .........要填充?
        self.button_back.setStatusTip(u"倒退")
        
        self.button_quickback = QtGui.QPushButton(QtGui.QIcon(self.icon_quickback),"")
        self.button_quickback.setStatusTip(u"快速倒退")
        
        self.button_play = QtGui.QPushButton(QtGui.QIcon(self.icon_pause),"")
        self.button_play.setShortcut("space")
        self.button_play.setStatusTip(u"播放")
        
        self.button_pause = QtGui.QPushButton(QtGui.QIcon(self.icon_pause),"")
        self.button_pause.setStatusTip(u"暂停")
        
        self.button_forward = QtGui.QPushButton(QtGui.QIcon(self.icon_forward),"")
        self.button_forward.setStatusTip(u"前进")
        
        self.button_quickforward = QtGui.QPushButton(QtGui.QIcon(self.icon_quickforward),"")
        self.button_quickforward.setStatusTip(u"快速前进")
        
        self.button_fullscreen = QtGui.QPushButton(QtGui.QIcon(self.icon_fullscreen),"")
        self.button_fullscreen.setStatusTip(u"全屏播放")

        # 对各个控制按钮进行功能绑定
        self.connect(self.button_forward,QtCore.SIGNAL("clicked()"),self.button_forward_event)
        
        self.connect(self.button_quickback,QtCore.SIGNAL("clicked()"),self.button_quickback_event)
        
        self.connect(self.button_quickforward,QtCore.SIGNAL("clicked()"),self.button_quickforward_event)
        
        self.connect(self.button_back,QtCore.SIGNAL("clicked()"),self.button_back_event)
        
        self.connect(self.button_fullscreen,QtCore.SIGNAL("clicked()"),self.fullscreen_event)
        
        self.connect(self.button_play,QtCore.SIGNAL("clicked()"),self.play_event)
        
        self.sound_label = QtGui.QLabel(u"声音")
        self.sound_label.show()
        
        self.sound_slider = QtGui.QSlider(QtCore.Qt.Horizontal)

        self.sound_slider.setRange(0,100)
        self.sound_slider.setValue(88)
        self.sound_slider.setStatusTip(u"声音大小"+str(self.sound_slider.value()))
        self.connect(self.sound_slider,QtCore.SIGNAL("valueChanged(int)"),self.sound_slider_dialog)

        
        self.button_side = QtGui.QPushButton(QtGui.QIcon(self.icon_close),"")
        self.button_side.setStatusTip(u"侧边栏开关")
        self.connect(self.button_side,QtCore.SIGNAL("clicked()"),self.dock_right_true)

        # 布局管理
        self.layout_2 = QtGui.QHBoxLayout()
        self.layout_2.addWidget(self.button_quickback)
        self.layout_2.addWidget(self.button_back)
        self.layout_2.addWidget(self.button_play)

        self.layout_2.addWidget(self.button_forward)
        self.layout_2.addWidget(self.button_quickforward)
        self.layout_2.addWidget(self.button_fullscreen)

        self.layout_2.addStretch(10)
        self.layout_2.addWidget(self.sound_label)
        self.layout_2.addWidget(self.sound_slider)
        self.layout_2.addWidget(self.button_side)
        

        self.layout_3 = QtGui.QVBoxLayout(self.frame_bottom)
        self.layout_3.addLayout(self.layout)
        self.layout_3.addLayout(self.layout_2)

        #
        self.dock_bottom.setWidget(self.frame_bottom)
        self.dock_bottom.setWindowTitle(u"控制栏")
        self.dock_bottom.setStatusTip(u"控制栏")
        self.dock_bottom.DockWidgetClosable = False
        self.addDockWidget(QtCore.Qt.BottomDockWidgetArea,self.dock_bottom)

        self.setCorner(QtCore.Qt.Corner(3),QtCore.Qt.RightDockWidgetArea)

        # 播放视频
        self.update_movie()


    def slider_time_dialog(self):
        self.process.write("seek "+str(self.slider.value())+"\n")
        self.slider.setStatusTip(u"时间进度"+str(self.slider.value()))
        
    def sound_slider_dialog(self):
        u"声音进度条事件处理函数"
        
        value=self.sound_slider.value()
        self.process.write("set_property volume "+str(value)+" 2\n")
        self.sound_slider.setStatusTip(u"声音大小"+str(self.sound_slider.value()))

    def button_back_event(self):
        u"倒退10帧开始播放"
        
        self.process.write("seek -1 0\n")
        self.slider.setValue(self.slider.value()-1)
        self.slider.setStatusTip(u"时间进度"+str(self.slider.value()))
        
    def button_forward_event(self):
        u"前进10帧开始播放"
        
        self.process.write("seek +1 0\n")
        self.slider.setValue(self.slider.value()+1)

        self.slider.setStatusTip(u"时间进度"+str(self.slider.value()))
        
    def button_quickback_event(self):
        u"倒退50帧开始播放"
        
        self.process.write('seek -5 0\n')
        self.slider.setValue(self.slider.value()-5)
        self.slider.setStatusTip(u"时间进度"+str(self.slider.value()))
        
    def button_quickforward_event(self):
        u"前进50帧开始播放"
        
        self.process.write("seek +5 0\n")
        self.slider.setValue(self.slider.value()+5)
        self.slider.setStatusTip(u"时间进度"+str(self.slider.value()))

    def play_event(self):
        u"播放按钮事件处理函数"
        
        self.process.write("pause\n")
        self.playorpause = not self.playorpause
        
        if self.playorpause:
            self.button_play.setIcon(QtGui.QIcon(self.icon_pause))
            self.button_play.setStatusTip(u"暂停")
        else:
            self.button_play.setIcon(QtGui.QIcon(self.icon_play))
            self.button_play.setStatusTip(u"播放")

    def dock_right_true(self):
        u"侧边栏显示开关处理函数"
        
        self.side_bool = not self.side_bool
        self.dock_right.setShown(self.side_bool)

        

    def list_action_event(self):
        u"侧边栏文件列表事件处理函数"
        
        print "in list_action_event"

    def setCmd(self):
        u"管道文件处理函数"
        
        if os.path.exists("/tmp/cmd"):
            os.remove("/tmp/cmd")

        os.mkfifo("/tmp/cmd")
        
    def play_event_default(self):
        u"播放按钮图标处理函数"
        
        self.playorpause = True
        self.button_play.setIcon(QtGui.QIcon(self.icon_pause))
        
    def showFile_dialog(self):
        u"打开新播放文件事件处理函数"
        
        #打开文件函数
        filename = QtGui.QFileDialog.getOpenFileName(self, u'打开文件','/home')

        if filename == '':
            return
        self.change_movie_file(filename)
        
    def change_movie_file(self,filename=None):
        u"更新播放文件"
        
        self.movieFile = filename
        self.process.write("quit\n")
        self.play_event_default()
        self.setCmd()
        self.slider.setValue(0)
        self.update_movie()
        
    def fullscreen_event(self):
        u"全屏显示处理函数"
        
        if self.fullscreen_bool:
            self.control_bool=True
            self.control_dialog()
            self.showFullScreen()
            self.fullscreen_bool=False
        else:
            self.fullscreen_bool=True
            self.showNormal()
            self.control_bool=False
            self.control_dialog()

class UPlayer(QtGui.QMainWindow,SettingMenuBar,SettingMovieScreen,SettingIcons):
    u"""主窗口，用于元素布局"""
    
    def __init__(self):
        
        # 父类构造函数
        QtGui.QMainWindow.__init__(self)
	SettingMenuBar.__init__(self)
	SettingMovieScreen.__init__(self)
        SettingIcons.__init__(self)
        
        #设置窗口大小等信息
        self.resize(600,600)
        self.setWindowTitle(u"UPlayer-beta")
        self.setWindowIcon(QtGui.QIcon(self.icon_uplayer))
        self.statusBar().showMessage(u"你自己的MPlayer")
        self.setToolTip(u"你自己的MPlayer")
        QtGui.QToolTip.setFont(QtGui.QFont('OldEnglish', 10))

        # 播放画面居中
        screen = QtGui.QDesktopWidget().screenGeometry()
        size =  self.geometry()
        self.move((screen.width()-size.width())/2, (screen.height()-size.height())/2)

        
        #设置菜单栏等
        self.settingMenuBar()
        self.settingMovieScreen()

        
def main():
    app = QtGui.QApplication(sys.argv)
    w = UPlayer()
    w.show()
    sys.exit(app.exec_())
      
if __name__ == '__main__':
    main()
    os.system("rm /tmp/cmd")
