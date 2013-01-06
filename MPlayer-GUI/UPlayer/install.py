#!/usr/bin/env python
#-*- coding:utf-8 -*-

import os

print "=" * 10,"Welcome To UPlayer","=" * 10

print "=" * 5, "mkdir /usr/share/uplayer..."
if not os.path.exists("/usr/share/uplayer"):
    os.system("mkdir /usr/share/uplayer")
print "=" * 5,"done..."

print "=" * 5,"mkdir /usr/share/uplayer/media...."
if not os.path.exists("/usr/share/uplayer/media"):
    os.system("mkdir /usr/share/uplayer/media/")
print "=" * 5,"done..."

print "=" * 5,"mkdir /usr/share/uplayer/icons..."
if not os.path.exists("/usr/share/uplayer/icons"):
    os.system("mkdir /usr/share/uplayer/icons/")
print "=" * 5,"done...."

print "=" * 5,"cp media/* /usr/share/uplayer/media/...."
os.system("cp media/* /usr/share/uplayer/media/")
print "=" * 5,"done..."

print "=" * 5,"cp icons/* /usr/share/uplayer/icons/..."
os.system("cp icons/* /usr/share/uplayer/icons/")
print "=" * 5,"done..."

print "=" * 5,"cp uplayer.man /usr/share/man/man8/...."
os.system("cp uplayer.8.gz /usr/share/man/man8/")
print "=" * 5,"done..."
print "=" * 5,"you can use 'man uplayer' or 'man 8 uplayer' get the help manual..."

print "=" * 5,"cp uplayer.py /usr/bin/uplayer....."
os.system("cp uplayer.py /usr/bin/uplayer")
print "=" * 5,"done...."

print "=" * 5,"chmod u+x /usr/bin/uplayer..."
os.system("chmod 775 /usr/bin/uplayer")
print "=" * 5,"done...."
print "=" * 5,"you can use 'uplayer [optin] <movie>' ...."

print "=" * 5,"install python2.6..."
in_python = raw_input("=====Have you installed python?[Y/N]")
if in_python in ["n","no","N","No","NO","nO"]:
    os.system("sudo apt-get install python2.6")
print "=" * 5,"done.."

print "=" * 5,"install pyqt..."
in_pyqt = raw_input("=====Have you installed python-qt4?[Y/N]")
if in_pyqt in ["n","no","N","No","NO","nO"]:
    os.system("sudo apt-get install pyqt-tools pyqt4-dev-tools")
print "=" * 5,"done.."

print '=' * 40
print '=' * 10,"SETUP SUCCESS",'=' * 10
print '=' * 40
