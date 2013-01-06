#!/usr/bin/env python
#-*- coding:utf-8 -*-

import os

print "=" * 10,"Thank you for using UPlayer","=" * 10

print "=" * 5,"Remove /usr/share/uplayer"
if os.path.exists("/usr/share/uplayer"):
    os.system("sudo rm -r /usr/share/uplayer")

print "=" * 5,"Done..."

print "=" * 5,"Remove /usr/share/man/man8/uplayer.8.gz"
if os.path.exists("/usr/share/man/man8/uplayer.8.gz"):
    os.system("sudo rm /usr/share/man/man8/uplayer.8.gz")

print "=" * 5,"Done..."

print "=" * 5,"Remove /usr/bin/uplayer"
if os.path.exists("/usr/bin/uplayer"):
    os.system("sudo rm /usr/bin/uplayer")
print "=" * 5,"Done..."

print "=" * 40
print "=" * 10,"UNINSTALL SUCCESSED","=" * 10
print "=" * 40
