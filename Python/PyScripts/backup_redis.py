#!/usr/bin/env python

import os,sys,time

stat=os.stat("~/run/dump.rdb")
filename=os.popen("ls -tr ~/run/dump*.gz|tail -1|awk -F' ' '{print $NF}'").readline().strip()
stat_backup=os.stat(filename)
if stat[-1] > stat_backup[-1]:
   os.system("cd ~/run/ && tar czvf dump_$(date +%Y%m%d_%H%M).tar.gz dump.rdb")
