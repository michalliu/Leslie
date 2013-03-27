# 说明

这是GNU/Linux 各种命令的用法记录。

# 1.join如何用TAB分割 : $"\t"

```bash
join -t $"\t" -1 4 -2 1 file1 file2
```

# 2. python如何四舍五入：根据操作数决定

```python
round(100  *1078/3512,2) #30.0
round(100.0*1078/3512,2) #30.69
round(100  *float(1078)/3512,2) #30.69
```

# 3. Shell中unzip解压：脚本中指定解压到某个目录

```bash
# 之前脚本中没有指定解压后的目录，导致发生解压失败的情况
unzip -d dir -o file.zip
```
