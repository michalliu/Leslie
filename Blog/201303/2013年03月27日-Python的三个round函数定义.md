# round:四舍五入

Python语言内建函数round，使用过程中常常会出人意料。

* 对末尾0的处理：说不定什麽时候保留
```python
round(1.20)   #1.0
round(1.20,2) #1.2
```

* 常让人犯错的除法:以操作数决定取位
```python
round(1137/2303,2)             #0.0
round(float(1137)/2303,2)      #0.49
```

# 自己定义：版本一

```python
round = lambda f,i=0: float(int(f*(10**i) + 0.5))/(10**i) if f > 0 else float(int(f*(10**i) - 0.5))/(10**i)
```

# 自己定义：版本二

```python
newround = lambda f,i=0: float(int(f*(10**i) + 0.5))/float(10**i) if f > 0 else -1*float(int(-1*f*(10**i) + 0.5))/float(10**i)
```

# 自己定义：版本三

四舍五入后以str形式保存，强制保留后面的零：
```python
def round_to_str(f,i=0):
	result=float(int(f*(10**i) + 0.5))/float(10**i) if f > 0 else -1*float(int(-1*f*(10**i) + 0.5))/float(10**i)
    format="%0."+str(i)+"f"
    return format % result
```

版本一和版本二的结果一样，只是版本二似乎更好读些,但都存在一个问题（也不算问题），数字形式会自动忽略后面的零：

```python
round(string.atof('3.6012'),2) #3.6
```

但是，这里我就是想保留两位（比如做报告），以字符串形式保存：
```python
round_to_str(string.atof('3.6012'),2) #'3.60'
```

# 用round要注意的问题

* Python的除法保留精度根据操作数自动决定
* Python的数字形式会自动忽略末尾的零，除非以string形式保存
