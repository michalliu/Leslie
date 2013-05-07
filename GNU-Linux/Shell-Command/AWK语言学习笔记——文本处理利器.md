# AWK语言学习笔记——文本处理利器

2012年6月30日

这是我看书做的笔记，以前觉得自己对awk进行文本处理已经不错了，最近工作中遇到一些数据量大、格式复杂的报表处理时，我才知道awk的高级应用我还不了解。为了更好应对以后的千军万马，我重新整理这个笔记，希望磨刀不误砍柴功！
        
# 一、简介

awk语言的设计目的是为了简化一般文本处理的工作，这让awk在文本处理方面十分简单且强大。正是因为awk很简单与功能强大，这让awk看起来就是为了某种工作而设计的工具；使用awk语言的话，很少遇到需要文本处理却找不到可用的功能或者实现很难的情况。

在文本处理方面，如果你想用其它语言代替awk，你收获的也许是稍微快一点的速度，但你将耗费至少一倍的时间，以及更难调试的代码。
 
# 二、awk命令行

```bash
Usage: awk [POSIX or GNU style options] -f progfile [--] file ...
Usage: awk [POSIX or GNU style options] [--] 'program' file ...
```

短程序通常是直接在命令行上运行，长程序则通过-f选项指定。

如果没有指定文件名，则awk会读取标准输入。

```text
--是特殊选项，指出awk本身并没有更进一步的命令行选项，任何接下来的选项都可以被程序使用。
-F选项用来重新定义默认字段分隔符号，且一般惯例把它作为第一个命令行选项。
-v选项从shell传递参数给awk，此选择一般要在最前面。
```
 
# 三、awk程序模型

awk把输入流看作一连串记录的集合，每条记录都可进一步细分为字段。通常，一行一条记录，而字段则由一个或多个非空白字符的单词组成。

一个awk程序是一对以模式（pattern）与大括号括起来的操作（action）组合而成的。

模式和操作可省略其中一个，模式省略则操作应用到每一条记录，操作省略则默认操作是打印匹配记录。

传统awk配置：

```bash
pattern  {action} 模式匹配，则执行操作
pattern           模式匹配，则打印记录
{action} 每条记录，执行操作
```

输入会自动地由一个输入文件切换到下一个文件，且awk会处理输入文件的打开、读取和关闭。

awk保留了BEGIN和END两种特殊模式：

* 与BEGIN关联的操作只会执行一次，在任何命令行文件或一般命令行赋值被处理之前，但是任何开头的-v选项指定已经完成之后。主要用于初始化工作。
* 与END关联的操作也是执行一次，用于所有输入数据已经被处理完之后，多半用于产生摘要报告或清除操作。


  
# 四、程序元素

awk里面的注释从#开始倒该行结束，如shell脚本，空行等于空的注释。

空白字符可以任意，但不能将单条语句分割为跨越多行，除非在行切断的地方立即前置一个反斜杠，如shell一样。

awk里面的字符串常数以引号定界，字符串可以包含任何8-bit的字符，除控制字符NUL（字符值0）以外。因为NUL在底层实现语言C语言中是一个中断字符的角色。gawk没有这个限制。

字符串比较，关系操作符：

```text
==（相等） ！=（不等） <（小于）
<=（小于等于） >（大于） >=(大于等于）
比较后返回1为真，0为假。
```

awk不用字符串连接函数，两个连续字符串会自动连接在一起，并且如前面所说，可以有空格，空格会被忽略。

把数字转换为字符串，通过数字连接空字符串即可，如n=123,s="" n。

awk支持正则表达式，运算符～表示（匹配），运算符！～表示（不匹配）。
如～ /^[A-Z]+$/，正则表达式常以斜杠形式出现，也可以以引号形式出现，如果有字面意义的斜杠，需要用反斜杠进行保护。

所有awk里面的数字，都以双精度的浮点值表示。
浮点数可以包含一个e或E结尾所表示的10次方指数，以及正负号的整数。

字符串转换为数字，只要加一个零倒字符串里面，如s="123",n=0 + s,则n获得数值123.
通常是这样转换数字：
```text
“+123ABC”   ---   123
“ABC”       ---   0
“”          ---   0
“ABC123”    ---   0
```
即强制非数值字符串转换为数字。

awk运算符（优先级由大到小）：
```text
++ --      增加或减少（前置或后置）
^ **      指数（右结合性）
! + -      非、一元加号、一元减号
 * / %      乘、除、余数
 + -      加、减
 < <= == != > >=     比较
 &&      逻辑AND
 ||      逻辑OR
 ?:      三元条件式
 = += -= *= /= %= ^= **=    赋值（右结合性）
```
对于优先级，可以用括号改变。
 
内建函数int可以返回参数的整数部分，如int(-3.14156)=-3.
 
保存单一值的变量叫标量变量，变量不需要先声明；所有awk变量建立时初始化为空字符串，即数值0.

量名称必须符合正则表达式[A-Za-z_][A-Za-z0-9]*。
awk是大小写敏感的，将全局变量首字母大写，局部变量全部小写，内建变量全部大写。
 
常用内建变量：
```bash
FILENAME  当前输入文件的名称，即处理哪个文件
FNR   当前输入文件的记录数，即当前输入文件中第几条记录
FS   输入记录字段分隔符（正则表达式）（默认是空格）
NF   当前记录的字段数，即有几个字段
NR   在工作中的记录数，即在所有输入文件中的第几条记录
OFS   输出字段的分割字符（默认空格）
ORS   输出记录分割字符（默认\n）
RS   输入记录分割字符（默认\n）
$LINE   当前处理记录即$0
SUBSEP   代替多维数组的索引分割逗号
ARGC   参数计数
ARGV   参数向量
ENVIRON   环境变量
OFMT      数字输出格式，默认%.6g
CONVFMT   数字内部转换格式，默认%.6g
RSTART      match()函数正则匹配到字符串开始位置
RLENGTH     match()函数正则匹配到字符串的长度
```

 
 
awk支持数组，不但支持数字索引，还支持类似字典的索引；这种以任意值为索引的数组叫关联数组。
如：
```bash
recode[1] = "hello"
recode["telephone"] = 123445
``` 
awk数组不需要声明，数组大小在引用新元素时会自动增长。
delete array[index]删除元素，delete array删除所有元素。

一个变量不可以同时作为标量变量和数组变量，delete会删除数组元素，但不会删除数组定义。
awk通过将“以逗号分割的索引列表”看作一个字符串，而使用这个索引模拟数组。然而，由于逗号本身也可能出现在索引值内，因此awk用内建变量SUBSEP代替索引分隔符逗号，一般是\034。

以下四个表示相同项目：

```bash
 print maildrop[53, "oak lane", "t4q 7xv"]
 print maildrop["53" SUBSEP "oak lane" SUBSEP "t4q 7xv"]
 print maildrop["53\034oak lane","t4q 7xv"]
 print maildrop["53\034oak lane\034t4q 7xv"]
```
应该在BEGIN模式里面设置SUBSEP一次，后面不要改变。
利用关联数组，有了另一种选择。

awk通过内建变量ARGC（参数计数）与ARGV（参数向量），让命令行参数可用，其中ARGV[0]表示程序名。
ARGC和ARGV可修改，如果修改了ARGV，一定要修改ARGC的大小。
 
当awk发现参数含有程序或者特殊--选项时，会立即停止将参数解释为选项；任何接下来的的参数，都由awk程序处理，并从ARGV删除或设置为空白字符。
 
如果在shell脚本中包裹awk引用，可以把awk程序定义为变量，用单引号括起；也可以作为单个文件放到特定目录，如：
```bash
#! /bin/sh -
AWK=${AWK:-nawk}
$AWK -f `driname $0`/../share/lib/myprog.awk --"$@"
```
 
 
awk提供访问内建数组ENVIRON中所有环境变量，如ENVIRON["HOME"],ENVIRON["USER"]等。
应将ENVIRON数组看作只读数组，不要修改其内容。
 
 
# 五、记录与字段

在awk程序中，通过输入文件隐含的循环每一次迭代，会处理单一记录(record)，通常是一行文本。记录可以进一步细分为更小的字符串，叫字段(field)。
通常记录分隔符是换行符，但可以用内建变量RS来指定记录分隔符。文件开头和结尾的空行被忽略。
gawk和mawk支持RS是正则表达式，即换行符不一定是单一字符。
如果RS不支持正则表达式，却要处理多行作为一条记录时，可以用tr命令将换行符转换为其它未用到的字符。
gawk、mawk和emacs是少数几个没有面向行的数据浏览限制（缓冲区大小）的程序。

字段分隔符FS默认是单一空格，行的开头和结尾的空白将被忽略，但FS=“[ ]”时不忽略行的开头和结尾空白。
FS只有在它超过一个字符时，才会被视为正则表达式。

字段以特殊名称$1,$2,$3,....,$NF供awk程序使用，特殊字段名称$0表示当前记录。
字段可以赋值，如$1="alef"，但会有副作用。

# 六、模式与操作

模式由字符串与/或数值表达式构建而成：一旦计算出当前记录的值为非零（真），则实行结合性操作；如果模式是正则表达式，则此表达式被拿来和整个记录进行匹配。
常用选定模式：
```bash
NF == 0      选定空行
NF > 3      选定拥有三个字段以上的记录
NR < 5      选定第1到第4条记录
(FNR == 3) && (FILENAME ~/[.][ch]$/)  于C文件中选定记录3
$1 ~ /jones/     选定字段1中有“jones”的记录
/[Xx][Mm][Ll]/     选定含有“XML”的记录，并忽略大小写差异
$0 ~ /[Xx][Mm][Ll]/    同上
```
  
awk在匹配功能上，还可以使用范围表达式(range expression)，以逗号隔开的两个表达式，会从匹配与左边表达式开始取样，直到匹配右边的表达式。如果两个范围表达式匹配后匹配一条记录，则选定该单一记录。
如：
```bash
(FNR == 3),(FNR ==10)    选定每个输入文件里的记录3到10
/<[Hh][Tt][Mm][Ll]>/,/<\/[Hh][Tt][Mm][Ll]>/ 选定HTML文件里的主体
/[aeiouy][aeiouy]/,/[^aeioy][^aeioy]/  选定起始于两个元音、结尾为两个辅音的记录
```
在BEGIN操作里，FILENAME、FNR、NF和NR初始都未定义，引用到他们时，会返回null字符串或零。

如果awk程序里仅仅包括BEGIN模式操作，则awk完成最后一个操作之后退出，不用读取任何文件。
进入第一个END操作时，FILENAME是最后一个要处理的输入文件，而FNR，NF和NR则会保留它们从最后一条输入记录而来的值。
在END操作里的$0不可靠。

操作用来标记如何处理匹配记录。
纯print是在标准输出上打印当前的输入记录，接着输出记录分隔符ORS，默认是单一换行符。
如果要更改输出字段分割字符，并指定至少一个字段（即时不改变其值），强制以新的字段分隔符重新组合记录，如：
```bash
echo "one tow three four" | awk '{OFS="\n";$1=$1;print $0}'
```

# 七、在awk里的单行程序

* UNIX单词计数程序wc：
```bash
awk '{C += length($0) + 1; W += NF} END {print NR,W,C}'
```

 
* 如果程序为空，则awk不会读取任何的输入并立即退出，可以匹配cat
```bash
awk '' *.xml
```

 
* 撇开NUL字符问题，awk可以轻松替代cat
```bash
awk 1 *.xml
```

 
* 要将原书数据值及它们的对数打印为单栏的数据文件
```bash
awk '{ print $1, log($1) }' file(s)
```

 
* 要从文本文件里，打印5%行左右的随机样本：
```bash
awk 'rand() < 0.05' file(s)
```

 
 * 以空白分割字符的表格中，报告第n栏的和：
 ```bash
 awk -v COLUMN=n '{ sum += $COLUMN} END{ print sum}' file(s)
 ```

 
 * 报表第n栏的平均值：
 ```bash
 awk -v COLUMN=n '{ sum += $COLUMN} END{ print sum/NR}' file(s)
 ```


* 统计最后一个字段的和：
```bash
 awk '{ sum += $NF; print $0, sum}' file(s)
 ```

 
 * 三种查找文件内文本的方式：
 ```bash
 egrep 'pattern|pattern' files(s)
 awk '/pattern|pattern/' files(s)
 awk '/pattern|pattern/' {print FILENAME ":" FNR ":" $0}' file(s)
 ```

* 查找100-150行：
```bash
 awk '{100 <= FNR} && {FNR <= 150} &&  /pattern|pattern/ { print FILENAME ":" FNR ":" $0}' file(s)
 ```
 
* 调换第三字段和第四字段位置：
 ```bash
 awk -F'\t' -v OFS='\t' '{ print $1, $2, $4,$3}' old > new
 awk 'BEGIN { FS=OFS="\t"} {print $1, $2, $4,$3}' old > new
 awk -F'\t' '{ print $1 "\t" $2 "\t" $4 "\t" $3}' old > new
 ```
 
* 替换分隔符：
 ```bash
 sed -e 's/\t/\&/g' file(s)
 awk 'BEGIN { FS="\t";OFS="&"} { $1 = $1; print}' file(s)
 ```
 
* 删除重复行：
 ```bash
 sort file(s) | uniq
 sort file(s) | awk 'Last != $0 { print } { Last = $0}'
 ```
 
* DOS文本转换为UNIX文本：
 ```bash
 sed -e 's/\r$//' file(s)
 sed -e 's/^M$//' file(s)
 mawk 'BEGIN { RS = "\r\n"} { print }' file(s)
 ```
 
* 将单换行符的行转换为双换行符的行：
 ```bash
 sed -e 's/$/\n/' file(s)
 awk 'BEGIN { ORS = "\n\n" } { print } file(s)
 awk 'BEGIN { ORS = "\n\n" } 1 ' file(s)
 awk '{ print $0 "\n"} file(s)
 awk '{ print; print ""}' file(s)
 ```
 
* 删除空行或只有空格组成的行：
 ```bash
 gawk 'BEGIN { RS="\n *\n"} 1' file(s)
 ```

 
* 截去HTML文本里以角括号框起的标记标签(markup tag)：
 ```bash
 mawk 'BEGIN { ORS = " "; RS="<[^<>]*>" } {print }' *.html
 ```

 
* 寻找长度超过72个字符的行：
 ```bash
 awk 'length($0) > 72 { print FILENAME ":" FNR ":" $0}' *.f
 ```

 
 
# 八、语句

语句结尾可以用有分号，但一般不写。
 
 条件选择语句：
 ```bash
 if (expression)
  statement
 else if (expression)
  statement
 ....
 else
  statement
  ```
  
  重复执行：
  ```bash
 --> while(expression)
   statement
   
 --> do
   statement
  while(expression)
  
 --> for(expr1; expr2; expr3)
   statement
   
 --> for (key in array)
   statement 
   ```
   
awk支持break和continue语句。
 
 举例：
 ```bash
 #awk -f factorize.awk
 {
  n = int($1)
  m = n = (n >= 2) ? n:2
  for (k=2; (m>1) && (k^2 <= n);)
  {
   if (int(m %k) != 0)
   {
    k++;
    continue;
   }
   m /=k;
   factors=(factors == "") ? ("" k) : (factors " * " k);
  }
  if (1 < m) && (m < n))
   factors= factors " * " m
  print n, (factor == "") ? "is prime" : ("= " factor)
 }
 ```
 
 对于数组成员的测试，成员测试不可能建立元素，然而引用元素时，如果元素不存在，便会建立它。
因此：
```bash
if ("sally" in array)
....
```
而非：
```bash
if (arrar["sally"] != "")
....
```
要区分寻找索引（index）和寻找特定值（value)的差异。
寻找索引需要固定的时间，而值的寻找需要的时间和元素个数成正比，如果要进行大量值的寻找，可以：
for (name in array)
new_array[array[name]] = name
以后寻找array的元素值时改为查找new_array的索引。

其它流程控制：
* 只针对此记录省略进一步的模式检查，用next语言。
* 针对当前输入文件省略进一步的模式检查，用nextfile语句。
* 省略整个工作的更进一步执行，并返回状态码给shell，用exit n语言。
  
 
可以通过awk的getline语句来控制用户的输入。
当输入被成功读取时，它的返回值为+1，而返回值为0时，表示文件结尾，而-1则表示错误：
```bash
getline      从当前输入文件中，读取下一条记录，存入$0，更新NF,NR,FNR
getline var     从当前输入文件中，读取下一条记录，存入var，更新NF，NR，FNR
getline < file     从file中读取下一条记录，存入$0，更新NF
getline var < file    从file中读取下一条记录，存入var
cmd | getline     从外部命令cmd读取下一条记录，存入$0，更新NF
cmd | getline var    从外部命令cmd读取下一条记录，存入var
```
 
举例：
```bash
print "What is the squeue root of 625?"
getline answer
print "Your reply, ", answer ", is", (answer == 25) ? "right" : "wrong"
```
 
命令管道在awk使用举例：
```bash
"date" | getline now
close("date")
print "The current time is", now
```
通过close来关闭管道文件，即close(cmd)。
 
 
awk也支持输出重定向,如：
```bash
print "Hello, world" > file
printf("The tenth power of %d is %d\n", 2, 2^10) > "/dev/tty"
print "hello" >> file
close(file)

print "hello" | sort > file
close("sort > file")
```
 
 
 
 
awk语言通过system函数执行外部程序，如：
```bash
tmpfile = "/tmp/telephone.tmp"
for (name in telephone)
print name "\t" telephone[name] > tmpfile
close(tmpfile)
system("sort < " tmpfile)
```
system函数的返回值是执行命令的返回值，并且system函数执行命令后不需要调用close函数，close函数只针对I/O文件或管道等。
传递给system函数的命令可以包含多行：
```bash
system("cat <<EOFILE\nFirst\nTwo\nThree\nEOFILE")
```

 
 
由于每次调用system函数都会起始一个全新的Shell，因此没有简单的方式可以在分开的system调用内的命令之内传递数据，除非通过中间文件。
这有个方案，将输出管道传递到Shell，以送出多个命令：
```bash
 Shell = "/usr/local/bin/ksh"
 print "export INPUTFILE=/var/tmp/myfile.in" | Shell
 print "export OUTPUTFILE=/var/tmp/myfile.out" | Shell
 print "env | grep PUFILE" | Shell
 close(Shell)
 ```
 
 

# 九、用户定义函数

没有函数的话，awk已经可以足够编写任何数据处理程序了，但使用函数可以更加简化。
函数可定义在程序顶层的任何位置：成对的“模式/操作组“之前、之间、之后。在单一文件的程序里，惯例是将所有函数在成对的模式/操作码之后，且让它们依字母顺序排列。
函数定义：
```
  function name(arg1, arg2, ..., argn)
  {
   statement(s);
  }
```
指定的参数在函数体中用来当作局部变量，会隐藏任何相同名称的全局性变量。
和C语言一样，标量是传值，而数组是传引用。
 
awk允许在被调用函数中的参数比函数定义里所声明的参数还要少，额外的参数会被视为局部变量。这类变量一般都用得到，所以惯例上是将它们列到函数参数列表里，并在字首前置一些额外的空白。这些额外的参数和局部变量一样，初始化为空字符串。
gawk通过--dump-variables选项检查是否能成功将局部变量列为额外的函数参数。
 
函数定义举例：
```bash
 function find_key(array, value, key)
 {
  #find array[] value,if array[key] == value,thus return key or ""
  
  for (key in array)
   if (array[key] == value)
    return key
  
  return ""
 }
```
awk语言支持递归。
 

 
# 十、字符串函数

函数substr(string, start, len)，返回一份由string的start字符开始，共len个字符长度的字符串副本。字符的位置从1开始编号，len省略的时候默认输出剩余。
gawk的--lint选项可以检查len长度有没有超出范围。
 
函数tolower（string）返回小写副本；函数toupper（string）返回大写副本。
 
函数index（string，find）查找string里面是否有字符串find，返回string里find字符串的起始位置，否则返回0。
 
函数match（string，regexp）将string与正则表达式regexp匹配，如果匹配，则返回匹配string的索引，否则返回0。如果匹配，会将全局变量RSTART设置为string开始匹配的索引值，而将RLENGTH设置为匹配的长度。可以substr(string,RSTART,RLENGTH)获取匹配后的剩余部分。
 
函数sub（regexp，replacement，target）将target与正则表达式regexp进行匹配，将最左边最长的匹配部分替换为字符串replacement。
函数gsub（regexp，replacement，target）则会替换所有匹配的字符串。
这两个函数的target参数默认是$0.
 
函数split（string，array，regexp）将string分割为片段，并存储倒array中的连续元素。在数组里，片段放置在匹配正则表达式regexp的子字符串之间。如果省略regexp的话，则使用内建字段分割字符FS的当前默认值。返回array数组元素数量。
即在string中根据regexp为匹配正则表达式的内容作为分隔符，分割后存入数组array中。
比如split("",array)可以快速删除数组元素；split(string,array,"")快速分割string为单个字符存入数组array。
 
一个关于字符串重建的函数join：
```bash
 function join(array, n, fs,  k,s)
 {
  #join array[1]...array[n] as string,fs is the FS var
  if (n >= 1)
  {
   s = array[1];
   for (k=2; k <= n; k++)
    s = s fs array[k]
  }
  return (s)
 }
```
 
字符串格式化的函数sprintf(format,expression1,expression2,...)返回已格式化的字符串为其函数值。
当浮点数出现在print语句里时，awk会根据内建变量OFMT的值格式化它们，OFMT的默认值是“%.6g“，可重新定义。
当浮点数转换为连续字符串时，awk会根据内建变量CONVFMT的值来格式化，默认值是”%.6g“。

```bash
print(string)                                                            打印字符串

split（string，array，regexp）                               根据regexp拆分数据存入数组

sprintf(format,expression1,expression2,...)              返回格式化字符串

sub（regexp，replacement，target）                    替换target中匹配regexp部分

gsub（regexp，replacement，target）                  同上

match（string，regexp）                                       返回匹配索引或0

substr(string, start, len)                                          返回从start开始长len的串

index（string，find）                                             返回find第一次出现位置或0

tolower（string）                                                   返回小写形式

toupper（string）                                                  返回大写形式

length(string)                                                         返回字符长度

blength(string)                                                                   返回位长度

close（cmd）                                                                    关闭文件或管道

system（cmd）                                                                执行外部程序
```

 
 
# 十一、数值函数

```bash
 <function>     <info>
 atan2(y,x)     返回y/x的反正切，值介于-pi～+pi
 cos(x)      返回x的余弦值，介于-1～+1
 exp(x)      返回x的指数
 int(x)      返回x的整数部分，截去前置的0
 log(x)      返回x的自然对数
 rand()      返回平均分布的虚拟随机r，0<= r < 1
 sin(x)      返回x的正弦值，介于-1～+1
 sqrt(x)      返回x的平方
 srand(x)     设置虚拟随机产生器的种子为x，并返回正确的种子。如果省略x，则使用当前时间。
 ```
       
       
笔记写到这里，基本是摘抄，在感叹awk语言的强大同时，也为自己所学知识贫乏而困扰不已。
UNIX系统中的工具个个都有自己的独特用处、专门领域，而我平时使用的却不过是九牛之一毛。很多时候，我是自己用不是很熟练的语言写一个功能不是很强大、复用性不强的代码完成任务，而忘记了“不要重复发明一个轮子”的道理。我应该把更多的时间花费在思考上，而不是在工具上耽误功夫。花尽量少的时间，掌握强大的工具，更有效率的完成工作，然后多化时间如何提高效率。



(预留补充awk脚本实例）：
1、update.awk:
```bash
BEGIN{
FS="\t";
}
function month(asofdate){
if(asofdate == "asofdate")
 return asofdate
 
 year=0+substr(asofdate,1,4);
 mon=substr(asofdate,5,2);
 if(mon == "01"){
 mon="12";
 year=year-1;
 }
 else if(mon == "12"){
 mon="11";
 }
 else if(mon == "11"){
 mon="10";
 }
 else if(mon == "10"){
 mon="09";
 }
    else{
 it=0+substr(mon,2,1); 
 mon=mon-1;
 mon="0"+mon;
    }
    return ""year""mon"01";
}
{
    if($4 != "20060101"){
 for(x=1;x<=3;x++)
     printf($x"\t");
 printf(month($4)"\t");
 for(x=5;x<NF;x++)
     printf($x"\t");
 printf($NF"\n");
    }
}
```
shebang语句：#!/usr/bin/awk -f ，注意结尾是一个空格，否则会除错。
