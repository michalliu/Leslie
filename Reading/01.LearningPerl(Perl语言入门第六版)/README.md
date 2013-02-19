# Perl语言入门

![第六版](./image/LearningPerl.jpg)

# Perl介绍

Perl借取了C、sed、awk、shell scripting以及很多其他程序语言的特性。其中最重要的特性是它内部集成了正则表达式的功能，以及巨大的第三方代码库CPAN。简而言之，Perl象C一样强大，象awk、sed等脚本描述语言一样方便。

Perl被广泛地认为是一种工业级的强大工具，可以在任何地方用来完成你的工作。perl的前身是Unix系统管理的一个工具，被用在无数的小任务里。后逐渐发展成为一种功能强大的程序设计语言，用作Web编程、数据库处理、XML处理以及系统管理；在完成这些工作时，同时仍能处理日常细小工作，这是它的设计初衷。

Perl之所以强大, 是因为有CPAN, CPAN上面有无数的开源模块, 从科学计算到桌面应用到网络等等各个方面都有大量的模块! 并且现在世界上也还有无数的人在向上面添加模块! 如果你想要用PERL实现某功能, 不用自己做, 在CPAN上面搜一搜, 多半都会得到已有的结果! CPAN（"the Comprehensive Perl Archive Network"全面的 Perl 存档网络）是查找任何 Perl 有关的东西的中心仓库。它包含从整个 Perl 社区收集来的智慧：成百上千的 Perl 模块和脚本，相当于好几本书的文档，以及整个 Perl 发布。如果有东西是用 Perl 写的，而且这个东西很有用而且是自由的，那么它很有可能就在 CPAN 上。

# 编程范型： 多重编程范式

多重编程范式（Multi-paradigm programming language）是一种可以支持超过一种编程范型的编程语言。“Multi-paradigm”这个词是由Bjarne Stroustrup博士在其著作中提出的，用于表述 C++ 可以以同时使用多种风格来写程序，比如面向对象和泛型编程。

## 两种模态的语言

### 可视化、数据流
#### LabVIEW
### 函数式、逻辑
#### ALF
### 面向对象，函数式
#### Dylan
#### Sather
#### Claire
### 函数式，可视化
#### Spreadsheet
### 面向对象（基于类），可视化
#### Lava

## 十一种模态的编程语言


### 并发编程，约束编程, 数据流编程, 声明性编程, 分布式的编程，函数式编程，泛型编程，命令式编程，逻辑编程，元编程，面向对象编程

# Perl语言的中心思想

Perl语言的中心思想可以集成为一句话“TMTOWTDI”：

```
There's More Than One Way To Do It.
（不只一种方法来做一件事。）
```

Perl的作者拉里·沃尔建议可以把这个缩写词念成“Tim Toady”。这句话后来被扩充成:
```
There's more than one way to do it, but sometimes consistency is not a bad thing either.
（不只一种方法来做一件事，但有时保持一致也不错。）
TIMTOWTDIBSCINABTE，发音为“Tim Toady Bicarbonate”.
```

另一个Perl程序员常常想起的Perl俗语是：
```
Easy things should be easy, and hard things should be possible.
（简单的事情应该是简单的，复杂的事情应该变得可能。）
```

# 缺点

也正是因为Perl的灵活性和“过度”的冗余语法，也因此获得了write-only的“美誉”，因为许多Perl程序的代码令人难以阅读，实现相同功能的程序代码长度可以相差十倍百倍。但Perl同样可以将代码书写得像Python或Ruby等语言一样优雅。

# 基本定义

* 变量定义，以$号开头，如：$num =1;
* 数组定义，以@开头，如：@array = (1,2,3);
* 数组元素调用 $array[index]，其中index表示数组下标，如上例，$array[0]的值是1
* 散列定义，以%开头，如：%hash=("a",1,"b",2);
* 散列调用 %hash，其中keys表示键值，多用字符串表示，注意hash的key必须具有唯一性，但value可以不唯一，为此hash的key经常被用来做唯一化处理，如上例中的"a", "b", values是keys对应的值，如1，2。$hash{"b"}的值是2。



# 记录

* 公司出资，员工购书，吾购得《Prel语言入门》。 —— 02/18/2013
