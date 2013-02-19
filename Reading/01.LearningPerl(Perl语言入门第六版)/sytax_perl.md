# 判断语句

因为Perl大量的创造冗余的语法，并且从其他语言中大量的借鉴语法，使得Perl的语法极其丰富和灵活。Perl共有下列几种判断语句：

## if区块

```perl
if ($hour > 22) {
   print "should sleep...\n";
}
```

## if语句

```perl
print "hello" if $guest >= 1;
```

## 	unless区块

```perl
unless ($credit > 100) {
   print "You can not graduate!\n";
}
```
		
## unless语句

```
print "eat\n" unless $food == 0;
```

## given/when(语句及区块)

```perl
use 5.010;
given ($foo)
{
    say 'a' when 'a';
	when (/b/) { say 'b'; } #when可以写成语句或区块
	default { say 'not match'; } #只可以写成区块。
}
```

由于逻辑运算对象的作用，还可以写出不用关键字if或unless的判断语句，如常用的打开文件语句：

```perl
open DATA, '<', $filename or die "Can't open $filename: $!\n";
```

# 循环语句

Perl中的循环语句也是非常的丰富。主要有：

## Perl自己的for或者foreach循环(两个完全一样)：

```perl
@group = 1 .. 10;
for (@group) {
    print "$_\n";
}

print "$_\n" for @group;

foreach (@group) {
	    print "$_\n";
}
```

## 从C语言借鉴来的for循环(又可写作foreach，两个完全一样)：

```perl
for ($i = 0; $i < 10; $i++) {
    print "$group[$i]\n";
}
```

## while循环：

```perl
$i=0;
while ($i < 10) {
    print "$group[$i]\n";
    $i++;
}
```

## do...while循环：

```perl
$i = 0;
do {
    print "$group[$i]\n";
    $i++;
} while ($i < 10);
```

## until循环：

```perl
$i = 0;
until ($i == 10) {
    print "$group[$i]\n";
    $i++;
}
```

## 从PASCAL语言借鉴来的do...until循环：

```perl
$i = 0;
do {
    print "$group[$i]\n";
    $i++;
} until ($i == 10);
```


## 甚至还有利用动态语言特性，用map函数也可以做循环：

```perl
map { print "$_\n" } @group;
```

## 其实还有其他的循环方式。总而言之，就是TMTOWTDI。
