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

