#!/usr/bin/env perl
#-*- perl -*-

# 写一个程序，计算在半径12.5时，圆的周长应该是多少？圆周长是半径的长度乘以2pi（大约是2乘以3.141592654）。计算结果大约是78.5

# 

$r=12.5;
$pi=3.141592654;
$sum=2 * $r * $pi;


print "计算圆的周长\n";
print "半径: ",$r,"\n";
print "周长: ",$sum,"\n\n";


print "根据输入半径，计算圆周长\n";
$i=1;
while($i <= 2){
    $new_r = <STDIN>;

    if($new_r < 0){
	$new_sum = 0;
    } else {
	$new_sum = 2 * $new_r * $pi;
    }
    
    print "半径: ",$new_r;
    print "周长: ",$new_sum,"\n";
    $i += 1;
}


print "输入长、宽，计算面积\n";
print "长：";
$length = <STDIN>;
print "宽：";
$width = <STDIN>;
print "面积：",$length * $width,"\n";

print "输入个数n、内容text，则打印n遍text\n";
chomp($num = <STDIN>);
$text = <STDIN>;


#print $num;
while($num > 0){
    print $text;
    $num -= 1;
};

print "注意：print (2+4)*5 和 print 5*(2+4)结果不一样\n";
