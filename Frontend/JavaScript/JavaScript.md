# JavaScript

JavaScript，也称ECMAScript，是一种广泛用于客户端网页开发的脚本语言，最早是在HTML上使用的，用来给HTML网页添加动态功能，然而现在JavaScript也可被用于网络服务器，如Node.js。

一般来说，完整的JavaScript包括以下几个部分：
* ECMAScript，描述了该语言的语法和基本对象
* 文档对象模型（DOM），描述处理网页内容的方法和接口
* 浏览器对象模型（BOM），描述与浏览器进行交互的方法和接口

它的基本特点如下：
* 是一种解释性脚本语言（代码不进行预编译）。
* 主要用来向 HTML 页面添加交互行为。
* 可以直接嵌入 HTML 页面，但写成单独的js文件有利于结构和行为的分离。

JavaScript常用来完成以下任务：
* 嵌入动态文本于HTML页面
* 对浏览器事件作出响应
* 读写HTML元素
* 在数据被提交到服务器之前验证数据
* 检测访客的浏览器信息
* 控制cookies，包括创建和修改等


不同于服务器端脚本语言，例如PHP与ASP，JavaScript主要被作为客户端脚本语言在用户的浏览器上运行，不需要服务器的支持。所以在早期程序员比较青睐于JavaScript以减少对服务器的负担，而与此同时也带来另一个问题：安全性。而随着服务器的强壮，虽然现在的程序员更喜欢运行于服务端的脚本以保证安全，但JavaScript仍然以其跨平台、容易上手等优势大行其道。同时，有些特殊功能（如AJAX）必须依赖Javascript在客户端进行支持。随着引擎如V8和框架如Node.js的发展，及其事件驱动及异步IO等特性，JavaScript逐渐被用来编写服务器端程序。

# 编程

JavaScript是一种脚本语言，其源代码在发往客户端运行之前不需经过编译，而是将文本格式的字符代码发送给浏览器由浏览器解释运行。直译语言的弱点是安全性较差，而且在JavaScript中，如果一条运行不了，那么下面的语言也无法运行。而其解决办法就是于使用try{}catch(){}︰
```javascript
console.log("a");    //这是正确的
console.log("b");    //这是正确的
console.logg("c");   //这是错误的，并且到这里会停下来
console.log("d");    //这是正确的
console.log("e");    //这是正确的
 
 /*解决办法*/
 try{console.log("a");}catch(e){}    //这是正确的
 try{console.log("b");}catch(e){}    //这是正确的
 try{console.logg("c");}catch(e){}   //这是错误的，但是到这里不会停下来，而是跳过
 try{console.log("d");}catch(e){}    //这是正确的
 try{console.log("e");}catch(e){}    //这是正确的
```

# Hello World

```javascript
<!DOCTYPE HTML>
<html>
    <head>
    <title>简单的JavaScript Hello World</title>
    <script type="text/javascript">
    document.write("Hello, world!");   // 在浏览器视窗内直接显示
    alert("Hello, world!");            //  弹窗显示
    console.log("Hello, world!");      // 在控制台(console)里显示，需要先开启开发工具控制台
    </script>
    </head>
    <body>
    HTML 内容……
    </body>
</html>
```

或是在浏览器的地址栏（location bar）中使用javascript:，以交互方式表示：

```javascript
javascript:alert("Hello world!");
```

# JavaScript引擎

## Mozilla
* SpiderMonkey，第一款JavaScript引擎，由Brendan Eich在Netscape Communications时编写，用于Mozilla Firefox 1.0～3.0版本。
* Rhino，由Mozilla基金会管理，开放源代码，完全以Java编写。
* TraceMonkey，基于实时编译的引擎，其中部份代码取自Tamarin引擎，用于Mozilla Firefox 3.5～3.6版本。
* JaegerMonkey，德文Jäger原意为猎人，结合追踪和组合码技术大幅提高性能，部分技术借凿了V8、JavaScriptCore、WebKit，用于Mozilla Firefox 4.0以上版本。
* IonMonkey，可以对JavaScript编译后的结果进行优化，用于Mozilla Firefox 18.0以上版本。

## Google
* V8，开放源代码，由Google丹麦开发，是Google Chrome的一部分。

## 微软 
* Chakra (JScript引擎)，中文译名为查克拉，用于Internet Explorer 9的32位版本[1]及Internet Explorer 10。

## Opera
* Linear A，用于Opera 4.0～6.1版本。
* Linear B，用于Opera 7.0～9.2版本。
* Futhark，用于Opera 9.5～10.2版本。
* Carakan，由Opera软件公司编写，自Opera10.50版本开始使用。

## 其它 
* KJS，KDE的ECMAScript/JavaScript引擎，最初由Harri Porten开发，用于KDE项目的Konqueror网页浏览器中。
* Narcissus，开放源代码，由Brendan Eich编写（他也参与编写了第一个SpiderMonkey）。
* Tamarin，由Adobe Labs编写，Flash Player 9所使用的引擎。
* Nitro（原名SquirrelFish），为Safari 4编写。

# V8 (JavaScript引擎)

V8是一个由丹麦Google开发的开源JavaScript引擎，用于Google Chrome中。Lars Bak是这个项目的组长。

V8在运行之前将JavaScript编译成了机器码，而非字节码或是直译它，以此提升性能。更进一步，使用了如内联缓存（inline caching）等方法来提高性能。有了这些功能，JavaScript程序与V8引擎的速度媲美二进制编译。

传统的javascript是动态语言, 又可称之为 Prototype-based Language，JavaScript继承方法是使用prototype, 通过指定prototype属性，便可以指定要继承的目标。属性可以在运行时添加到或从对象中删除，引擎会为运行中的对象创建一个属性字典，新的属性都要通过字典查找属性在内存中的位置。V8为object新增属性的时候，就以上次的hidden class为父类别，创建新属性的hidden class的子类别，如此一来属性访问不再需要动态字典查找了。

为了缩短由垃圾收集造成的停顿，V8 使用stop-the-world, generational, accurate 的垃圾收集器。在执行回收之时会暂时中断程序的执行，而且只处理对象堆栈。还会收集內存内所有对象的指针，可以避免内存溢出的情况。V8汇编器是基于Strongtalk汇编器。

