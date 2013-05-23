# Node模块

Node中每一个JavaScript文件就是一个模块，一如python模块。

```javascript
var fs = require('fs');
```

require函数会搜索模块，然后将模块的定义载入Node的执行环境，从而让对应的函数可以供外部调用。

比如模块simple.js:
```javascript
var count = 0;
exports.next = function(){ return count++;}
```

这个模块定义了一个对外开放的函数和一个局部变量。

使用方法：

```javascript
require('./simple')
```
返回的对象是在simple.js中定义的exports。

require的机制就是exports的所有成员（函数、对象等）都会暴露给模块外的代码，而那些没有指定在exports下的对象，在模块外部是不可见的。

```bash
$ node
> require('./simple')
{ next: [Function] }
> var s = require('./simple')
undefined
> s.next()
0
> s.next()
1
> s.next()
2
> s.next()
3
> 
```


# 解析require('module')的工作方式

在Node中，模块存储在文件中，每个文件仅仅存储一个模块。

## 模块标识符和路径名

一般模块名就是一个没有文件后缀的路径名，如require('./simple')会自动找到并加载simple.js文件。

Node也支持二进制原生语言库，并可以作为Node模块（文件后缀是.node)。

Node模块还可以不以文件形式存储，而是被编译成Node可执行文件，这些模块是核心模块，nodejs.org文档已经列出。这些模块原本是以源文件形式存在，但构建进程将其编译成二进制Node可执行文件（不就是源码编译安装嘛:()。

Node有3中定义模块的方式：
* 相对路径的定义方式（以`./`或者`../`开头的路径）
* 绝对路径的定义方式（以`/`开头的路径）
* 顶级目录的定义方式（以模块名开头）

对于顶级目录以模块名开头，这些模块存储于某个目录中，比如node_modules或者其它require.paths中的目录。

## Node应用里的本地模块

应用通常都会都有一个专属的目录结构，遵从这目录结构的模块文件按序放置在源代码控制系统中，然后会部署到源服务器上。（就是说一个应用就是一个顶级目录，所以用到的东西按照一定的习惯存储在该目录下)。

## 绑定应用的外部依赖

引用node_modules目录中的模块必须使用顶级目录标识符：
```javascript
var express = require('express')
```

Node会寻找模块时搜索node_modules目录，Node里含有不止一个node\_modules目录。Node会从当前模块的目录开始，先把node\_modules目录添加进去，然后搜索当前目录下的node\_modules。如果在当前node\_modules目录下没有找到相应的模块（不会递归查询子目录，只查询第一级子目录），Node就会转向父级目录继续查找，直到根目录。

如果想用Express框架，可以在应用目录中创建node_modules目录，然后把Express框架安装到该目录。


Node会层层向上搜索node\_modules目录，然后在第一次找到需要模块的所在目录停止。因此，在维护多个版本冲突的Node包时，node_modules目录很关键。


## require.paths目录下的系统级模块（内置模块）

NODE_PATH环境变量可以添加目录到require.paths数组中:
```bash
$ export NODE_PATH=/usr/lib/node
```

但requier.paths似乎被移除了，最好是将目录放到node_modules目录中：

```bash
> require.paths;
Error: require.paths is removed. Use node_modules folders, or the NODE_PATH environment variable instead.
   at Function.Object.defineProperty.get (module.js:388:11)
   at repl:1:8
   at REPLServer.self.eval (repl.js:110:21)
   at repl.js:249:20
   at REPLServer.self.eval (repl.js:122:7)
   at Interface.<anonymous> (repl.js:239:12)
   at Interface.EventEmitter.emit (events.js:95:17)
   at Interface._onLine (readline.js:202:10)
   at Interface._line (readline.js:531:8)
   at Interface._ttyWrite (readline.js:754:14)
```

## 复杂模块（作为目录的模块）

对于复杂模块，需要一个index.js或者package.json文件放到一个目录中，以满足require('moduleName')的需求.

package.json:
```javascript
{ name: "myLibrary",
  main: "./lib/simple.js"}
```

有了package.json文件，命令require('myLibrary')就会加载文件/path/to/node_modules/myLibrary/lib/simple.js

如果没有package.json文件，Node会查找index.js，从而加载/path/to/node_modules/myLibrary/index.js

# Node包管理器（npm）

npm为Node定义的包格式大部分基于CommonJS包规范。

## npm包的格式

一个npm包就是一个包含了package.json的文件夹，package.json描述了这个文件夹的结构。

访问package.json文档：
```bash
$ npm help json
```

一个基本的package.json文件：
```javascript
{ name: "packageName",
  version: "1.0",
  main: "mainModuleName",
  modules: {
        "mod1": "lib/mod1",
        "mod2": "lib/mod2"
        }
  }
```

通过命令检查包名是否已被使用：
```bash
$ npm search packagename
```

Node包以tar-gzip格式打包。

npm包可以定义和其它包的依赖关系，从而自动安装:
```javascript
"dependencies":
  { "foo": "1.0.0 - 2.9999.9999",
    "bar": ">=1.0.2 < 2.1.2"
  }
```

其它描述信息：
```javascript
"description": "My fist package",
"homepage": "www.github.com/LeslieZhu",
"author": xxxx@yyy.org 
```

bin标签是命令名到实现该命令的脚本映射:
```javascript
bin: {
  "nodeload.js" : "./nodeload.js",
  "nl.js": "./nl.js"
  }
```

directories标签用于记录包的目录结构，lib目录中的文件会被自动扫描并加载:
```javascript
directories:{
   lib: './lib',
   bin: './bin'
   }
```

## 查找npm包

默认npm模块从http://npmjs.org 安装包获取，只要知道模块名字:

```bash
$ npm search modulesname
$ npm install modulesname
$ npm view modulesname
$ npm view modulesname pagename
$ npm view zombie homepage
```

## 使用npm命令

### 获取npm的帮助

```bash
$ npm help <command>
```

### 查看包信息

npm view命令会把package.json文件当作数据，能够找到使用点标记法记录的JSON标签。

```bash
$ npm view openid repository.url
$ npm view openid engines
```

### npm包的安装

```bash
$ npm install openid
```

npm的安装分为全局模式和本地模式。一般情况下会以本地模式运行，包会被按照到和应用代码同级的本地node_modules目录。在全局模式下，Node包会被安装到Node的安装目录下(requie.paths中的目录），而不是当前文件的子文件夹node\_modules中。

```bash
$ npm install -g openid
```


也可以通过修改npm配置设置实现：

```bash
$ npm set global=true
$ npm get global
```

### 使用已安装的node 包

```javascript
var openid = require('openid');
```

安装的包的子目录也可以被引用:

```javascript
var base64 = require('openid/lib/base64').base64;
```

### 查看已安装的node包

list命令只是list当前目录的node_modules，不同目录运行结果不一样。

```bash
$ npm list
/path/to/
├── UNMET DEPENDENCY app.js *
├── UNMET DEPENDENCY ls.js *
└── UNMET DEPENDENCY simple.js *
npm ERR! missing: app.js@*, required by undefined@undefined
npm ERR! missing: ls.js@*, required by undefined@undefined
npm ERR! missing: simple.js@*, required by undefined@undefined
npm ERR! not ok code 0
```

```bash
$ npm set parseable=true
$ npm list （将列出各个模块的绝对路径）
```


### Node包脚本

一个node包可以这样测试:

```bash
$ npm test <packagename>
```

### 查看和编辑node包内容

```bash
$ npm explore <packagename>
$ npm rebuild <packagename>
```

### 更新包


检查是否过时：

```bash
$ npm outdated
$ npm update <packagename>
```

### 卸载

```bash
$ npm uninstall <packagename>
```

### 开发和发布npm包

* 第一步：使用npm init创建package.json文件

* 第二步：创建包源文件

每次更新了源文件，都要更新package.json文件，但npm有方式自动更新。

** 命令一：npm link

npm link只是设置了一个连接到源文件目录的符号链接，然后可以自由编辑包文件，而不需要在包有变化的时候重新打包和更新。

首先，将自己的项目链接到Node安装程序上

```bash
$ cd <your dir>
$ npm link
```

然后，将Node包链接到应用中

```bash
$ npm link <packagename>
```

* 第三步：注册npm帐号并发布包

```bash
$ npm adduser
$ npm publish
$ npm unpublish
```

### npm的配置

```bash
$ npm config set <key> <value> [--global]
$ npm config get <key>
$ npm config delete <key>
$ npm config list
$ npm config edit
$ npm get <key>
$ npm set <key> <value> [--global]
```

所有以NPM_CONFIG开头的环境变量都是npm的配置项。
```bash
~/.npmrc
/path/to/etc/npmrc
```

## Node包版本的标识和范围

npm使用Semantic Versioning模式来识别版本号:
* 版本字符串一般是X.Y.Z，其中X是主版本，Y是副版本，Z是日常补丁版本
* 版本字符串的补丁号后可以有一个任意的文本，叫“特别版本”，如1.2.3beta1
* 版本字符串的比较不是一般的字符比较，而是一种数值比较，如1.9.0 < 1.10.0 < 1.11.3, 1.0.0beta1 < 1.0.0beta2 < 1.0.0

用版本号记录兼容性：
* X为0的版本是不文档的，任何API随时会改变
* 如果只是做向前兼容的bug修复，Z的值（补丁号）必须递增
* 如果引入了向前兼容的函数（功能），Y值必须递增
* 当和之前版本有冲突的情况，X的值必须递增

## CommonJS模块

模块提供一个简单的封装机制去隐藏本身的实现方式，而暴露出一个API。模块内容是JavaScript代码，如：

```bash
(function() { ... contents of module file ...})();
```

将模块的所有顶级对象隐藏在一个私有的命名空间中，使其它代码无法访问。

模块对外开放的API即是require函数返回的对象。在模块内部，这个API是由exports对象实现的，exports的字段包含被暴露的API。

如：
module1.js:
```javascript
var A = "value A";
var B = "value B";

exports.values = function(){
    return { A: A, B: B};
}
```

module2.js:
```javascript
var util = require('util');
var A = "a different value A";
var B = "a different value B";
var m1 = require('./module1');
util.log('A='+A+' B='+B+' value='+util.inspect(m1.values()));
```

```bash
$ node modules.js
23 May 23:15:07 - A=a different value A B=a different value B value={ A: 'value A', B: 'value B' }
```

