# 安装并配置Node

利用源码安装，需要一个C编译器（如GCC），需要python2.4以上版本，需要OpenSSL加密库。

下载源码，执行./configure;make && make install.

验证，运行node --help

```bash
$ node
> console.log('hello,wolrd');
```

# 运行脚本

脚本ls.js:
```javascripts
var fs = require('fs');
var files = fs.readdirSync('.');
for (fn in files){
    console.log(files[fn]);
}
```

$ node ls.js 模仿了UNIX里面的ls命令。

脚本参数通过全局数组process.argv传递

# 用Node启动服务器

app.js:
```javascript
var http = require('http');
http.createServer(function (req,res){
    res.writeHead(200,{'Content-Type': 'text/plain'});
    res.end('Hello, Wolrd!\n');
}).listen(8124,'127.0.0.1');
console.log('Server running at http://127.0.0.1:8124');
```

$ node app.js （然后访问http://127.0.0.1:8124)


这里的listen创建了一个活动的事件监听器进行事件循环。这个事件监听器会使app.js一直处于执行状态。

在ls.js中没有创建事件监听器，所以当脚本执行到末尾的时候，Node进程就会退出。

# npm包管理器

npm是一个非官方标准的Node包管理器，极大简化了下载和使用Node模块的流程。

```bash
$ git clone --recursive git://github.com/isaacs/npm.git 
$ cd npm 
$ node cli.js install npm -gf 
```

中间出现了一些错误，但暂时并不影响使用。

验证：
```bash
$ npm install underscore 

underscore@1.2.2 ./node_modules/underscore 
```

```bash
$ sudo /usr/local/bin/npm install -g hexy
npm http GET https://registry.npmjs.org/hexy
npm http 200 https://registry.npmjs.org/hexy
npm http GET https://registry.npmjs.org/hexy/-/hexy-0.2.5.tgz
npm http 200 https://registry.npmjs.org/hexy/-/hexy-0.2.5.tgz
/usr/local/bin/hexy -> /usr/local/lib/node_modules/hexy/bin/hexy_cmd.js
hexy@0.2.5 /usr/local/lib/node_modules/hexy

$ hexy --width 12 ../ls.js 
00000000: 7661 7220 6673 203d 2072 6571  var.fs.=.req
0000000c: 7569 7265 2827 6673 2729 3b0a  uire('fs');.
00000018: 7661 7220 6669 6c65 7320 3d20  var.files.=.
00000024: 6673 2e72 6561 6464 6972 5379  fs.readdirSy
00000030: 6e63 2827 2e27 293b 0a66 6f72  nc('.');.for
0000003c: 2028 666e 2069 6e20 6669 6c65  .(fn.in.file
00000048: 7329 7b0a 2020 2020 636f 6e73  s){.....cons
00000054: 6f6c 652e 6c6f 6728 6669 6c65  ole.log(file
00000060: 735b 666e 5d29 3b0a 7d0a       s[fn]);.}.
```
