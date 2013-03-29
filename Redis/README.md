# Redis: NoSQL Database

Redis是一个开源的使用ANSI C语言编写、支持网络、可基于内存亦可持久化的日志型、Key-Value数据库，并提供多种语言的API。从2010年3月15日起，Redis的开发工作由VMware主持。

# 数据模型

作为Key-value型数据库，Redis也提供了键（Key）和键值（Value）的映射关系。但是，除了常规的数值或字符串，Redis的键值还可以是以下形式之一：
* Lists （列表）
* Sets （集合）
* Sorted sets （有序集合）
* Hashes （哈希表）

键值的数据类型决定了该键值支持的操作。Redis支持诸如列表、集合或有序集合的交集、并集、差集等高级原子操作；同时，如果键值的类型是普通数字，Redis则提供自增等原子操作。

# 持久化

通常，Redis将数据存储于内存中，或被配置为使用虚拟内存。[3]通过两种方式可以实现数据持久化：使用快照的方式，将内存中的数据不断写入磁盘；或使用类似MySQL的日志方式，记录每次更新的日志。前者性能较高，但是可能会引起一定程度的数据丢失；后者相反。

# 主从同步

Redis支持将数据同步到多台从库上，这种特性对提高读取性能非常有益。

# 性能

相比需要依赖磁盘记录每个更新的数据库，基于内存的特性无疑给Redis带来了非常优秀的性能。读写操作之间有显著的性能差异。

# 资料

* python-redis源码https://github.com/andymccurdy/redis-py
* redisco纯python实现https://github.com/andymccurdy/redis-py
* 文档http://redis-py.readthedocs.org/en/latest/
* 一个说明http://redis-py.readthedocs.org/en/latest/
* 中文文档http://redis.cn/
* 一个博客http://www.litrin.net/tag/redis/
* redis应用场景http://blog.csdn.net/xymyeah/article/details/6578422
* 作者博客http://antirez.com/latest/0
