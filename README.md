<p align="center">
  <a href="https://gitee.com/anwena/mongo-plus/blob/master/LICENSE"><img src="https://img.shields.io/hexpm/l/plug.svg" alt="License"></a>
<a target="_blank" href="https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html">
	<img src="https://img.shields.io/badge/JDK-8-green.svg" alt="jdk-8" />
</a>
</p>

### 介绍：

#### 🔥🔥🔥使用MyBatisPlus的方式，优雅的操作MongoDB

* #### 随风潜入夜
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;可和现有Mongo框架结合使用，无侵入性
* #### 简化开发
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;只需简单配置，即可快速进行CRUD操作，链式调用更加便捷

### 优点：

* 无侵入：只做增强不做改变，引入它不会对现有工程产生影响，如丝般顺滑
* 损耗小：启动即会自动注入基本 CURD，性能基本无损耗，直接面向对象操作
* 强大的 CRUD 操作：通用 Service，仅仅通过少量配置即可实现单表大部分 CRUD 操作，更有强大的条件构造器，满足各类使用需求
* 支持 Lambda 形式调用：通过 Lambda 表达式，方便的编写各类查询条件，无需再担心字段写错
* 支持主键自动生成：支持多达 4 种主键策略（内含分布式唯一 ID 生成器 - Sequence），可自由配置，完美解决主键问题
* 支持无实体类情况下的操作

### 快速开发

MyBatisPlus就是可以不用像MyBatis一样写过多的sql语句，Mongo-Plus也一样！

###   更新日志
* v1.0      基础用法</br>
* v2.0      修复1.0BUG</br>
* v2.0.1    优化查询效率，增加ID生成策略，优化和MongoTemplate一起使用问题，重构链式调用结构</br>
* v2.0.2    修复增删改时的解码器问题，增加遗漏条件</br>
* v2.0.3    增加sql日志打印，增加遗漏条件，增加无实体类情况下的操作（MongoPlusOperation）</br>
* v2.0.4    修复分页的总行数显示数量问题，增加count()函数的操作</br>

### 文档
文档地址： https://www.mongoplus.cn/

###  参与贡献

1.  Star and Fork 本仓库
2.  新建 Feat_xxx 分支
3.  提交代码
4.  新建 Pull Request