<p align="center">
  <a href="https://gitee.com/anwena/mongo-plus/blob/master/LICENSE">
    <img src="https://img.shields.io/hexpm/l/plug.svg" alt="License">
  </a>
<a href="https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html">
	<img src="https://img.shields.io/badge/JDK-8-green.svg" alt="jdk-8" />
</a>
<a target="_blank" href="https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html">
	<img src="https://img.shields.io/badge/JDK-17-green.svg" alt="jdk-17" />
</a>
<a href='https://gitee.com/aizuda/mongo-plus/stargazers'>
  <img src='https://gitee.com/aizuda/mongo-plus/badge/star.svg?theme=gvp' alt='star'/>
</a>
<a href='https://gitee.com/aizuda/mongo-plus/members'>
  <img src='https://gitee.com/aizuda/mongo-plus/badge/fork.svg?theme=gvp' alt='fork'/>
</a>
</p>
<p style="text-align: center;">
<img style="width: 200px;display: inline-block;" src="logo.png" alt="MongoPlusLogo">
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
* 支持主键自动生成：支持多达 5 种主键策略（内含分布式唯一 ID 生成器 - Sequence），可自由配置，完美解决主键问题
* 支持无实体类情况下的操作
* 支持动态数据源
* 支持逻辑删除、防止全集合更新和删除、自动填充等等功能

### 快速开发

MyBatisPlus就是可以不用像MyBatis一样写过多的sql语句，Mongo-Plus也一样！

### 项目已接入墨菲安全
##### 点击查看检测报告
[![Security Status](https://www.murphysec.com/platform3/v31/badge/1775074551634931712.svg)](https://www.murphysec.com/console/report/1775074551597182976/1775074551634931712)

###   更新日志
* v1.0      基础用法</br>
* v2.0      修复1.0BUG</br>
* v2.0.1    优化查询效率，增加ID生成策略，优化和MongoTemplate一起使用问题，重构链式调用结构</br>
* v2.0.2    修复增删改时的解码器问题，增加遗漏条件</br>
* v2.0.3    增加sql日志打印，增加遗漏条件，增加无实体类情况下的操作（MongoPlusOperate）</br>
* v2.0.4    修复分页的总行数显示数量问题，增加count()函数的操作</br>
* v2.0.4.1  修复分页总行数显示为所有，增加projection操作，修改日志配置，修改MongoPlusOperate增删改操作
* v2.0.4.2  增加自增id，修改打开两次连接，修改集群配置时，连接字符串拼接问题
* v2.0.5    增加管道函数实现，增加自增id策略，修改实体类映射关系
* v2.0.6    修复已知问题，增加实物操作，增加对spring3的支持，增加对solon框架的支持，增加管道函数的options，修改结构
* v2.0.6.1  修复依赖引入问题
* 不再此处更新 更新日志
### 文档
文档地址： https://www.mongoplus.com/

### 联系我们
#### 在使用MongoPlus的过程中，有任何问题和想法或者加入群聊请联系我
#### v：anwen_529
#### 备注：MongoPlus
<img src="wx.png" alt="微信">

###  参与贡献

1.  Star and Fork 本仓库
2.  新建 Feat_xxx 分支
3.  提交代码
4.  新建 Pull Request
