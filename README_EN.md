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
<a href='https://gitee.com/anwena/mongo-plus/stargazers'>
  <img src='https://gitee.com/anwena/mongo-plus/badge/star.svg?theme=dark' alt='star'/>
</a>
<a href='https://gitee.com/anwena/mongo-plus/members'>
  <img src='https://gitee.com/anwena/mongo-plus/badge/fork.svg?theme=dark' alt='fork'/>
</a>
</p>
<p style="text-align: center;">
<img style="width: 200px;display: inline-block;" src="logo.png" alt="MongoPlusLogo">
</p>

### Introduction：

#### 🔥🔥🔥Use MyBatisPlus to elegantly operate MongoDB

* #### Sailing with the Wind
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Can be used in conjunction with existing MongoDB frameworks without any impact
* #### Simplify Development
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Simple configuration enables quick CRUD operations, with convenient chain calling

### Advantages：

* Non-invasive: Only enhancements are made without changes. Introducing it will not affect the existing project, smooth as silk
* Low overhead: Automatically inject basic CURD upon startup, with almost no performance overhead, directly operating on objects
* Powerful CRUD operations: Generic Service can achieve most of the CRUD operations on a single table with only a small amount of configuration. It also provides a powerful condition builder to meet various needs
* Support Lambda expression invocation: Conveniently write various query conditions using Lambda expressions, no need to worry about incorrect field names
* Support automatic primary key generation: Supports up to 5 primary key strategies (including distributed unique ID generator - Sequence), freely configurable, perfectly solving the primary key issue
* Support operation without entity class

### Quick Development

Just like MyBatis, you don't need to write excessive SQL statements, and it's the same with Mongo-Plus!

###   Changelog
* v1.0     Basic usage</br>
* v2.0     Bug fixes for 1.0</br>
* v2.0.1   Optimize query efficiency, add ID generation strategy, optimize usage with MongoTemplate, refactor chain calling structure</br>
* v2.0.2   Fix decoder issues in insert, delete, and update operations, add missed conditions</br>
* v2.0.3   Add SQL log printing, add missed conditions, add operation without entity class (MongoPlusOperate)</br>
* v2.0.4   Fix the problem of displaying the total number of rows in pagination, add count() function operation</br>
* v2.0.4.1 Fix pagination showing all rows, add projection operation, modify log configuration, modify MongoPlusOperate insert, delete, and update operations
* v2.0.4.2 Add auto-increment ID, modify opening two connections, modify connection string concatenation when configuring clusters
* v2.0.5   Implement pipeline functions, add auto-increment ID strategy, modify entity mapping relationship
* v2.0.6   Fix known issues, add transactional operations, add support for Spring 3, add support for Solon framework, add options for pipeline functions, modify structure
* v2.0.6.1 Fix dependency import issues
### Documentation
Documentation： https://www.mongoplus.cn/

### Contact Us
If you encounter any problems, please contact me<br/>
v: JiaChaoYang_<br/>
<img src="wx.png" alt="WeChat">

###  Contribution

1.  Star and Fork this repository
2.  Create a new Feat_xxx branch
3.  Commit your code
4.  Create a new Pull Request
