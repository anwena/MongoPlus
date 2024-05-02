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

### Introduction:

#### 🔥🔥🔥Use MyBatisPlus to elegantly operate MongoDB

* #### Sneak into the night with the wind
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Can be used in combination with the existing Mongo framework, non-invasive
* #### Simplify development
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;With simple configuration, CRUD operations can be performed quickly, and chain calls are more convenient

### Advantages:

* Non-invasive: Only enhancements are made without changes. Introducing it will not affect the existing project, as smooth as silk
* Low loss: Basics will be automatically injected upon startup CURD, basically no performance loss, direct object-oriented operation
* Powerful CRUD operation: General Service, most CRUD operations of a single table can be implemented with only a small amount of configuration, and there is a powerful conditional constructor to meet various usage needs
* Support Lambda form call: Through Lambda expressions, it is convenient to write various query conditions, no need to worry about the wrong field
* Support automatic generation of primary keys: Support up to 5 primary key strategies (including distributed unique ID generator - Sequence), which can be freely configured to perfectly solve the primary key problem
* Support operations without entity classes

### Rapid development

MyBatisPlus does not need to write too many SQL statements like MyBatis, and the same is true for Mongo-Plus!

### The project has been connected to Murphy Security
##### Click to view the detection report
[![Security Status](https://www.murphysec.com/platform3/v31/badge/1775074551634931712.svg)](https://www.murphysec.com/console/report/1775074551597182976/1775074551634931712)

### Update log
* v1.0 Basic usage</br>
* v2.0 Fix 1.0BUG</br>
* v2.0.1 Optimize query efficiency, add ID generation strategy, optimize the use with MongoTemplate, and reconstruct the chain call structure</br>
* v2.0.2 Fix the decoder problem when adding, deleting and modifying, and add missing conditions</br>
* v2.0.3 Add sql log printing, add missing conditions, add operations without entity classes (MongoPlusOperate) </br>
* v2.0.4 Fix the total number of rows displayed in paging, add count() function operations</br>
* v2.0.4.1 Fix the total number of rows displayed in paging as all, add projection operations, modify log configuration, modify MongoPlusOperate add, delete and modify operations
* v2.0.4.2 Add auto-increment id, modify the connection opened twice, modify the connection string splicing problem when cluster configuration is modified
* v2.0.5 Add pipeline function implementation, add auto-increment id strategy, modify entity class mapping relationship
* v2.0.6 Fix known issues, add physical operations, add support for spring3, add support for solon framework, add pipeline function options, modify structure
* v2.0.6.1 Fix dependency introduction problem
* No longer update here Update log
### Document
Document address: https://www.mongoplus.cn/

### Contact us
#### If you have any questions or ideas when using MongoPlus, please contact me
#### v: anwen_529
#### Note: MongoPlus
<img src="wx.png" alt="微信">

### Contribute

1. Star and Fork this repository

2. Create a new Feat_xxx branch

3. Submit code

4. Create a new Pull Request