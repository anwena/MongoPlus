<p align="center">
  <a href="https://gitee.com/anwena/mongo-plus/blob/master/LICENSE"><img src="https://img.shields.io/hexpm/l/plug.svg" alt="License"></a>
<a target="_blank" href="https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html">
	<img src="https://img.shields.io/badge/JDK-8-green.svg" alt="jdk-8" />
</a>
</p>


通过MyBatisPlus的方式，优雅的操作MongoDB


### 首先，你有一个实体类：

```java
import com.anwen.mongo.sql.model.BaseModelID;

/**
 * @TableName：配置对应的表名，不配置默认使用小写类名，通过dataSource属性切换数据源
 **/
@TableName(value="对应的表名",dataSource="配置多数据源时的slaveName"/*不配置dataSource默认使用主数据源*/)
public class User extends BaseModelID { 
  @TableField("user_name")//标识对应数据库中的字段
  private String username;
  private int status;
  private int age;
  private String gender;
  private Date joinDate;
  private int roleId;
  private String roleName;
}
```
### 将你的service接口继承IService

```java
import com.anwen.mongo.sql.IService;
import org.apache.catalina.User;
//如果和MyBatisPlus一起使用的话，只需要使用注入IService的方式就可以了
public interface MongoServiceImpl extends IService<User> {
        
}
```

### 再将你的service实现类继承ServiceImpl

```java
import com.anwen.mongo.config.MongoDBConnectProperty;
import com.anwen.mongo.sql.ServiceImpl;
import com.mongodb.MongoClient;
import org.apache.catalina.User;
//如果和MyBatisPlus一起使用的话，只需要使用注入IService的方式就可以了
public class MongoServiceImpl extends ServiceImpl<User> implements MongoService {
    
}
```

### 然后你就可以使用MyBatisPlus一模一样的代码操作mongodb啦：

```java
import org.apache.catalina.User;

@RestController
@RequestMapping("/user")
public class UserController {

  @Autowired
  private MongoService mongoService;
  
  //如果和MyBatisPlus一起使用的话，只需要使用注入IService的方式就可以了
//  private IService<User> service;  

  @GetMapping("/index")
  public void index() {
    List<User> userList = mongoService.lambdaQuery().eq(User::getName,"张三").ne(User::getUsername,"admin").list();
    mongoService.save(new User());
    mongoService.updateById(new User());
    mongoService.removeById(1);
  }

}
```

这样一来，就可以直接启动运行了，是不是跟MyBatisPlus的操作方式一模一样，可以不用花太多的成本去学习


### 🚀 快速开发

MyBatisPlus就是可以不用像MyBatis一样写过多的sql语句，Mongo-Plus也一样！

### 🌱 集成简单

可以和任意 Java Web 框架集成，如：SpringBoot、Spring MVC、Jfinal 等等。

#### Spring Boot 项目，添加依赖即集成完毕：
```xml
<dependency>
    <groupId>com.gitee.anwena</groupId>
    <artifactId>mongo-plus-boot-starter</artifactId>
    <version>2.0.2</version>
</dependency>
```

#### 配置文件配置：
```yaml
mongo-plus:
  data:
    mongodb:
      host: 127.0.0.1
      port: 27017
      database: test
      username: admin
      password: admin
      authenticationDatabase: admin
      connectTimeoutMS: 50000
```

#### 多数据源配置：
```yaml
mongo-plus:
  data:
    mongodb:
      host: 127.0.0.1
      port: 27017
      database: test
      username: admin #没有可不写
      password: admin #没有可不写
      slaveDataSource[0]:
        slaveName: test1
        host: 127.0.0.1
        port: 27017
        database: database1
        username: admin #没有可不写
        password: admin #没有可不写
      slaveDataSource[1]:
        slaveName: test2
        host: 127.0.0.1
        port: 27017
        database: database2
        username: admin #没有可不写
        password: admin #没有可不写
```

### 📚   更新日志
v1.0 基础用法

### ❤️ 参与贡献

1.  Star and Fork 本仓库
2.  新建 Feat_xxx 分支
3.  提交代码
4.  新建 Pull Request

