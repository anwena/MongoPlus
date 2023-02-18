<p align="center">
  <a href="https://gitee.com/anwena/mongo-plus/blob/master/LICENSE"><img src="https://img.shields.io/hexpm/l/plug.svg" alt="License"></a>
</p>

通过MyBatisPlus的方式，优雅的操作MongoDB


首先，你有一个实体类：

```java
import com.anwen.mongo.sql.model.BaseModelID;

public class User extends BaseModelID {
  private String username;
  private int status;
  private int age;
  private String gender;
  private Date joinDate;
  private int roleId;
  private String roleName;
}
```
将你的service接口继承IService

```java
import com.anwen.mongo.sql.IService;
import org.apache.catalina.User;

public interface MongoServiceImpl extends IService<User> {
        
}
```

再将你的service实现类继承ServiceImpl

```java
import com.anwen.mongo.config.MongoDBConnectProperty;
import com.anwen.mongo.sql.ServiceImpl;
import com.mongodb.MongoClient;
import org.apache.catalina.User;

public class MongoServiceImpl extends ServiceImpl<User> implements MongoService {
    
}
```

然后你就可以使用MyBatisPlus一模一样的代码操作mongodb啦：

```java
import org.apache.catalina.User;

@RestController
@RequestMapping("/user")
public class UserController {

  @Autowired
  private MongoService mongoService;

  @GetMapping("/index")
  public void index() {
    List<User> userList = mongoService.lambdaQuery().eq(User::getName,"张三").ne(User::getUsername,"admin").list();
    mongoService.save(new User());
    mongoService.updateById(new User());
    mongoService.removeById(1);
  }

}
```

这样一来，就可以直接启动运行了，是不是跟MyBatisPlus的操作方式一模一样，可以不用花太多的成本去学习，而且可以和MongoDBTemplate一起使用


### 🚀 快速开发

MyBatisPlus就是可以不用像MyBatis一样写过多的sql语句，Mongo-Plus也一样！

### 🌱 集成简单

可以和任意 Java Web 框架集成，如：SpringBoot、Spring MVC、Jfinal 等等。

#### Spring Boot 项目，添加依赖即集成完毕：
```xml
<dependency>
    <groupId>com.gitee.anwena</groupId>
    <artifactId>mongo-plus-boot-starter</artifactId>
    <version>1.0.0</version>
</dependency>
```

#### 配置文件配置：
```yaml
spring:
  data:
    mongodb:
      host: 127.0.0.1
      port: 27017
      database: test
```

### ❤️ 参与贡献

1.  Star and Fork 本仓库
2.  新建 Feat_xxx 分支
3.  提交代码
4.  新建 Pull Request

