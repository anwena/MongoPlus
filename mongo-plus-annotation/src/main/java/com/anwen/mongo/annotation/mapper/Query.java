package com.anwen.mongo.annotation.mapper;

import java.lang.annotation.*;

/**
 * 执行查询语句
 * 语句只需要写内部逻辑就可以，比如db.test.find({"userStatus":{"$eq":1})
 * 只需要写find内部的{"userStatus":{"$eq":1}
 * @author JiaChaoYang
 * @date 2023/9/3 19:43
*/
@Target(ElementType.METHOD)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
@Deprecated
public @interface Query {

    String value() default "";

}
