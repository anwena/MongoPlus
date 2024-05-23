package com.anwen.mongo.annotation.datasource;

import java.lang.annotation.*;

/**
 * 多数据源注解
 *
 * @author JiaChaoYang
 **/
@Target(ElementType.METHOD)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface MongoDs {

    /**
     * 数据源名称
     * @author JiaChaoYang
     * @date 2024/4/5 1:20
    */
    String value();

}
