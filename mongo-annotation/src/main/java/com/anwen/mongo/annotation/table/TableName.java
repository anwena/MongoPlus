package com.anwen.mongo.annotation.table;

import java.lang.annotation.*;

/**
 * @Description: 指定表名，不使用此注解默认取实体类名
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.annotation.table
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-17 21:19
 * @Version: 1.0
 */
@Target(ElementType.TYPE)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface TableName {
    String value() default "";

    String dataSource() default "";
}
