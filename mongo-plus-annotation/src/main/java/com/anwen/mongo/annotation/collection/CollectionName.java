package com.anwen.mongo.annotation.collection;

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
public @interface CollectionName {
    String value();

    /**
     * 多数据源选项，对应配置文件中的slaveName
     * @author JiaChaoYang
     * @date 2023/10/17 0:22
    */
    @Deprecated
    String dataSource() default "";
    
    /**
     * 选择数据库，可以写配置文件中没有的
     * @author JiaChaoYang
     * @date 2024/1/6 1:50
    */ 
    String database() default "";
}
