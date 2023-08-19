package com.anwen.mongo.annotation.collection;

import java.lang.annotation.*;

/**
 * 指定列名，不指定默认取属性名
 * @author: JiaChaoYang
 * @date: 2023/2/17 21:21
 **/
@Target(ElementType.FIELD)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface CollectionField {
    /**
     * 字段映射值
     * @author JiaChaoYang
     * @date 2023/8/20 2:45
    */
    String value() default "";

    /**
     * 如果值时对象，接收字段是集合，可以使用此配置，会将对象自动转为集合，只存在这一个值
     * 可以用来应对"unwind"等操作符的情况
     * @author JiaChaoYang
     * @date 2023/8/20 2:49
    */
    boolean convertCollect() default false;

    /**
     * 是否为数据库表字段
     * @author JiaChaoYang
     * @date 2023/8/20 2:45
    */
    boolean exist() default true;
}
