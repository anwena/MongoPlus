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
    String value();

    boolean exist() default true;
}
