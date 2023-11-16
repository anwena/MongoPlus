package com.anwen.mongo.annotation.mapper;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @Description: 查询注解
 * @Name: Select
 * @Author: Bomber
 * @CreateTime: 2023/11/16 14:40
 */

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Select {

    /**
     * sql命令
     */
    String value();
}
