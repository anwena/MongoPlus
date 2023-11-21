package com.anwen.mongo.annotation.mapper;

import java.lang.annotation.*;

/**
 * @Description: 对应参数注解
 * @Name: Param
 * @Author: Bomber
 * @CreateTime: 2023/11/16 14:42
 */

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.PARAMETER)
@Documented
public @interface Param {

    /**
     * 参数名称
     * @return
     */
    String value() default "";
}
