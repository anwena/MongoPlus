package com.anwen.mongo.annotation.mapper;

import java.lang.annotation.*;

/**
 * @Description: mapper接口的标识注解
 * @Name: Mapper
 * @Author: Bomber
 * @CreateTime: 2023/11/16 15:33
 */

@Documented
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Mapper {

    // beanName
    String value() default "";
}
