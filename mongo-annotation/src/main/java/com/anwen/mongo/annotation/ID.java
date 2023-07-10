package com.anwen.mongo.annotation;

import com.anwen.mongo.enums.IdTypeEnum;

import java.lang.annotation.*;

/**
 * @author JiaChaoYang
 * 标识这是MongoDB的ID属性
 * @since 2023-02-13 11:54
 **/
@Target(ElementType.FIELD)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface ID {

    IdTypeEnum type() default IdTypeEnum.ASSIGN_ID;

}
