package com.anwen.mongo.annotation;

import com.anwen.mongo.registrar.MongoPlusMapperRegistrar;
import org.springframework.context.annotation.Import;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @Description: mapper扫描注解
 * @Name: MapperScan
 * @Author: Bomber
 * @CreateTime: 2023/11/16 16:28
 */

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Import(MongoPlusMapperRegistrar.class)
public @interface MapperScan {
    String[] basePackages() default "";
}
