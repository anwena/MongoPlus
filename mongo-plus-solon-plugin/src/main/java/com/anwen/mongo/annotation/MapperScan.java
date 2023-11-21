package com.anwen.mongo.annotation;

import com.anwen.mongo.registrar.MongoPlusMapperRegistrar;
import org.noear.solon.annotation.Import;

import java.lang.annotation.*;

/**
 * @Description: mapper扫描注解
 * @Name: MapperScan
 * @Author: Bomber
 * @CreateTime: 2023/11/16 16:28
 */

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Documented
public @interface MapperScan {

    // 扫描的包名
    String[] basePackages() default "";
}
