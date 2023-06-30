package com.anwen.mongo.annotation.proxy;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 标识需要设置泛型的方法
 * @author JiaChaoYang
 * @date 2023/6/30/030 0:13
*/
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface AutoSetMongoEntity {
}
