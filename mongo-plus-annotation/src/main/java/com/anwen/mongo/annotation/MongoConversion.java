package com.anwen.mongo.annotation;

import java.lang.annotation.*;

/**
 * 定义转换器注解
 * @author JiaChaoYang
 * @since 2023/2/13 14:24
*/
@Target(ElementType.TYPE)
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
@Deprecated
public @interface MongoConversion {

    Class<?> type();

}
