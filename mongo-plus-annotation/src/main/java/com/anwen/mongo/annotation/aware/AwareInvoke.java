package com.anwen.mongo.annotation.aware;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 标志感知类需要调用的方法 只能唯一
 *
 * @author loser
 * @date 2024/6/30
 */
@Target({ElementType.METHOD})
//运行时注解
@Retention(RetentionPolicy.RUNTIME)
//表明这个注解应该被 javadoc工具记录
//生成文档
@Documented
public @interface AwareInvoke {

}
