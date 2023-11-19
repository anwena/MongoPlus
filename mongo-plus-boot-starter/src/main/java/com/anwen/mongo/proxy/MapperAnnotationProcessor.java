package com.anwen.mongo.proxy;

import java.lang.reflect.Method;

/**
 * @Description: mapper注解的处理接口
 * @Name: MapperAnnotationProcessor
 * @Author: Bomber
 * @CreateTime: 2023/11/16 14:47
 */

@FunctionalInterface
public interface MapperAnnotationProcessor {

    /**
     * 处理方法
     * @param source 源对象
     * @param proxy 代理对象
     * @param method 执行的方法
     * @param args 参数
     * @return 执行结果
     */
    Object process(Object source, Object proxy, Method method, Object[] args) throws Throwable;
}
