package com.anwen.mongo.proxy.impl;

import com.anwen.mongo.annotation.mapper.Select;

import java.lang.reflect.Method;
import java.util.function.Supplier;

/**
 * @Description: select注解处理实现类
 * @Name: SelectAnnotationProcessor
 * @Author: Bomber
 * @CreateTime: 2023/11/20 15:23
 */
public class SelectAnnotationProcessor extends AbstractSqlAnnotationProcessor {

    @Override
    protected Supplier<String> sqlSupplier(Method method) {
        return () -> method.getAnnotation(Select.class).value();
    }
}
