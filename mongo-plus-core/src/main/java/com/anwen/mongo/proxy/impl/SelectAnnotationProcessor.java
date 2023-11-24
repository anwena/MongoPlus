package com.anwen.mongo.proxy.impl;

import com.anwen.mongo.annotation.mapper.Select;
import com.anwen.mongo.mapper.AbstractMapper;

import java.lang.reflect.Method;
import java.util.List;
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

    @Override
    protected List<?> executeSql(AbstractMapper<?> source, Object proxy, String realSql) {
        return source.sql(realSql);
    }
}
