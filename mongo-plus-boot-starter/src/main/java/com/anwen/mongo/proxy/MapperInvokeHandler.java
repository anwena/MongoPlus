package com.anwen.mongo.proxy;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.mapper.AbstractMapper;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * @Author Bomber
 * @Description mapper方法执行器
 * @Date 2023/11/16 20:16
 * @Version 1.0
 */
public class MapperInvokeHandler<T> extends AbstractMapper<T> implements InvocationHandler {

    // 每种类型注解对应的处理器
    private static final Map<String, MapperAnnotationProcessor> MAPPER_ANNOTATION_PROCESSOR_MAP = new HashMap<>();
    // 默认的处理器，没有注解的时候自动使用
    private static final MapperAnnotationProcessor DEFAULT_MAPPER_ANNOTATION_PROCESSOR
            = (source, proxy, method, args) -> method.invoke(source, args);

    // sql执行器
    private SqlExecute sqlExecute;
    // 注解处理器缓存
    private final Map<Method, MapperAnnotationProcessor> cacheMap = new HashMap<>();

    public void setSqlExecute(SqlExecute sqlExecute) {
        this.sqlExecute = sqlExecute;
    }

    @Override
    public SqlExecute getSqlOperation() {
        return sqlExecute;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        // 获取方法上的所有的注解
        Annotation[] annotations = method.getAnnotations();
        // 从缓存中读取
        MapperAnnotationProcessor annotationProcessor = cacheMap.get(method);

        if (Objects.isNull(annotationProcessor)) {
            annotationProcessor = DEFAULT_MAPPER_ANNOTATION_PROCESSOR;

            // 匹配各种处理器
            for (Annotation annotation : annotations) {
                MapperAnnotationProcessor processor = MAPPER_ANNOTATION_PROCESSOR_MAP.get(annotation.getClass().getTypeName());
                if (Objects.nonNull(processor)) {
                    annotationProcessor = processor;
                    break;
                }
            }
            // 进行缓存
            cacheMap.put(method, annotationProcessor);
        }

        // 在各种不同的注解处理器中进行执行方法
        return annotationProcessor.process(this, proxy, method, args);
    }
}
