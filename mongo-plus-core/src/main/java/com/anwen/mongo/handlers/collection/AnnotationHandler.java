package com.anwen.mongo.handlers.collection;

import java.util.function.Function;

/**
 * 注解处理器
 * @author anwen
 * @date 2024/7/10 下午3:48
 */
public interface AnnotationHandler {

    /**
     * 获取属性值
     * @param obj 参数
     * @param func 属性
     * @return {@link R}
     * @author anwen
     * @date 2024/7/10 下午3:49
     */
    default <T,R> R getProperty(T obj, Function<? super T,? extends R> func){
        return func.apply(obj);
    }

}
