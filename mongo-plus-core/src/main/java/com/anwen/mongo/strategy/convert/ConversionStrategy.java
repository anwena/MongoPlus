package com.anwen.mongo.strategy.convert;

import java.lang.reflect.Field;

/**
 * 转换策略
 * @author JiaChaoYang
 * @date 2023/10/16 23:30
*/
public interface ConversionStrategy {

    /**
     * 转换器方法
     * @param field 这个field为JavaBean中，转换器的类型对应的字段如，String转换器，这个field就是所有String类型的字段
     * @param obj javaBean
     * @param fieldValue 字段值
     * @return void
     * @author JiaChaoYang
     * @date 2023/10/20 18:30
    */
    void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException;

}
