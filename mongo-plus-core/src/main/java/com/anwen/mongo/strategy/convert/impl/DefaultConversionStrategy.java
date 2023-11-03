package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * 无策略格式数据
 *
 * @author JiaChaoYang
 **/
public class DefaultConversionStrategy implements ConversionStrategy<Object> {
    @Override
    public Object convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return fieldValue;
    }
}
