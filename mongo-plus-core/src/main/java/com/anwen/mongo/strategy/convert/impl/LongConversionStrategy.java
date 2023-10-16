package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * Long类型转换策略实现
 *
 * @author JiaChaoYang
 **/
public class LongConversionStrategy implements ConversionStrategy {
    @Override
    public void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        field.set(obj, Long.parseLong(String.valueOf(fieldValue)));
    }
}
