package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * Long类型转换策略实现
 *
 * @author JiaChaoYang
 **/
public class LongConversionStrategy implements ConversionStrategy<Long> {
    @Override
    public Long convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return Long.parseLong(String.valueOf(fieldValue));
    }
}
