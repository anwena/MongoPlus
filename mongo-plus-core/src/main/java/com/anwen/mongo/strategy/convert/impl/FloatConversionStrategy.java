package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * Float类型转换策略
 *
 * @author JiaChaoYang
 **/
public class FloatConversionStrategy implements ConversionStrategy<Float> {
    @Override
    public Float convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return Float.parseFloat(String.valueOf(fieldValue));
    }
}
