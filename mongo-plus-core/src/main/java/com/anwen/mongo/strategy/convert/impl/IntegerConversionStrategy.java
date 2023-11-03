package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * Integer转换策略实现
 *
 * @author JiaChaoYang
 **/
public class IntegerConversionStrategy implements ConversionStrategy<Integer> {
    @Override
    public Integer convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return Integer.parseInt(String.valueOf(fieldValue));
    }
}
