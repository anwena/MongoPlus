package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * String类型转换策略
 *
 * @author JiaChaoYang
 **/
public class StringConversionStrategy implements ConversionStrategy<String> {
    @Override
    public String convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return String.valueOf(fieldValue);
    }
}
