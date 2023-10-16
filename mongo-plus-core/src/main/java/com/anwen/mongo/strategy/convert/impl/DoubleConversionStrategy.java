package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * Double类型转换策略
 *
 * @author JiaChaoYang
 **/
public class DoubleConversionStrategy implements ConversionStrategy {
    @Override
    public void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        field.set(obj, Double.parseDouble(String.valueOf(fieldValue)));
    }
}