package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * Double类型转换策略
 *
 * @author JiaChaoYang
 **/
public class DoubleConversionStrategy implements ConversionStrategy<Double> {
    @Override
    public Double convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return Double.parseDouble(String.valueOf(fieldValue));
    }
}
