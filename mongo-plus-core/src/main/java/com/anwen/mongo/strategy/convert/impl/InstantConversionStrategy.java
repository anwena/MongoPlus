package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;
import java.time.Instant;

/**
 * Instant类型转换器策略实现类
 *
 * @author JiaChaoYang
 **/
public class InstantConversionStrategy implements ConversionStrategy<Instant> {
    @Override
    public Instant convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return Instant.ofEpochMilli(Long.parseLong(String.valueOf(fieldValue)));
    }
}
