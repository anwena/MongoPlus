package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;

/**
 * Byte转换策略
 * @author anwen
 * @date 2024/5/30 下午2:08
 */
public class ByteConversionStrategy implements ConversionStrategy<Byte> {
    @Override
    public Byte convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        return Byte.valueOf(String.valueOf(fieldValue));
    }
}
