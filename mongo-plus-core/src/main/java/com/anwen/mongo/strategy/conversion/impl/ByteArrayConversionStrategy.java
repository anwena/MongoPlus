package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import org.bson.types.Binary;

/**
 * 转换为byte[]
 * @author anwen
 * @date 2024/5/29 下午10:01
 */
public class ByteArrayConversionStrategy implements ConversionStrategy<byte[]> {
    @Override
    public byte[] convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        return ((Binary) fieldValue).getData();
    }
}
