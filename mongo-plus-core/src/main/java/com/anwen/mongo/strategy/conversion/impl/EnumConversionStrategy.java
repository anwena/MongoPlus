package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * 枚举的转换策略
 * @author anwen
 * @date 2024/5/4 上午12:42
 */
public class EnumConversionStrategy<T> implements ConversionStrategy<Enum> {

    @Override
    @SuppressWarnings({"unchecked","rawtypes"})
    public Enum convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        Class<? extends Enum> enumType = (Class<? extends Enum>) fieldType;
        return Enum.valueOf(enumType, (String) fieldValue);
    }
}
