package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * 枚举的转换策略
 * @author anwen
 * @date 2024/5/4 上午12:42
 */
public class EnumConversionStrategy<T> implements ConversionStrategy<Enum> {

    @Override
    public Enum convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        @SuppressWarnings("unchecked")
        Class<? extends Enum> enumType = (Class<? extends Enum>) field.getType();
        return Enum.valueOf(enumType, (String) fieldValue);
    }
}
