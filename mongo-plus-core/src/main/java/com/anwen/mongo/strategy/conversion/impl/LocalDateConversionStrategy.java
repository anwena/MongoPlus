package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.InstantUtil;

import java.time.LocalDate;
import java.util.Date;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description LocalDate类型转换策略
 * @date 2023-10-17 09:59
 **/
public class LocalDateConversionStrategy implements ConversionStrategy<LocalDate> {

    @Override
    public LocalDate convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        return fieldValue.getClass().equals(Long.class) ? InstantUtil.convertTimestampToLocalDate((Long) fieldValue) : InstantUtil.convertTimestampToLocalDate(((Date) fieldValue).toInstant());
    }
}
