package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.InstantUtil;

import java.time.LocalTime;
import java.util.Date;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description LocalTime转换策略实现类
 * @date 2023-10-17 10:02
 **/
public class LocalTimeConversionStrategy implements ConversionStrategy<LocalTime> {

    @Override
    public LocalTime convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        return fieldValue.getClass().equals(Long.class) ? InstantUtil.convertTimestampToLocalTime((Long) fieldValue) : InstantUtil.convertTimestampToLocalTime(((Date) fieldValue).toInstant());
    }
}
