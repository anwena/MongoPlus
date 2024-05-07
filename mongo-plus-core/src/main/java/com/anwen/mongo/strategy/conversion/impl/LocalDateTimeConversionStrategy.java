package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.InstantUtil;

import java.time.LocalDateTime;
import java.util.Date;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description LocalDateTime类型策略实现类
 * @date 2023-10-17 09:55
 **/
public class LocalDateTimeConversionStrategy implements ConversionStrategy<LocalDateTime> {

    @Override
    public LocalDateTime convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        return fieldValue.getClass().equals(Long.class) ?
                InstantUtil.convertTimestampToLocalDateTime((Long) fieldValue) :
                InstantUtil.convertTimestampToLocalDateTime8(((Date) fieldValue).toInstant());
    }
}
