package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.InstantUtil;

import java.lang.reflect.Field;
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
    public LocalDateTime convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return fieldValue.getClass().equals(Long.class) ?
                InstantUtil.convertTimestampToLocalDateTime((Long) fieldValue) :
                InstantUtil.convertTimestampToLocalDateTime(((Date) fieldValue).toInstant());
    }
}
