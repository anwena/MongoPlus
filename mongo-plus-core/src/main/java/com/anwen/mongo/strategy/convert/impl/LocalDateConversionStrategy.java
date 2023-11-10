package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.InstantUtil;

import java.lang.reflect.Field;
import java.time.Instant;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Date;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description LocalDate类型转换策略
 * @date 2023-10-17 09:59
 **/
public class LocalDateConversionStrategy implements ConversionStrategy<LocalDate> {
    @Override
    public LocalDate convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return fieldValue.getClass().equals(Long.class) ? InstantUtil.convertTimestampToLocalDate((Long) fieldValue) : InstantUtil.convertTimestampToLocalDate(((Date) fieldValue).toInstant());
    }
}
