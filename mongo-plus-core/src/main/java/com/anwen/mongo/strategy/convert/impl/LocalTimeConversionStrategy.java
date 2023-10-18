package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.InstantUtil;

import java.lang.reflect.Field;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description LocalTime转换策略实现类
 * @date 2023-10-17 10:02
 **/
public class LocalTimeConversionStrategy implements ConversionStrategy {
    @Override
    public void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        field.set(obj, fieldValue.getClass().equals(Long.class) ? InstantUtil.convertTimestampToLocalTime((Long) fieldValue) : LocalTime.parse(String.valueOf(fieldValue), DateTimeFormatter.ofPattern("HH:mm:ss")));
    }
}
