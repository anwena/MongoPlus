package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description LocalDateTime类型策略实现类
 * @date 2023-10-17 09:55
 **/
public class LocalDateTimeConversionStrategy implements ConversionStrategy {
    @Override
    public void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        field.set(obj, LocalDateTime.parse(String.valueOf(fieldValue), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
    }
}
