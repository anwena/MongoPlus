package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description LocalDate类型转换策略
 * @date 2023-10-17 09:59
 **/
public class LocalDateConversionStrategy implements ConversionStrategy {
    @Override
    public void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        field.set(obj, LocalDate.parse(String.valueOf(fieldValue), DateTimeFormatter.ofPattern("yyyy-MM-dd")));
    }
}
