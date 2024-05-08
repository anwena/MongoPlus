package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;

import java.util.Date;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description Date类型转换器策略实现
 * @date 2023-10-17 10:40
 **/
public class DateConversionStrategy implements ConversionStrategy<Date> {

    @Override
    public Date convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        Date date;
        if (fieldValue.getClass().equals(Long.class)){
            date = new Date((Long) fieldValue);
        }else {
            date = (Date) fieldValue;
        }
        return date;
    }
}
