package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description Date类型转换器策略实现
 * @date 2023-10-17 10:40
 **/
public class DateConversionStrategy implements ConversionStrategy<Date> {

    @Override
    public Date convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Date date;
        if (fieldValue.getClass().equals(Long.class)){
            date = new Date((Long) fieldValue);
        }else {
            date = (Date) fieldValue;
        }
        return date;
    }
}
