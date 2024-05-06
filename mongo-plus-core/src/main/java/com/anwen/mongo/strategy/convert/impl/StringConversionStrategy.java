package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;

/**
 * String类型转换策略
 *
 * @author JiaChaoYang
 **/
public class StringConversionStrategy implements ConversionStrategy<String> {

    Log log = LogFactory.getLog(StringConversionStrategy.class);

    @Override
    public String convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        try {
            if (fieldValue == null) {
                return null;
            }
            if (fieldValue instanceof String) {
                return (String) fieldValue;
            }
            return String.valueOf(fieldValue);
        } catch (Exception e) {
            log.warn("Conversion to String failed, exception message: {}",e.getMessage());
        }
        return null;
    }
}
