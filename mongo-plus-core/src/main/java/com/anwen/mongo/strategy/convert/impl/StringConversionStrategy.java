package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

/**
 * String类型转换策略
 *
 * @author JiaChaoYang
 **/
public class StringConversionStrategy implements ConversionStrategy<String> {

    Logger logger = LoggerFactory.getLogger(StringConversionStrategy.class);

    @Override
    public String convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        String value = null;
        try {
            value = StringUtils.isNotBlankAndConvert(fieldValue);
        } catch (Exception e) {
            logger.error("Conversion to String failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
