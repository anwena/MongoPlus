package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;

import java.lang.reflect.Field;

/**
 * Float类型转换策略
 *
 * @author JiaChaoYang
 **/
public class FloatConversionStrategy implements ConversionStrategy<Float> {

    Log log = LogFactory.getLog(FloatConversionStrategy.class);

    @Override
    public Float convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        Float value = null;
        try {
            value = Float.parseFloat(StringUtils.isNotBlankAndConvert(fieldValue));
        } catch (Exception e) {
            log.warn("Conversion to Float failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
