package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

/**
 * Float类型转换策略
 *
 * @author JiaChaoYang
 **/
public class FloatConversionStrategy implements ConversionStrategy<Float> {

    Logger logger = LoggerFactory.getLogger(FloatConversionStrategy.class);

    @Override
    public Float convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Float value = null;
        try {
            value = Float.parseFloat(StringUtils.isNotBlankAndConvert(fieldValue));
        } catch (Exception e) {
            logger.error("Conversion to Float failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
