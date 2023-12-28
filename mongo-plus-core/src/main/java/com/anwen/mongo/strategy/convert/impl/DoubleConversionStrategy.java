package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

/**
 * Double类型转换策略
 *
 * @author JiaChaoYang
 **/
public class DoubleConversionStrategy implements ConversionStrategy<Double> {

    Logger logger = LoggerFactory.getLogger(DoubleConversionStrategy.class);

    @Override
    public Double convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Double value = null;
        try {
            value = Double.parseDouble(StringUtils.isNotBlankAndConvert(fieldValue));
        } catch (NumberFormatException e) {
            logger.error("Conversion to Double failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
