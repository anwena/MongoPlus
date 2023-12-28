package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

/**
 * Integer转换策略实现
 *
 * @author JiaChaoYang
 **/
public class IntegerConversionStrategy implements ConversionStrategy<Integer> {

    Logger logger = LoggerFactory.getLogger(IntegerConversionStrategy.class);

    @Override
    public Integer convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Integer value = null;
        try {
            value = Integer.parseInt(StringUtils.isNotBlankAndConvert(fieldValue));
        } catch (Exception e) {
            logger.error("Conversion to number failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
