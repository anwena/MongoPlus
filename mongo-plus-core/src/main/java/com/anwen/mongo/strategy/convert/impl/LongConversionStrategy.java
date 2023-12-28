package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

/**
 * Long类型转换策略实现
 *
 * @author JiaChaoYang
 **/
public class LongConversionStrategy implements ConversionStrategy<Long> {

    Logger logger = LoggerFactory.getLogger(LongConversionStrategy.class);

    @Override
    public Long convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Long value = null;
        try {
            value = Long.parseLong(StringUtils.isNotBlankAndConvert(fieldValue));
        } catch (Exception e) {
            logger.error("Conversion to Long failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
