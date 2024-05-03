package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;

import java.lang.reflect.Field;

/**
 * Long类型转换策略实现
 *
 * @author JiaChaoYang
 **/
public class LongConversionStrategy implements ConversionStrategy<Long> {

    Log log = LogFactory.getLog(LongConversionStrategy.class);

    @Override
    public Long convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Long value = null;
        try {
            if (fieldValue instanceof Long){
                value = (Long) fieldValue;
            } else {
                value = Long.parseLong(StringUtils.isNotBlankAndConvert(fieldValue));
            }
        } catch (Exception e) {
            log.warn("Conversion to Long failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
