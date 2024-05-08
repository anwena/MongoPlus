package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;

/**
 * Long类型转换策略实现
 *
 * @author JiaChaoYang
 **/
public class LongConversionStrategy implements ConversionStrategy<Long> {

    Log log = LogFactory.getLog(LongConversionStrategy.class);

    @Override
    public Long convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
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
