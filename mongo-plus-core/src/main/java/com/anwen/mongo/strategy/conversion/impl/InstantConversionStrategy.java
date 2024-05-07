package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;

import java.time.Instant;

/**
 * Instant类型转换器策略实现类
 *
 * @author JiaChaoYang
 **/
public class InstantConversionStrategy implements ConversionStrategy<Instant> {

    Log log = LogFactory.getLog(InstantConversionStrategy.class);

    @Override
    public Instant convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        Instant value = null;
        try {
            value = Instant.ofEpochMilli(Long.parseLong(StringUtils.isNotBlankAndConvert(fieldValue)));
        } catch (Exception e){
            log.warn("Conversion to timestamp failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
