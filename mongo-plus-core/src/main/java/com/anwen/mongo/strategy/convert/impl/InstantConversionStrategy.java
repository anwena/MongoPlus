package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.time.Instant;

/**
 * Instant类型转换器策略实现类
 *
 * @author JiaChaoYang
 **/
public class InstantConversionStrategy implements ConversionStrategy<Instant> {

    Logger logger = LoggerFactory.getLogger(InstantConversionStrategy.class);

    @Override
    public Instant convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        Instant value = null;
        try {
            value = Instant.ofEpochMilli(Long.parseLong(StringUtils.isNotBlankAndConvert(fieldValue)));
        } catch (Exception e){
            logger.warn("Conversion to timestamp failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
