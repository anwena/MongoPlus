package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;

import java.math.BigDecimal;

/**
 * BigDecimal类型转换器策略实现类
 *
 * @author JiaChaoYang
 **/
public class BigDecimalConversionStrategy implements ConversionStrategy<BigDecimal> {

    private final Log log = LogFactory.getLog(BigDecimalConversionStrategy.class);

    @Override
    public BigDecimal convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        BigDecimal value = null;
        try {
            value = new BigDecimal(StringUtils.isNotBlankAndConvert(fieldValue));
        } catch (Exception e){
            log.warn("Convert fieldValue To BigDecimal Fail,Exception Message: {}",e.getMessage(),e);
        }
        return value;
    }
}
