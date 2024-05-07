package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;

import java.math.BigInteger;

/**
 * BigInteger类型转换器策略实现类
 *
 * @author JiaChaoYang
 **/
public class BigIntegerConversionStrategy implements ConversionStrategy<BigInteger> {

    private final Log log = LogFactory.getLog(BigIntegerConversionStrategy.class);

    @Override
    public BigInteger convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        BigInteger value = null;
        try {
            value = new BigInteger(StringUtils.isNotBlankAndConvert(fieldValue));
        }catch (Exception e){
            log.warn("Convert fieldValue To BigDecimal Fail,Exception Message: {}",e.getMessage(),e);
        }
        return value;
    }
}
