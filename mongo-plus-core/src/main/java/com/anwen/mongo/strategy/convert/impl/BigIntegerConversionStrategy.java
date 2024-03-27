package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.math.BigInteger;

/**
 * BigInteger类型转换器策略实现类
 *
 * @author JiaChaoYang
 **/
public class BigIntegerConversionStrategy implements ConversionStrategy<BigInteger> {

    private final Logger logger = LoggerFactory.getLogger(BigIntegerConversionStrategy.class);

    @Override
    public BigInteger convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        BigInteger value = null;
        try {
            value = new BigInteger(StringUtils.isNotBlankAndConvert(fieldValue));
        }catch (Exception e){
            logger.warn("Convert fieldValue To BigDecimal Fail,Exception Message: {}",e.getMessage(),e);
        }
        return value;
    }
}
