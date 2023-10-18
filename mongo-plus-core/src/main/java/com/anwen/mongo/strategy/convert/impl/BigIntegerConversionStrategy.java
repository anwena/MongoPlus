package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.math.BigInteger;

/**
 * BigInteger类型转换器策略实现类
 *
 * @author JiaChaoYang
 **/
public class BigIntegerConversionStrategy implements ConversionStrategy {

    private final Logger logger = LoggerFactory.getLogger(BigIntegerConversionStrategy.class);

    @Override
    public void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        try {
            field.set(obj,new BigInteger(String.valueOf(fieldValue)));
        }catch (Exception e){
            logger.error("Convert fieldValue To BigDecimal Fail,Exception Message: {}",e.getMessage(),e);
        }
    }
}
