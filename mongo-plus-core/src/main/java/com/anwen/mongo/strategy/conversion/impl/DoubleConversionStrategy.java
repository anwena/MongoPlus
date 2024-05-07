package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.StringUtils;

/**
 * Double类型转换策略
 *
 * @author JiaChaoYang
 **/
public class DoubleConversionStrategy implements ConversionStrategy<Double> {

    Log log = LogFactory.getLog(DoubleConversionStrategy.class);

    @Override
    public Double convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        Double value = null;
        try {
            if (fieldValue instanceof Double){
                value = (Double) fieldValue;
            }else {
                value = Double.parseDouble(StringUtils.isNotBlankAndConvert(fieldValue));
            }
        } catch (NumberFormatException e) {
            log.warn("Conversion to Double failed, exception message: {}",e.getMessage());
        }
        return value;
    }
}
