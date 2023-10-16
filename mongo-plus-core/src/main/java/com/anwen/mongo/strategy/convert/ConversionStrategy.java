package com.anwen.mongo.strategy.convert;

import java.lang.reflect.Field;

/**
 * 转换策略
 * @author JiaChaoYang
 * @date 2023/10/16 23:30
*/
public interface ConversionStrategy {

    void convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException;

}
