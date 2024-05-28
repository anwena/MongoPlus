package com.anwen.mongo.strategy.mapping.impl;

import com.anwen.mongo.strategy.mapping.MappingStrategy;

import java.math.BigInteger;

/**
 * BigInteger映射器
 * @author anwen
 * @date 2024/5/28 下午9:44
 */
public class BigIntegerMappingStrategy implements MappingStrategy<BigInteger> {
    @Override
    public Object mapping(BigInteger fieldValue) throws IllegalAccessException {
        return fieldValue.longValue();
    }
}
