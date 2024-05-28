package com.anwen.mongo.cache.global;

import com.anwen.mongo.strategy.mapping.MappingStrategy;
import com.anwen.mongo.strategy.mapping.impl.BigIntegerMappingStrategy;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class MappingCache {

    private static final Map<Class<?>, MappingStrategy<?>> mappingStrategyMap = new HashMap<>();

    static {
//        mappingStrategyMap.put(BigInteger.class,new BigIntegerMappingStrategy());
    }

    public static MappingStrategy<?> getMappingStrategy(Class<?> clazz){
        return mappingStrategyMap.get(clazz);
    }

    public static void putMappingStrategy(Class<?> clazz,MappingStrategy<?> mappingStrategy){
        mappingStrategyMap.put(clazz,mappingStrategy);
    }

}
