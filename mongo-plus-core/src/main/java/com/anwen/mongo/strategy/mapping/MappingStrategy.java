package com.anwen.mongo.strategy.mapping;

/**
 * 映射器，将Java类型映射为MongoDB支持类型
 * 比如将BigInteger映射为Long，因为MongoDB并不支持BigInteger，或者将自定义的类型（如User类），映射为Document类型
 * @author anwen
 * @date 2024/5/28 下午9:42
 */
public interface MappingStrategy<T> {

    Object mapping(T fieldValue) throws IllegalAccessException;

}
