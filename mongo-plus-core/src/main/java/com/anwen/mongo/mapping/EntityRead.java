package com.anwen.mongo.mapping;

/**
 * Mongo对象映射为Bean
 * @author anwen
 * @date 2024/5/2 下午5:35
 */
public interface EntityRead {

    <T> T read(FieldInformation fieldInformation,Object sourceObj, Class<T> clazz);

}
