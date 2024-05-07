package com.anwen.mongo.mapping;

import org.bson.Document;

/**
 * Mongo对象映射为Bean
 * @author anwen
 * @date 2024/5/2 下午5:35
 */
public interface EntityRead {

    <T> T read(Document document, Class<T> clazz);

}
