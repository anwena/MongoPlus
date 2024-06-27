package com.anwen.mongo.mapping;

import org.bson.Document;

/**
 * Mongo对象映射为Bean
 * @author anwen
 * @date 2024/5/2 下午5:35
 */
public interface EntityRead {

    default <T> T read(Document document, Class<T> clazz){
        return read(document,new TypeReference<T>(clazz) {});
    }

    /**
     * 映射
     * @author anwen
     * @date 2024/5/7 下午5:11
     */
    default <T> T read(Object sourceObj,TypeReference<T> typeReference){
        return readInternal((Document) sourceObj,typeReference,true);
    }

    /**
     * 写内部对象
     * @author anwen
     * @date 2024/6/28 上午1:09
     */
    default <T> T readInternal(Document document, Class<T> clazz, boolean useIdAsFieldName){
        return readInternal(document,new TypeReference<T>(clazz) {},useIdAsFieldName);
    }

    /**
     * 写内部对象
     * @author anwen
     * @date 2024/6/28 上午1:09
     */
    <T> T readInternal(Document document, TypeReference<T> typeReference, boolean useIdAsFieldName);

}
