package com.anwen.mongo.mapping;

import com.mongodb.client.MongoIterable;
import org.bson.Document;

import java.util.List;
import java.util.Map;

/**
 * Mongo对象映射为Bean
 * @author anwen
 * @date 2024/5/2 下午5:35
 */
public interface EntityRead {

    <T> T read(Document document, Class<T> target);

    default <T,R> void read(MongoIterable<R> mongoIterable, Class<T> target, List<T> resultList){
//        resultList.add(read(mongoIterable,target));
    }

//    void read(MongoIterable<Map> mongoIterable);

}
