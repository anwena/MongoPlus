package com.anwen.mongo.cache.global;

import com.anwen.mongo.manager.MongoPlusClient;
import com.mongodb.client.MongoClient;

import java.util.HashMap;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class MongoPlusClientCache {

//    public static MongoClient mongoClient;

    /**
     * 将数据源对应的MongoClient缓存起来
     * @author JiaChaoYang
     * @date 2024/1/6 12:25
    */
    public static Map<String, MongoPlusClient> mongoPlusClientMap = new HashMap<>();

//    private BlockingQueue<Map<String,MongoClient>> mongoClientBlockingQueue = new LinkedBlockingQueue<>();

//    public void addMongoClient(String key,MongoClient mongoClient){
//        mongoClientBlockingQueue.add(new HashMap<String,MongoClient>(){{
//            put(key,mongoClient);
//        }});
//    }

}
