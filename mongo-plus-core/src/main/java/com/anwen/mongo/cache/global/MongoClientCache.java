package com.anwen.mongo.cache.global;

import com.mongodb.client.MongoClient;

import java.util.HashMap;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class MongoClientCache {

//    public static MongoClient mongoClient;

    public static Map<String,MongoClient> mongoClientMap = new HashMap<>();

//    private BlockingQueue<Map<String,MongoClient>> mongoClientBlockingQueue = new LinkedBlockingQueue<>();

//    public void addMongoClient(String key,MongoClient mongoClient){
//        mongoClientBlockingQueue.add(new HashMap<String,MongoClient>(){{
//            put(key,mongoClient);
//        }});
//    }

}
