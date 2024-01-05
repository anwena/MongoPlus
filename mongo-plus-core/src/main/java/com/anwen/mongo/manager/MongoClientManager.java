package com.anwen.mongo.manager;

import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.model.BaseProperty;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * MongoClient管理器
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-01-05 14:04
 **/
public class MongoClientManager {

    /**
     * Mongo的客户端
     * @author JiaChaoYang
     * @date 2024/1/6 1:33
    */
    protected final Map<String, MongoClient> mongoClientMap = new HashMap<>();

    /**
     * 封装后的Client
     * @author JiaChaoYang
     * @date 2024/1/6 1:34
    */
    protected final Map<String,MongoPlusClient> mongoPlusClientMap = new HashMap<>();

    public void setMongoClient(String key,MongoClient mongoClient){
        mongoClientMap.put(key,mongoClient);
    }

    public MongoClient getMongoClient(String key){
        return mongoClientMap.get(key);
    }

    public void setMongoPlusClient(String key,MongoPlusClient mongoPlusClient){
        mongoPlusClientMap.put(key,mongoPlusClient);
    }

    public MongoPlusClient getMongoPlusClient(String key){
        return mongoPlusClientMap.get(key);
    }

}