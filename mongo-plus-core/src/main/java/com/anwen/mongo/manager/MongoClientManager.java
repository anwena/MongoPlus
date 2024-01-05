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

    protected final Map<String, MongoClient> mongoClientMap = new HashMap<>();



    public void setMongoClient(String key,MongoClient mongoClient){
        mongoClientMap.put(key,mongoClient);
    }

    public MongoClient getMongoClient(String key){
        return mongoClientMap.get(key);
    }

}
class A{
    BaseProperty baseProperty;

    MongoClient mongoClient;

    List<MongoDatabase> mongoDatabase;

    CollectionManager collectionManager;
}
