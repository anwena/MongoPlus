package com.anwen.mongo.manager;

import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.model.BaseProperty;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoDatabase;

import java.util.List;
import java.util.Map;

/**
 * 连接管理器
 *
 * @author JiaChaoYang
 **/
public class MongoPlusClient {

    private BaseProperty baseProperty;

    private MongoClient mongoClient;

    private List<MongoDatabase> mongoDatabase;

    /**
     * 连接管理器,结构为：{"dataSourceName":{"database":ConnectionManager(){"collection":MongoCollection}}}
     * @author JiaChaoYang
     * @date 2024/1/6 2:12
    */
    private Map<String, Map<String,CollectionManager>> collectionManager;

    public Map<String, Map<String, CollectionManager>> getCollectionManager() {
        return collectionManager;
    }

    public void setCollectionManager(Map<String, Map<String, CollectionManager>> collectionManager) {
        this.collectionManager = collectionManager;
    }

    public BaseProperty getBaseProperty() {
        return baseProperty;
    }

    public void setBaseProperty(BaseProperty baseProperty) {
        this.baseProperty = baseProperty;
    }

    public MongoClient getMongoClient() {
        return mongoClient;
    }

    public void setMongoClient(MongoClient mongoClient) {
        this.mongoClient = mongoClient;
    }

    public List<MongoDatabase> getMongoDatabase() {
        return mongoDatabase;
    }

    public void setMongoDatabase(List<MongoDatabase> mongoDatabase) {
        this.mongoDatabase = mongoDatabase;
    }

    @Override
    public String toString() {
        return "ConnectionManager{" +
                "baseProperty=" + baseProperty +
                ", mongoClient=" + mongoClient +
                ", mongoDatabase=" + mongoDatabase +
                ", collectionManager=" + collectionManager +
                '}';
    }
}
