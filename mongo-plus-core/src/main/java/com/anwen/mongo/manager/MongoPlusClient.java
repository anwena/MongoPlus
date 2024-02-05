package com.anwen.mongo.manager;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;

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
     * 连接管理器
     * @author JiaChaoYang
     * @date 2024/1/6 2:12
    */
    private Map<String,CollectionManager> collectionManager;

    private CollectionNameConvert collectionNameConvert;

    public CollectionNameConvert getCollectionNameConvert() {
        return collectionNameConvert;
    }

    public void setCollectionNameConvert(CollectionNameConvert collectionNameConvert) {
        this.collectionNameConvert = collectionNameConvert;
    }

    public Map<String,CollectionManager> getCollectionManager() {
        return collectionManager;
    }

    public MongoCollection<Document> getCollection(Class<?> clazz){
        return getCollectionManager(clazz).getCollection(clazz);
    }

    public MongoCollection<Document> getCollection(Class<?> clazz,String collectionName){
        return getCollectionManager(clazz).getCollection(collectionName);
    }

    public MongoCollection<Document> getCollection(String database,String collectionName){
        return getCollectionManager(database).getCollection(collectionName);
    }

    public MongoCollection<Document> getCollection(String database,Class<?> clazz){
        return getCollectionManager(database).getCollection(clazz);
    }

    public CollectionManager getCollectionManager(Class<?> clazz){
        String database = "";
        CollectionName collectionName = clazz.getAnnotation(CollectionName.class);
        if (collectionName != null){
            database = collectionName.database();
        }
        return getCollectionManager(database);
    }

    public CollectionManager getCollectionManager(String database){
        Map<String, CollectionManager> managerMap = getCollectionManager();
        if (StringUtils.isBlank(database)){
            database = managerMap.keySet().stream().findFirst().get();
        }
        CollectionManager collectionManager = managerMap.get(database);
        if (null == collectionManager){
            collectionManager = new CollectionManager(getMongoClient(), collectionNameConvert, database);
            getMongoDatabase().add(getMongoClient().getDatabase(database));
            getCollectionManager().put(database,collectionManager);
        }
        return collectionManager;
    }

    public void setCollectionManager(Map<String,CollectionManager> collectionManager) {
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
