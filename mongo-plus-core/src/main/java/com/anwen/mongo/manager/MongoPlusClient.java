package com.anwen.mongo.manager;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.factory.MongoClientFactory;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 连接管理器
 *
 * @author JiaChaoYang
 **/
public class MongoPlusClient {

    private BaseProperty baseProperty;

    private List<MongoDatabase> mongoDatabase;

    /**
     * 连接管理器
     * @author JiaChaoYang
     * @date 2024/1/6 2:12
    */
    private Map<String,Map<String,CollectionManager>> collectionManagerMap;

    private CollectionNameConvert collectionNameConvert;

    public CollectionNameConvert getCollectionNameConvert() {
        return collectionNameConvert;
    }

    public void setCollectionNameConvert(CollectionNameConvert collectionNameConvert) {
        this.collectionNameConvert = collectionNameConvert;
    }

    public Map<String,Map<String,CollectionManager>> getCollectionManagerMap() {
        return collectionManagerMap;
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
        String database = baseProperty.getDatabase();
        if (database.contains(",")){
            database = Arrays.stream(database.split(",")).collect(Collectors.toList()).get(0);
        }
        CollectionName collectionName = clazz.getAnnotation(CollectionName.class);
        if (collectionName != null && StringUtils.isNotBlank(collectionName.database())){
            database = collectionName.database();
        }
        return getCollectionManager(database);
    }

    public CollectionManager getCollectionManager(String database){
        if (StringUtils.isBlank(database)){
            database = getCollectionManagerMap().keySet().stream().findFirst().get();
        }
        Map<String, CollectionManager> managerMap = getCollectionManagerMap().get(DataSourceNameCache.getDataSource());
        if (null == managerMap || null == managerMap.get(database)){
            CollectionManager collectionManager = new CollectionManager(getMongoClient(), collectionNameConvert, database);
            getMongoDatabase().add(getMongoClient().getDatabase(database));
            String finalDatabase = database;
            getCollectionManagerMap().put(DataSourceNameCache.getDataSource(),new ConcurrentHashMap<String,CollectionManager>(){{
                put(finalDatabase, collectionManager);
            }});
        }
        return getCollectionManagerMap().get(DataSourceNameCache.getDataSource()).get(database);
    }

    public void setCollectionManagerMap(String database) {
        CollectionManager collectionManager = new CollectionManager(getMongoClient(), collectionNameConvert, database);
        getMongoDatabase().add(getMongoClient().getDatabase(database));
        getCollectionManagerMap().put(DataSourceNameCache.getDataSource(),new ConcurrentHashMap<String,CollectionManager>(){{
            put(database, collectionManager);
        }});
    }

    public void setCollectionManagerMap(Map<String,Map<String,CollectionManager>> collectionManagerMap) {
        this.collectionManagerMap = collectionManagerMap;
    }

    public BaseProperty getBaseProperty() {
        return baseProperty;
    }

    public void setBaseProperty(BaseProperty baseProperty) {
        this.baseProperty = baseProperty;
    }

    public MongoClient getMongoClient() {
        return MongoClientFactory.getInstance().getMongoClient();
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
                ", mongoDatabase=" + mongoDatabase +
                ", collectionManager=" + collectionManagerMap +
                '}';
    }
}
