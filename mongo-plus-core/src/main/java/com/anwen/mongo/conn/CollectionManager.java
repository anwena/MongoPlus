package com.anwen.mongo.conn;

import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.factory.MongoClientFactory;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.codec.RegisterCodecUtil;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import org.bson.Document;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 连接管理器
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 11:13
 **/
public class CollectionManager {

    /**
     * 缓存mongoCollection
     * @author JiaChaoYang
     * @date 2023/12/28 10:58
     */
    private final Map<String, MongoCollection<Document>> collectionMap = new ConcurrentHashMap<>();

    private final CollectionNameConvert collectionNameConvert;

    private final String database;

    public CollectionManager(MongoClient mongoClient, CollectionNameConvert collectionNameConvert, String database) {
        this.collectionNameConvert = collectionNameConvert;
        this.database = database;
    }

    /**
     * 设置一个连接
     * @author JiaChaoYang
     * @date 2023/12/28 11:20
    */
    public void setCollectionMap(String key,MongoCollection<Document> value){
        collectionMap.put(key,value);
    }

    private <T> MongoCollection<Document> getCollection(T entity) {
        return getCollection(ClassTypeUtil.getClass(entity)).withCodecRegistry(RegisterCodecUtil.registerCodec(entity));
    }

    private MongoCollection<Document> getCollection(String collectionName,Map<?,?> map){
        return getCollection(collectionName).withCodecRegistry(RegisterCodecUtil.registerCodec(map));
    }

    public MongoCollection<Document> getCollection(Class<?> clazz) {
        String collectionName = this.collectionNameConvert.convert(clazz);
        return getCollection(collectionName);
    }

    public MongoCollection<Document> getCollection(String collectionName) {
        MongoCollection<Document> mongoCollection;
        // 检查连接是否需要重新创建
        if (!this.collectionMap.containsKey(collectionName)) {
            mongoCollection = new ConnectMongoDB(MongoClientFactory.getInstance().getMongoClient(), database, collectionName).open();
            this.collectionMap.put(collectionName, mongoCollection);
        }else {
            mongoCollection = this.collectionMap.get(collectionName);
        }
        return mongoCollection.withCodecRegistry(RegisterCodecUtil.getCodecCacheAndDefault());
    }

}
