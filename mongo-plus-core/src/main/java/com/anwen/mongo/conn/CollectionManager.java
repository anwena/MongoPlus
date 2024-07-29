package com.anwen.mongo.conn;

import com.anwen.mongo.cache.global.CollectionLogicDeleteCache;
import com.anwen.mongo.factory.MongoClientFactory;
import com.anwen.mongo.handlers.collection.AnnotationOperate;
import com.anwen.mongo.logic.UnClassCollection;
import com.anwen.mongo.toolkit.codec.RegisterCodecUtil;
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
     *
     * @author JiaChaoYang
     * @date 2023/12/28 10:58
     */
    private final Map<String, MongoCollection<Document>> collectionMap = new ConcurrentHashMap<>();


    private final String database;

    public CollectionManager(String database) {
        this.database = database;
    }

    /**
     * 设置一个连接
     *
     * @author JiaChaoYang
     * @date 2023/12/28 11:20
     */
    public void setCollectionMap(String key, MongoCollection<Document> value) {
        collectionMap.put(key, value);
    }

    public MongoCollection<Document> getCollection(Class<?> clazz) {
        return getCollection(AnnotationOperate.getCollectionName(clazz));
    }

    public MongoCollection<Document> getCollection(String collectionName) {
        MongoCollection<Document> mongoCollection;
        // 检查连接是否需要重新创建
        if (!this.collectionMap.containsKey(collectionName)) {
            mongoCollection = new ConnectMongoDB(MongoClientFactory.getInstance().getMongoClient(), database, collectionName).open();
            this.collectionMap.put(collectionName, mongoCollection);
            CollectionLogicDeleteCache.mapperClassByCollection(mongoCollection.getNamespace().getFullName(), UnClassCollection.class);
        } else {
            mongoCollection = this.collectionMap.get(collectionName);
        }
        return mongoCollection/*.withCodecRegistry(RegisterCodecUtil.getCodecCacheAndDefault())*/;
    }

}
