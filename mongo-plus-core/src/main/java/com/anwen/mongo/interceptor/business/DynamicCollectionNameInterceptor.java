package com.anwen.mongo.interceptor.business;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.handlers.CollectionNameHandler;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.manager.MongoPlusClient;
import com.mongodb.MongoNamespace;
import com.mongodb.client.MongoCollection;
import org.bson.Document;

/**
 * 动态集合拦截器
 *
 * @author anwen
 * @date 2024/6/27 下午2:46
 */
public class DynamicCollectionNameInterceptor implements Interceptor {

    private final CollectionNameHandler collectionNameHandler;

    private final MongoPlusClient mongoPlusClient;

    public DynamicCollectionNameInterceptor(CollectionNameHandler collectionNameHandler,MongoPlusClient mongoPlusClient) {
        this.collectionNameHandler = collectionNameHandler;
        this.mongoPlusClient = mongoPlusClient;
    }

    @Override
    public int order() {
        return 2;
    }

    @Override
    public Object[] beforeExecute(ExecuteMethodEnum executeMethodEnum, Object[] source, MongoCollection<Document> collection) {
        MongoNamespace namespace = collection.getNamespace();
        String collectionName = collectionNameHandler.dynamicCollectionName(executeMethodEnum,source,namespace);
        source[source.length-1] = mongoPlusClient.getCollection(namespace.getDatabaseName(),collectionName);
        return source;
    }
}
