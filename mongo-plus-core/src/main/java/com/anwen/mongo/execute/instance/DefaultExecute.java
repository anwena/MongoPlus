package com.anwen.mongo.execute.instance;

import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.model.BaseProperty;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.result.InsertOneResult;
import org.bson.Document;

/**
 * 默认执行器实例
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 11:03
 **/
public class DefaultExecute extends Execute {


    public DefaultExecute(MongoClient mongoClient, BaseProperty baseProperty, CollectionNameConvert collectionNameConvert, CollectionManager collectionManager) {
        super(mongoClient, baseProperty, collectionNameConvert, collectionManager);
    }

    @Override
    public InsertOneResult doSave(Document document, MongoCollection<Document> collection) {
        return collection.insertOne(document);
    }
}
