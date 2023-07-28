package com.anwen.mongo.sql.comm;

import com.anwen.mongo.sql.IService;
import com.anwen.mongo.sql.model.BaseModelID;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.Filters;
import org.bson.BsonDocument;
import org.bson.Document;
import org.bson.conversions.Bson;


/**
 * @author JiaChaoYang
 * 连接工具类
 * @since 2023-02-09 14:47
 **/
public class ConnectMongoDB {

    private final MongoClient mongoClient;

    private final String database;

    private final String collection;


    public ConnectMongoDB(MongoClient mongoClient, String database, String collectionValue) {
        this.mongoClient = mongoClient;
        this.database = database;
        this.collection = collectionValue;
    }

    public MongoCollection<Document> open(){
        return mongoClient.getDatabase(database)
                .getCollection(collection);
    }

    public boolean isSame(String database, String collection) {
        return this.database.equals(database) && this.collection.equals(collection);
    }

    public static void main(String[] args) {
        Bson bson = Filters.eq("userName", "张三");
        BsonDocument bsonDocument = bson.toBsonDocument();
        String key = bsonDocument.getFirstKey();
        System.out.println(key);
    }

}
