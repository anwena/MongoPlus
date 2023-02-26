package com.anwen.mongo.sql.comm;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import org.bson.Document;


/**
 * @author JiaChaoYang
 * 连接工具类
 * @since 2023-02-09 14:47
 **/
public class ConnectMongoDB {

    private final MongoClient mongoClient;

    private final String database;

    private final String collectionValue;

    public ConnectMongoDB(MongoClient mongoClient, String database, String collectionValue) {
        this.mongoClient = mongoClient;
        this.database = database;
        this.collectionValue = collectionValue;
    }

    public MongoCollection<Document> open(){
        //连接到数据库
        return mongoClient.getDatabase(database).getCollection(collectionValue);
    }

}
