package com.anwen.mongo.conn;

import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.model.BaseModelID;
import com.anwen.mongo.service.IService;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import lombok.Getter;
import org.bson.Document;


/**
 * @author JiaChaoYang
 * 连接工具类
 * @since 2023-02-09 14:47
 **/
@Getter
public class ConnectMongoDB {

    private final MongoClient mongoClient;

    private final String database;

    private final String collection;

    private MongoDatabase mongoDatabase;

    public ConnectMongoDB(MongoClient mongoClient, String database, String collectionValue) {
        this.mongoClient = mongoClient;
        this.database = database;
        this.collection = collectionValue;
    }

    public MongoCollection<Document> open(){
        mongoDatabase = mongoClient.getDatabase(database);
        return mongoDatabase.getCollection(collection);
    }

    public boolean isSame(String database, String collection) {
        return this.database.equals(database) && this.collection.equals(collection);
    }
}
