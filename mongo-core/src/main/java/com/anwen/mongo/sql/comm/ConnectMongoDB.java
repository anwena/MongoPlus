package com.anwen.mongo.sql.comm;

import com.anwen.mongo.codec.GenericCodec;
import com.anwen.mongo.utils.ClassTypeUtil;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import org.bson.Document;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;

import java.util.ArrayList;
import java.util.List;


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

    public <T> MongoCollection<Document> open(T t){
        // 定义多个CodecRegistry对象并合并
        List<CodecRegistry> codecRegistryList = new ArrayList<>();
        codecRegistryList.add(MongoClientSettings.getDefaultCodecRegistry());
        List<Class<?>> fieldClasses = ClassTypeUtil.getAllCustomFieldClasses(t.getClass());
        fieldClasses.forEach(clazz -> {
            codecRegistryList.add(CodecRegistries.fromCodecs(new GenericCodec<>(clazz)));
        });
        return mongoClient.getDatabase(database).getCollection(collectionValue).withCodecRegistry(CodecRegistries.fromRegistries(codecRegistryList));
    }

}
