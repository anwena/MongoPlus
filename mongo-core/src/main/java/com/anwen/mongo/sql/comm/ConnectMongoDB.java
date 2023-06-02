package com.anwen.mongo.sql.comm;

import com.anwen.mongo.codec.GenericCodec;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoCollection;
import org.bson.Document;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;

import java.lang.reflect.Field;
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

    private final CodecRegistry codecRegistry = CodecRegistries.fromRegistries(MongoClientSettings.getDefaultCodecRegistry());

    public ConnectMongoDB(MongoClient mongoClient, String database, String collectionValue) {
        this.mongoClient = mongoClient;
        this.database = database;
        this.collectionValue = collectionValue;
    }

    public <T> MongoCollection<Document> open(T t){
        List<T> cl = new ArrayList<>();
        traverseObject(t,cl);
        //连接到数据库
        // 定义多个CodecRegistry对象并合并
        List<CodecRegistry> codecRegistryList = new ArrayList<>();
        cl.forEach(c -> {
            codecRegistryList.add(CodecRegistries.fromCodecs(new GenericCodec<>(c.getClass())));
        });
        codecRegistryList.add(codecRegistry);
        return mongoClient.getDatabase(database).getCollection(collectionValue).withCodecRegistry(CodecRegistries.fromRegistries(codecRegistryList));
    }

    private <T> void traverseObject(T obj, List<T> customObjects) {
        if (obj == null) {
            return;
        }
        Field[] fields = obj.getClass().getDeclaredFields();
        for (Field field : fields) {
            field.setAccessible(true); // 设置访问权限
            T fieldValue = null;
            try {
                fieldValue = (T) field.get(obj);
            } catch (IllegalAccessException e) {
                throw new RuntimeException(e);
            }
            if (codecRegistry.get(fieldValue.getClass()) == null) { // 如果该字段不是基本类型
                if (!customObjects.contains(fieldValue)) { // 避免重复遍历
                    customObjects.add(fieldValue);
                    traverseObject(fieldValue, customObjects); // 递归遍历
                }
            }
        }
    }

    private static boolean isPrimitiveType(Object obj) {
        return obj instanceof String || obj instanceof Number || obj.getClass().isPrimitive();
    }

}
