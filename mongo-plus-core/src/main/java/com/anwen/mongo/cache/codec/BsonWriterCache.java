package com.anwen.mongo.cache.codec;

import org.bson.BsonWriter;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author JiaChaoYang
 **/
public class BsonWriterCache {

    public static Map<Class<?>, BsonWriter> bsonWriterMap = new ConcurrentHashMap<>();

}
