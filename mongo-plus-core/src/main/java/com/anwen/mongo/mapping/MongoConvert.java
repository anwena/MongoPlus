package com.anwen.mongo.mapping;

import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.Map;

/**
 *
 * @author JiaChaoYang
 * @date 2024/4/16 下午9:10
*/ 
public interface MongoConvert {

    <T> Document write(T entity);

    Document write(Map<String,Object> map);

    <T> List<T> read(Document document, Class<?> clazz);

    Map<String,Object> read(Document document);
}
