package com.anwen.mongo.mapping;

import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.Map;

/**
 * 将对象映射为Document和将Document映射为对象
 * @author JiaChaoYang
 * @date 2024/4/16 下午9:10
*/ 
public interface MongoConvert {

    <T> void write(Object sourceObj, Bson bson);

    void write(Map<String,Object> map,Bson bson);

    void read(Document document, Class<?> clazz,List<?> resultList);

    Map<String,Object> read(Document document);
}
