package com.anwen.mongo.mapping;

import com.mongodb.client.MongoIterable;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.Map;

/**
 * 将java转为mongodb可用类型
 * @author JiaChaoYang
 * @date 2024/5/1 下午8:36
 */
public interface MongoWriter {

    void write(Object sourceObj, Bson bson);

    void write(Map<?,?> map, Bson bson);

}
