package com.anwen.mongo.mapping;

import org.bson.Document;

/**
 * 将对象映射为Document和将Document映射为对象
 * @author JiaChaoYang
 * @date 2024/4/16 下午9:10
*/ 
public interface MongoConverter extends MongoWriter {

    /**
     * 添加的映射器
     * @author JiaChaoYang
     * @date 2024/5/1 下午11:52
     */
    void writeBySave(Object sourceObj, Document document);

}
