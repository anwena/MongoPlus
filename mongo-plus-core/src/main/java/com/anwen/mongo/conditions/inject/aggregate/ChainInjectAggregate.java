package com.anwen.mongo.conditions.inject.aggregate;

import com.mongodb.client.ClientSession;

import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public interface ChainInjectAggregate {

    /**
     * 获取列表 返回Map<String,Object>类型的List
     * @param collectionName 集合名
     * @return {@link List < Map <String,Object>>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
     */
    List<Map<String,Object>> list(String collectionName);

    List<Map<String,Object>> list(ClientSession clientSession,String collectionName);

    <E> List<E> list(String collectionName,Class<E> clazz);

    <E> List<E> list(ClientSession clientSession,String collectionName,Class<E> clazz);

}
