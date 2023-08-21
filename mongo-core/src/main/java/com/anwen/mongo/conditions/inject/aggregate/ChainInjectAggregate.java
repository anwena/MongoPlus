package com.anwen.mongo.conditions.inject.aggregate;

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

    <E> List<E> list(String collectionName,Class<E> clazz);



}
