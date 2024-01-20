package com.anwen.mongo.conditions.inject.query;

import com.anwen.mongo.conditions.interfaces.Inject.CommInjectQuery;

import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-23 21:39
 **/
public interface ChainInject extends CommInjectQuery {

    /**
     * 获取单个，返回T类型的对象
     * <p style="color:red">注：如果查询到大于一条数据，会抛出{@link com.anwen.mongo.domain.MongoQueryException}异常</p>
     * @param collectionName 集合名
     * @return Map<String,Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:19
     */
    Map<String,Object> one(String collectionName);

    /**
     * 获取单个，返回T类型的对象
     * <p style="color:red">注：如果查询到大于一条数据，会抛出{@link com.anwen.mongo.domain.MongoQueryException}异常</p>
     * @param collectionName 集合名
     * @return Map<String,Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:19
     */
    Map<String,Object> one(String database,String collectionName);

    /**
     * 获取单个，返回T类型的对象
     * <p style="color:red">注：如果查询到大于一条数据，会取第一条返回</p>
     * @param collectionName 集合名
     * @return Map< String, Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:20
     */
    Map<String,Object> limitOne(String collectionName);

    /**
     * 获取单个，返回T类型的对象
     * <p style="color:red">注：如果查询到大于一条数据，会取第一条返回</p>
     * @param collectionName 集合名
     * @return Map< String, Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:20
     */
    Map<String,Object> limitOne(String database,String collectionName);

}
