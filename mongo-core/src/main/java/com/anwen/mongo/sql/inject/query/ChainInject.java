package com.anwen.mongo.sql.inject.query;

import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;

import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-23 21:39
 **/
public interface ChainInject {


    /**
     * 获取列表 返回T类型的List
     * @param collection 集合名
     * @return {@link List<Map<String,Object>>}
     * @author JiaChaoYang
     * @date 2023/7/23 22:07
    */
    List<Map<String,Object>> list(String collection);

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
     * <p style="color:red">注：如果查询到大于一条数据，会取第一条返回</p>
     * @param collectionName
     * @return Map< String, Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:20
     */
    Map<String,Object> limitOne(String collectionName);

    /**
     * 分页
     * @param collectionName 集合名
     * @param pageParam 分页参数对象
     * @return {@link PageResult < Map< String, Object>>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:19
     */
    PageResult<Map<String,Object>> page(String collectionName, PageParam pageParam);

    /**
     * 分页
     * @param collectionName 集合名
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link PageResult< Map< String, Object>>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:20
     */
    PageResult<Map<String,Object>> page(String collectionName , Integer pageNum, Integer pageSize);

    /**
     * 获取总行数
     * @param collectionName 集合名
     * @return {@link long}
     * @author JiaChaoYang
     * @date 2023/7/27 13:11
    */
    long count(String collectionName);

    /**
     * 获取总行数
     * @param collectionName 集合名
     * @param lambdaQueryChainInjectWrapper 条件构造器
     * @return {@link long}
     * @author JiaChaoYang
     * @date 2023/7/27 13:12
    */
    long count(String collectionName,LambdaQueryChainInjectWrapper lambdaQueryChainInjectWrapper);

}
