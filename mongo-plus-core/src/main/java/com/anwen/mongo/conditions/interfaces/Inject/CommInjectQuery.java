package com.anwen.mongo.conditions.interfaces.Inject;

import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.ClientSession;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListMap;

/**
 * 公共条件
 * @author JiaChaoYang
 **/
public interface CommInjectQuery {

    /**
     * 获取列表 返回Map<String,Object>类型的List
     * @param collectionName 集合名
     * @return {@link List < Map <String,Object>>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
     */
    List<Map<String,Object>> list(String collectionName);

    /**
     * 获取列表 返回Map<String,Object>类型的List
     * @param database 数据库
     * @param collectionName collectionName 集合名
     * @return java.util.List<java.util.Map<java.lang.String,java.lang.Object>>
     * @author JiaChaoYang
     * @date 2024/1/10 21:58
    */
    List<Map<String,Object>> list(String database,String collectionName);

    /**
     * 获取列表 返回Map<String,Object>类型的List
     * @param collectionName 集合名
     * @return {@link List < Map <String,Object>>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
     */
    List<Map<String,Object>> list(ClientSession clientSession,String collectionName);

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
     * @param pageParam 分页参数对象
     * @return {@link PageResult < Map< String, Object>>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:19
     */
    PageResult<Map<String,Object>> page(ClientSession clientSession,String collectionName, PageParam pageParam);

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
     * 分页
     * @param collectionName 集合名
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link PageResult< Map< String, Object>>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:20
     */
    PageResult<Map<String,Object>> page(ClientSession clientSession,String collectionName , Integer pageNum, Integer pageSize);

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
     * @return {@link long}
     * @author JiaChaoYang
     * @date 2023/7/27 13:11
     */
    long count(ClientSession clientSession,String collectionName);

}
