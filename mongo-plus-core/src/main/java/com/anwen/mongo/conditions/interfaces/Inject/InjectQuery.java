package com.anwen.mongo.conditions.interfaces.Inject;

import com.anwen.mongo.annotation.CutInID;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.ClientSession;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-20 22:18
 **/
public interface InjectQuery extends CommInjectQuery {

    List<Map<String,Object>> list(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    List<Map<String,Object>> list(ClientSession clientSession,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    List<Map<String,Object>> aggregateList(String collectionName, AggregateChainWrapper<Map<String,Object>,?> queryChainWrapper);

    List<Map<String,Object>> aggregateList(ClientSession clientSession,String collectionName, AggregateChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(String collectionName,PageParam pageParam,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(ClientSession clientSession,String collectionName,PageParam pageParam,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(String collectionName , Integer pageNum, Integer pageSize,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(ClientSession clientSession,String collectionName , Integer pageNum, Integer pageSize,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    Map<String,Object> getById(String collectionName ,Serializable id);

    Map<String,Object> getById(ClientSession clientSession,String collectionName ,Serializable id);

    List<Map<String,Object>> getByIds(String collectionName , Collection<Serializable> ids);

    List<Map<String,Object>> getByIds(ClientSession clientSession,String collectionName , Collection<Serializable> ids);

    /**
     * 添加
     * @param entityMap 添加的Map
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:27
     */
    Boolean save(String collectionName , Map<String,Object> entityMap);

    /**
     * 添加
     * @param entityMap 添加的Map
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:27
     */
    Boolean save(ClientSession clientSession,String collectionName , Map<String,Object> entityMap);

    /**
     * 添加多个
     * @param entityMapList map对象集合
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:56
     */
    Boolean saveBatch(String collectionName , Collection<Map<String,Object>> entityMapList);

    /**
     * 添加多个
     * @param entityMapList map对象集合
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:56
     */
    Boolean saveBatch(ClientSession clientSession,String collectionName , Collection<Map<String,Object>> entityMapList);

    /**
     * 添加或修改
     * @param entityMap map对象
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:57
     */
    Boolean saveOrUpdate(String collectionName , Map<String,Object> entityMap);

    /**
     * 添加或修改
     * @param entityMap map对象
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:57
     */
    Boolean saveOrUpdate(ClientSession clientSession,String collectionName , Map<String,Object> entityMap);

    /**
     * 批量添加或修改
     * @param entityMapList map对象集合
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:57
     */
    Boolean saveOrUpdateBatch(String collectionName , Collection<Map<String,Object>> entityMapList);

    /**
     * 批量添加或修改
     * @param entityMapList map对象集合
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:57
     */
    Boolean saveOrUpdateBatch(ClientSession clientSession,String collectionName , Collection<Map<String,Object>> entityMapList);

    /**
     * 修改根据id
     * @param entityMap 修改的对象，需要包含id
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:28
     */
    Boolean updateById(String collectionName , Map<String,Object> entityMap);

    /**
     * 修改根据id
     * @param entityMap 修改的对象，需要包含id
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:28
     */
    Boolean updateById(ClientSession clientSession,String collectionName , Map<String,Object> entityMap);

    /**
     * 修改多个，根据id
     * @param entityMapList
     * @param collectionName 集合名
     * @return {@link Boolean}
     * @author JiaChaoYang
     * @date 2023/7/20 23:42
    */
    Boolean updateBatchByIds(String collectionName , Collection<Map<String,Object>> entityMapList);

    /**
     * 修改多个，根据id
     * @param entityMapList
     * @param collectionName 集合名
     * @return {@link Boolean}
     * @author JiaChaoYang
     * @date 2023/7/20 23:42
     */
    Boolean updateBatchByIds(ClientSession clientSession,String collectionName , Collection<Map<String,Object>> entityMapList);

    /**
     * 通过列进行修改
     * @param entityMap 修改的实体
     * @param column 根据什么列修改
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:46
     */
    Boolean updateByColumn(String collectionName,Map<String,Object> entityMap, String column);

    /**
     * 通过列进行修改
     * @param entityMap 修改的实体
     * @param column 根据什么列修改
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:46
     */
    Boolean updateByColumn(ClientSession clientSession,String collectionName,Map<String,Object> entityMap, String column);

    /**
     * 根据id删除
     * @param id 数据id
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:47
     */
    Boolean removeById(String collectionName,Serializable id);

    /**
     * 根据id删除
     * @param id 数据id
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:47
     */
    Boolean removeById(ClientSession clientSession,String collectionName,Serializable id);


    /**
     * 根据字段删除
     * @param column 字段
     * @param value 值
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 14:05
     */
    Boolean removeByColumn(String collectionName,String column,String value);

    /**
     * 根据字段删除
     * @param column 字段
     * @param value 值
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 14:05
     */
    Boolean removeByColumn(ClientSession clientSession,String collectionName,String column,String value);

    /**
     * 根据id批量删除
     * @param idList id集合
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:59
     */
    Boolean removeBatchByIds(String collectionName,Collection<Serializable> idList);

    /**
     * 根据id批量删除
     * @param idList id集合
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:59
     */
    Boolean removeBatchByIds(ClientSession clientSession,String collectionName,Collection<Serializable> idList);

    /**
     * 获取单个，返回T类型的对象
     * <p style="color:red">注：如果查询到大于一条数据，会抛出{@link com.anwen.mongo.domain.MongoQueryException}异常</p>
     * @param collectionName 集合名
     * @return Map<String,Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:19
     */
    Map<String,Object> one(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    /**
     * 获取单个，返回T类型的对象
     * <p style="color:red">注：如果查询到大于一条数据，会抛出{@link com.anwen.mongo.domain.MongoQueryException}异常</p>
     * @param collectionName 集合名
     * @return Map<String,Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:19
     */
    Map<String,Object> one(ClientSession clientSession,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    /**
     * 获取单个，返回T类型的对象
     * <p style="color:red">注：如果查询到大于一条数据，会取第一条返回</p>
     * @param collectionName 集合名
     * @return Map< String, Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:20
     */
    Map<String,Object> limitOne(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    /**
     * 获取单个，返回T类型的对象
     * <p style="color:red">注：如果查询到大于一条数据，会取第一条返回</p>
     * @param collectionName 集合名
     * @return Map< String, Object>
     * @author JiaChaoYang
     * @date 2023/7/20 23:20
     */
    Map<String,Object> limitOne(ClientSession clientSession,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    /**
     * 获取总行数
     * @param collectionName 集合名
     * @param queryChainWrapper 条件构造器
     * @return {@link long}
     * @author JiaChaoYang
     * @date 2023/7/27 13:12
     */
    long count(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    /**
     * 获取总行数
     * @param collectionName 集合名
     * @param queryChainWrapper 条件构造器
     * @return {@link long}
     * @author JiaChaoYang
     * @date 2023/7/27 13:12
     */
    long count(ClientSession clientSession,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);
}
