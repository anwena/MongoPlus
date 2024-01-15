package com.anwen.mongo.conditions.interfaces.Inject;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.ClientSession;
import com.mongodb.client.model.CreateIndexOptions;
import com.mongodb.client.model.DropIndexOptions;
import com.mongodb.client.model.IndexModel;
import com.mongodb.client.model.IndexOptions;
import org.bson.Document;
import org.bson.conversions.Bson;

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

    List<Map<String,Object>> list(String database,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    List<Map<String,Object>> list(ClientSession clientSession,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    List<Map<String,Object>> aggregateList(String collectionName, AggregateChainWrapper<Map<String,Object>,?> queryChainWrapper);

    List<Map<String,Object>> aggregateList(String database,String collectionName, AggregateChainWrapper<Map<String,Object>,?> queryChainWrapper);

    List<Map<String,Object>> aggregateList(ClientSession clientSession,String collectionName, AggregateChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(String collectionName,PageParam pageParam,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(String database,String collectionName,PageParam pageParam,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(ClientSession clientSession,String collectionName,PageParam pageParam,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(String collectionName , Integer pageNum, Integer pageSize,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(String database,String collectionName , Integer pageNum, Integer pageSize,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    PageResult<Map<String,Object>> page(ClientSession clientSession,String collectionName , Integer pageNum, Integer pageSize,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    Map<String,Object> getById(String collectionName ,Serializable id);

    Map<String,Object> getById(String database,String collectionName ,Serializable id);

    Map<String,Object> getById(ClientSession clientSession,String collectionName ,Serializable id);

    List<Map<String,Object>> getByIds(String collectionName , Collection<? extends Serializable> ids);

    List<Map<String,Object>> getByIds(String database,String collectionName , Collection<? extends Serializable> ids);

    List<Map<String,Object>> getByIds(ClientSession clientSession,String collectionName , Collection<? extends Serializable> ids);

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
    Boolean save(String database,String collectionName , Map<String,Object> entityMap);

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
    Boolean saveBatch(String database,String collectionName , Collection<Map<String,Object>> entityMapList);

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
    Boolean saveOrUpdate(String database,String collectionName , Map<String,Object> entityMap);

    /**
     * 根据传入wrapper条件判断添加还是删除，传递_id并不会修改
     * @param entityMap map对象
     * @param queryChainWrapper 条件
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2024/1/15 23:15
     */
    Boolean saveOrUpdateWrapper(String collectionName,Map<String,Object> entityMap,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    /**
     * 根据传入wrapper条件判断添加还是删除，传递_id并不会修改
     * @param entityMap map对象
     * @param queryChainWrapper 条件
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2024/1/15 23:15
     */
    Boolean saveOrUpdateWrapper(String database,String collectionName,Map<String,Object> entityMap,QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

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
    Boolean saveOrUpdateBatch(String database,String collectionName , Collection<Map<String,Object>> entityMapList);

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
    Boolean updateById(String database,String collectionName , Map<String,Object> entityMap);

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
    Boolean updateBatchByIds(String database,String collectionName , Collection<Map<String,Object>> entityMapList);

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
    Boolean updateByColumn(String database,String collectionName,Map<String,Object> entityMap, String column);

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
    Boolean removeById(String database,String collectionName,Serializable id);

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
    Boolean removeByColumn(String database,String collectionName,String column,String value);

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
    Boolean removeBatchByIds(String collectionName,Collection<? extends Serializable> idList);

    /**
     * 根据id批量删除
     * @param idList id集合
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:59
     */
    Boolean removeBatchByIds(String database,String collectionName,Collection<? extends Serializable> idList);

    /**
     * 根据id批量删除
     * @param idList id集合
     * @param collectionName 集合名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:59
     */
    Boolean removeBatchByIds(ClientSession clientSession,String collectionName,Collection<? extends Serializable> idList);

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
    Map<String,Object> one(String database,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

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
    Map<String,Object> limitOne(String database,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

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
    long count(String database,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    /**
     * 获取总行数
     * @param collectionName 集合名
     * @param queryChainWrapper 条件构造器
     * @return {@link long}
     * @author JiaChaoYang
     * @date 2023/7/27 13:12
     */
    long count(ClientSession clientSession,String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper);

    /**
     * 根据某一列查询
     * @param field 字段
     * @param fieldValue 字段值
     * @return T
     * @author JiaChaoYang
     * @date 2023/10/19 23:30
     */
    List<Map<String,Object>> getByColumn(String collectionName,String field,Object fieldValue);

    /**
     * 根据某一列查询
     * @param field 字段
     * @param fieldValue 字段值
     * @return T
     * @author JiaChaoYang
     * @date 2023/10/19 23:30
     */
    List<Map<String,Object>> getByColumn(String database,String collectionName,String field,Object fieldValue);

    List<Map<String,Object>> getByColumn(ClientSession clientSession,String collection,String field,Object fieldValue);

    /**
     * 根据条件删除
     * @param updateChainWrapper 条件
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2023/10/20 0:51
     */
    Boolean remove(String collectionName,UpdateChainWrapper<Map<String,Object>,?> updateChainWrapper);

    /**
     * 根据条件删除
     * @param updateChainWrapper 条件
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2023/10/20 0:51
     */
    Boolean remove(String database,String collectionName,UpdateChainWrapper<Map<String,Object>,?> updateChainWrapper);

    /**
     * 根据条件删除
     * @param updateChainWrapper 条件
     * @param clientSession 事务接口
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2023/10/20 0:51
     */
    Boolean remove(ClientSession clientSession,String collectionName,UpdateChainWrapper<Map<String,Object>,?> updateChainWrapper);

    /**
     * 根据条件修改
     * @param updateChainWrapper 条件
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2023/10/20 0:51
     */
    Boolean update(String collectionName,UpdateChainWrapper<Map<String,Object>,?> updateChainWrapper);

    /**
     * 根据条件修改
     * @param updateChainWrapper 条件
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2023/10/20 0:51
     */
    Boolean update(String database,String collectionName,UpdateChainWrapper<Map<String,Object>,?> updateChainWrapper);

    /**
     * 根据条件修改
     * @param updateChainWrapper 条件
     * @param clientSession 事务接口
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2023/10/20 0:51
     */
    Boolean update(ClientSession clientSession,String collectionName,UpdateChainWrapper<Map<String,Object>,?> updateChainWrapper);

    @Deprecated
    List<Map<String,Object>> sql(String collectionName,String sql);

    @Deprecated
    List<Map<String,Object>> sql(ClientSession clientSession, String collectionName, String sql);

    /**
     * 命令查询接口，传入值为json，如{eq:XXX}
     * @param command 命令json
     * @return java.util.List<T>
     * @author JiaChaoYang
     * @date 2023/12/30 23:28
     */
    List<Map<String,Object>> queryCommand(String collectionName,String command);

    /**
     * 命令查询接口，传入值为json，如{eq:XXX}
     * @param command 命令json
     * @return java.util.List<T>
     * @author JiaChaoYang
     * @date 2023/12/30 23:28
     */
    List<Map<String,Object>> queryCommand(String database,String collectionName,String command);

    /**
     * 创建索引，携带事务
     * @param bson 描述索引键的对象，该对象不能为 null
     * @return java.lang.String
     * @author JiaChaoYang
     * @date 2023/11/15 14:04
     */
    String createIndex(ClientSession clientSession,String collectionName, Bson bson);

    /**
     * 创建索引
     * @param bson 描述索引键的对象，该对象不能为 null
     * @return java.lang.String
     * @author JiaChaoYang
     * @date 2023/11/15 14:04
     */
    String createIndex(String collectionName,Bson bson);

    /**
     * 创建索引
     * @param bson 描述索引键的对象，该对象不能为 null
     * @return java.lang.String
     * @author JiaChaoYang
     * @date 2023/11/15 14:04
     */
    String createIndex(String database,String collectionName,Bson bson);

    /**
     * 使用给定的键和选项创建索引。
     * @param clientSession 要与此操作关联的客户端会话
     * @param bson 描述索引键的对象，该对象不能为 null
     * @param indexOptions 指数的选项
     * @return java.lang.String
     * @author JiaChaoYang
     * @date 2023/11/15 15:37
     */
    String createIndex(ClientSession clientSession,String collectionName, Bson bson, IndexOptions indexOptions);

    /**
     * 使用给定的键和选项创建索引。
     * @param bson 描述索引键的对象，该对象不能为 null
     * @param indexOptions 指数的选项
     * @return java.lang.String
     * @author JiaChaoYang
     * @date 2023/11/15 15:37
     */
    String createIndex(String collectionName,Bson bson, IndexOptions indexOptions);

    /**
     * 使用给定的键和选项创建索引。
     * @param bson 描述索引键的对象，该对象不能为 null
     * @param indexOptions 指数的选项
     * @return java.lang.String
     * @author JiaChaoYang
     * @date 2023/11/15 15:37
     */
    String createIndex(String database,String collectionName,Bson bson, IndexOptions indexOptions);

    /**
     * 创建多个索引
     * @param indexes 索引列表
     * @return java.util.List<java.lang.String>
     * @author JiaChaoYang
     * @date 2023/11/15 15:34
     */
    List<String> createIndexes(String collectionName,List<IndexModel> indexes);

    /**
     * 创建多个索引
     * @param indexes 索引列表
     * @return java.util.List<java.lang.String>
     * @author JiaChaoYang
     * @date 2023/11/15 15:34
     */
    List<String> createIndexes(String database,String collectionName,List<IndexModel> indexes);

    /**
     * 创建多个索引
     * @param indexes 索引列表
     * @param createIndexOptions 创建索引时要使用的选项
     * @return java.util.List<java.lang.String> 索引名称列表
     * @author JiaChaoYang
     * @date 2023/11/15 15:34
     */
    List<String> createIndexes(String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions);

    /**
     * 创建多个索引
     * @param indexes 索引列表
     * @param createIndexOptions 创建索引时要使用的选项
     * @return java.util.List<java.lang.String> 索引名称列表
     * @author JiaChaoYang
     * @date 2023/11/15 15:34
     */
    List<String> createIndexes(String database,String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions);

    /**
     * 创建多个索引
     * @param clientSession 要与此操作关联的客户端会话
     * @param indexes 索引列表
     * @return java.util.List<java.lang.String> 索引名称列表
     * @author JiaChaoYang
     * @date 2023/11/15 15:35
     */
    List<String> createIndexes(ClientSession clientSession, String collectionName,List<IndexModel> indexes);

    /**
     * 创建多个索引
     * @param clientSession 要与此操作关联的客户端会话
     * @param indexes 索引列表
     * @param createIndexOptions 创建索引时要使用的选项
     * @return java.util.List<java.lang.String> 索引名称列表
     * @author JiaChaoYang
     * @date 2023/11/15 15:36
     */
    List<String> createIndexes(ClientSession clientSession,String collectionName, List<IndexModel> indexes, CreateIndexOptions createIndexOptions);

    /**
     * 获取此集合中的所有索引。
     *
     * @return 列表索引可迭代接口
     */
    List<Document> listIndexes(String collectionName);

    /**
     * 获取此集合中的所有索引。
     *
     * @return 列表索引可迭代接口
     */
    List<Document> listIndexes(String database,String collectionName);

    /**
     * 获取此集合中的所有索引。
     *
     * @param clientSession 要与此操作关联的客户端会话
     * @return 列表索引可迭代接口
     */
    List<Document> listIndexes(ClientSession clientSession,String collectionName);

    /**
     * 删除给定其名称的索引。
     *
     * @param indexName 要删除的索引的名称
     */
    void dropIndex(String collectionName,String indexName);

    /**
     * 删除给定其名称的索引。
     *
     * @param indexName 要删除的索引的名称
     */
    void dropIndex(String database,String collectionName,String indexName);

    /**
     * 删除给定其名称的索引。
     *
     * @param indexName 要删除的索引的名称
     * @param dropIndexOptions 删除索引时要使用的选项
     */
    void dropIndex(String collectionName,String indexName, DropIndexOptions dropIndexOptions);

    /**
     * 删除给定其名称的索引。
     *
     * @param indexName 要删除的索引的名称
     * @param dropIndexOptions 删除索引时要使用的选项
     */
    void dropIndex(String database,String collectionName,String indexName, DropIndexOptions dropIndexOptions);

    /**
     * 在给定用于创建索引的键的情况下删除索引。
     *
     * @param keys 要删除的索引的键
     */
    void dropIndex(String collectionName,Bson keys);

    /**
     * 在给定用于创建索引的键的情况下删除索引。
     *
     * @param keys 要删除的索引的键
     */
    void dropIndex(String database,String collectionName,Bson keys);

    /**
     * 在给定用于创建索引的键的情况下删除索引。
     *
     * @param keys 要删除的索引的键
     * @param dropIndexOptions 删除索引时要使用的选项
     * @since 3.6
     */
    void dropIndex(String collectionName,Bson keys, DropIndexOptions dropIndexOptions);

    /**
     * 在给定用于创建索引的键的情况下删除索引。
     *
     * @param keys 要删除的索引的键
     * @param dropIndexOptions 删除索引时要使用的选项
     * @since 3.6
     */
    void dropIndex(String database,String collectionName,Bson keys, DropIndexOptions dropIndexOptions);

    /**
     * 删除给定其名称的索引。
     *
     * @param clientSession 要与此操作关联的客户端会话
     * @param indexName 要删除的索引的名称
     */
    void dropIndex(ClientSession clientSession,String collectionName, String indexName);

    /**
     * 在给定用于创建索引的键的情况下删除索引。
     *
     * @param clientSession 要与此操作关联的客户端会话
     * @param keys 要删除的索引的键
     */
    void dropIndex(ClientSession clientSession,String collectionName, Bson keys);

    /**
     * 删除给定其名称的索引。
     *
     * @param clientSession 要与此操作关联的客户端会话
     * @param indexName 要删除的索引的名称
     * @param dropIndexOptions 删除索引时要使用的选项
     */
    void dropIndex(ClientSession clientSession, String collectionName,String indexName, DropIndexOptions dropIndexOptions);

    /**
     * 在给定用于创建索引的键的情况下删除索引。
     *
     * @param clientSession 要与此操作关联的客户端会话
     * @param keys 要删除的索引的键
     * @param dropIndexOptions 删除索引时要使用的选项
     */
    void dropIndex(ClientSession clientSession, String collectionName,Bson keys, DropIndexOptions dropIndexOptions);

    /**
     * 删除此集合上的所有索引，但 _id 上的默认值除外。
     */
    void dropIndexes(String collectionName);

    /**
     * 删除此集合上的所有索引，但 _id 上的默认值除外。
     */
    void dropIndexes(String database,String collectionName);

    /**
     * 删除此集合上的所有索引，但 _id 上的默认值除外。
     *
     * @param clientSession 要与此操作关联的客户端会话
     */
    void dropIndexes(ClientSession clientSession,String collectionName);

    /**
     * 删除此集合上的所有索引，但 _id 上的默认值除外。
     *
     * @param dropIndexOptions 删除索引时要使用的选项
     * @since 3.6
     */
    void dropIndexes(String collectionName,DropIndexOptions dropIndexOptions);

    /**
     * 删除此集合上的所有索引，但 _id 上的默认值除外。
     *
     * @param dropIndexOptions 删除索引时要使用的选项
     * @since 3.6
     */
    void dropIndexes(String database,String collectionName,DropIndexOptions dropIndexOptions);

    /**
     * 删除此集合上的所有索引，但 _id 上的默认值除外。
     *
     * @param clientSession 要与此操作关联的客户端会话
     * @param dropIndexOptions 删除索引时要使用的选项
     */
    void dropIndexes(ClientSession clientSession,String collectionName, DropIndexOptions dropIndexOptions);

}
