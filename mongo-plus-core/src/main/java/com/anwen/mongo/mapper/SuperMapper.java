package com.anwen.mongo.mapper;

import com.anwen.mongo.aggregate.Aggregate;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.mapping.TypeReference;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.model.*;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

/**
 * 顶级Mapper接口
 *
 * @author anwen
 * @date 2024/6/26 下午2:03
 */
public interface SuperMapper {

    /**
     * 获取MongoPlusClient
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    MongoPlusClient getMongoPlusClient();

    /**
     * 获取绑定的转换器
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    MongoConverter getMongoConverter();

    /**
     * 获取执行器
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    Execute getExecute();

    /**
     * 添加单个
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    <T> boolean save(String database, String collectionName, T entity);

    /**
     * 添加多个
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    <T> Boolean saveBatch(String database,String collectionName,Collection<T> entityList);

    /**
     * 直接通过Bson条件更新，直接使用BaseMapper调用时，最好将构建的Bson，调用一下{@link MongoConverter#writeByUpdate(Object)}
     * @author anwen
     * @date 2024/5/4 下午1:21
     */
    Long update(String database,String collectionName,Bson queryBasic, Bson updateBasic);

    /**
     * 批量操作
     * @param writeModelList writeModelList
     * @return {@link Integer}
     * @author anwen
     * @date 2024/5/4 下午1:22
     */
    Integer bulkWrite(String database,String collectionName,List<WriteModel<Document>> writeModelList);

    /**
     * 根据queryWrapper修改entity
     * @author anwen
     * @date 2024/5/4 下午1:23
     */
    <T> Boolean update(String database,String collectionName,T entity, QueryChainWrapper<T,?> queryChainWrapper);

    /**
     * 是否存在
     * @param id id
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    boolean isExist(String database,String collectionName,Serializable id);

    /**
     * 根据条件查询是否存在
     * @param queryChainWrapper 条件
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    boolean isExist(String database,String collectionName,QueryChainWrapper<?,?> queryChainWrapper);


    /**
     * 修改，直接根据UpdateWrapper
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    Boolean update(String database,String collectionName,UpdateChainWrapper<?, ?> updateChainWrapper);

    /**
     * 删除，直接根据UpdateWrapper
     * @param updateChainWrapper 条件
     * @return {@link Boolean}
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    Boolean remove(String database,String collectionName,UpdateChainWrapper<?, ?> updateChainWrapper);

    /**
     * 根据条件删除
     * @param filter 条件
     * @return {@link Long}
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    Long remove(String database,String collectionName,Bson filter);

    /**
     * 根据条件查询总数
     * @param queryChainWrapper 条件
     * @return {@link long}
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    long count(String database,String collectionName,QueryChainWrapper<?, ?> queryChainWrapper);

    /**
     * 返回第N页
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    long recentPageCount(String database,String collectionName,List<CompareCondition> compareConditionList, Integer pageNum, Integer pageSize, Integer recentPageNum);


    /**
     * 查询所有
     * @param rClazz 返回的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <R> List<R> list(String database,String collectionName,Class<R> rClazz);

    /**
     * 查询所有
     * @param typeReference 返回的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <R> List<R> list(String database,String collectionName, TypeReference<R> typeReference);

    /**
     * 根据条件查询
     * @param queryChainWrapper 条件
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> list(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Class<R> rClazz);

    /**
     * 根据条件查询
     * @param queryChainWrapper 条件
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> list(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper, TypeReference<R> typeReference);

    /**
     * 管道查询
     * @param queryChainWrapper 管道构建
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> aggregateList(String database,String collectionName,AggregateChainWrapper<T, ?> queryChainWrapper,Class<R> rClazz);

    /**
     * 管道查询
     * @param queryChainWrapper 管道构建
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> aggregateList(String database,String collectionName,AggregateChainWrapper<T, ?> queryChainWrapper,TypeReference<R> typeReference);

    /**
     * 管道查询
     * @param aggregate 管道构建
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <R> List<R> aggregateList(String database,String collectionName,Aggregate<?> aggregate, Class<R> rClazz);

    /**
     * 管道查询
     * @param aggregate 管道构建
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <R> List<R> aggregateList(String database,String collectionName,Aggregate<?> aggregate, TypeReference<R> typeReference);

    /**
     * 根据条件查询单个
     * @param queryChainWrapper 条件
     * @return {@link T}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> R one(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper,Class<R> rClazz);

    /**
     * 根据条件查询单个
     * @param queryChainWrapper 条件
     * @return {@link T}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> R one(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper,TypeReference<R> typeReference);

    /**
     * 分页查询，如果queryWrapper有条件，查询会慢，因为需要重新进行count查询
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link PageResult <T>}
     * @author anwen
     * @date 2024/5/4 下午1:25
     */
    <T,R> PageResult<R> page(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize,Class<R> rClazz);

    /**
     * 分页查询，如果queryWrapper有条件，查询会慢，因为需要重新进行count查询
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link PageResult <T>}
     * @author anwen
     * @date 2024/5/4 下午1:25
     */
    <T,R> PageResult<R> page(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize,TypeReference<R> typeReference);

    /**
     * 分页查询，返回List，不进行count查询，比page查询效率高
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:26
     */
    <T,R> List<R> pageList(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<R> rClazz);

    /**
     * 分页查询，返回List，不进行count查询，比page查询效率高
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:26
     */
    <T,R> List<R> pageList(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, TypeReference<R> typeReference);

    /**
     * 分页查询，查询最近n页的数据
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近N页的数据
     * @return {@link PageResult<T>}
     * @author anwen
     * @date 2024/5/4 下午1:30
     */
    <T,R> PageResult<R> page(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum,Class<R> rClazz);

    /**
     * 分页查询，查询最近n页的数据
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近N页的数据
     * @return {@link PageResult<T>}
     * @author anwen
     * @date 2024/5/4 下午1:30
     */
    <T,R> PageResult<R> page(String database,String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum,TypeReference<R> typeReference);

    /**
     * 根据多个id查询
     * @param ids ids
     * @return {@link java.util.List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    <R> List<R> getByIds(String database,String collectionName,Collection<? extends Serializable> ids,Class<R> rClazz);

    /**
     * 根据多个id查询
     * @param ids ids
     * @return {@link java.util.List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    <R> List<R> getByIds(String database,String collectionName,Collection<? extends Serializable> ids,TypeReference<R> typeReference);

    /**
     * 根据id查询单个
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    <R> R getById(String database,String collectionName,Serializable id,Class<R> rClazz);

    /**
     * 根据id查询单个
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    <R> R getById(String database,String collectionName,Serializable id,TypeReference<R> typeReference);

    /**
     * 根据传入命令进行查询
     * @param command 命令，请传入mongo命令的find中完整的json
     * @return {@link java.util.List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    <R> List<R> queryCommand(String database,String collectionName,String command,Class<R> rClazz);

    /**
     * 根据传入命令进行查询
     * @param command 命令，请传入mongo命令的find中完整的json
     * @return {@link java.util.List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    <R> List<R> queryCommand(String database,String collectionName,String command,TypeReference<R> typeReference);

    /**
     * 根据某列进行查询
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    <R> List<R> getByColumn(String database,String collectionName,String column,Object value,Class<R> rClazz);

    /**
     * 根据某列进行查询
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    <R> List<R> getByColumn(String database,String collectionName,String column,Object value,TypeReference<R> typeReference);

    /**
     * 查询总数，estimatedDocumentCount高效率查询，但是不接收条件
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    long count(String database,String collectionName);

    String createIndex(String database,String collectionName,Bson bson);

    String createIndex(String database,String collectionName,Bson bson, IndexOptions indexOptions);

    List<String> createIndexes(String database,String collectionName,List<IndexModel> indexes);

    List<String> createIndexes(String database,String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions);

    List<Document> listIndexes(String database,String collectionName);

    void dropIndex(String database,String collectionName,String indexName);

    void dropIndex(String database,String collectionName,String indexName, DropIndexOptions dropIndexOptions);

    void dropIndex(String database,String collectionName,Bson keys);

    void dropIndex(String database,String collectionName,Bson keys,DropIndexOptions dropIndexOptions);

    void dropIndexes(String database,String collectionName);

    void dropIndexes(String database,String collectionName,DropIndexOptions dropIndexOptions);
    
}
