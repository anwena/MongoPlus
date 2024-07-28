package com.anwen.mongo.mapper;

import com.anwen.mongo.aggregate.Aggregate;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
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
 * mapper层
 * TODO 名称待定，方法留CRUD，不同数据库实现该接口
 * @author JiaChaoYang
 **/
public interface BaseMapper extends Mapper {

    /**
     * 添加单个
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    <T> boolean save(T entity);

    /**
     * 添加多个
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    <T> Boolean saveBatch(Collection<T> entityList);

    /**
     * 直接通过Bson条件更新，直接使用BaseMapper调用时，最好将构建的Bson，调用一下{@link MongoConverter#writeByUpdate(Object)}
     * @author anwen
     * @date 2024/5/4 下午1:21
     */
    @Deprecated
    Long update(Bson queryBasic,Bson updateBasic,Class<?> clazz);

    /**
     * 批量操作
     * @param writeModelList writeModelList
     * @param clazz class
     * @return {@link Integer}
     * @author anwen
     * @date 2024/5/4 下午1:22
     */
    Integer bulkWrite(List<WriteModel<Document>> writeModelList,Class<?> clazz);

    /**
     * 根据queryWrapper修改entity
     * @author anwen
     * @date 2024/5/4 下午1:23
     */
    <T> Boolean update(T entity,QueryChainWrapper<T,?> queryChainWrapper);

    /**
     * 是否存在
     * @param id id
     * @param clazz class
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    boolean isExist(Serializable id,Class<?> clazz);

    /**
     * 根据条件查询是否存在
     * @param queryChainWrapper 条件
     * @param clazz class
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    boolean isExist(QueryChainWrapper<?,?> queryChainWrapper,Class<?> clazz);


    /**
     * 修改，直接根据UpdateWrapper
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    Boolean update(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz);

    /**
     * 删除，直接根据UpdateWrapper
     * @param updateChainWrapper 条件
     * @param clazz class
     * @return {@link Boolean}
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    Boolean remove(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz);

    /**
     * 根据条件删除
     * @param filter 条件
     * @param clazz class
     * @return {@link Long}
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    Long remove(Bson filter,Class<?> clazz);

    /**
     * 根据条件查询总数
     * @param queryChainWrapper 条件
     * @param clazz class
     * @return {@link long}
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    long count(QueryChainWrapper<?, ?> queryChainWrapper,Class<?> clazz);

    /**
     * 返回第N页
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    long recentPageCount(List<CompareCondition> compareConditionList,Class<?> clazz, Integer pageNum, Integer pageSize, Integer recentPageNum);


    /**
     * 查询所有
     * @param clazz 操作的class
     * @param rClazz 返回的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> list(Class<T> clazz,Class<R> rClazz);

    /**
     * 查询所有
     * @param clazz 操作的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T> List<T> list(Class<T> clazz){
        return list(clazz, clazz);
    }

    /**
     * 查询所有
     * @param clazz 操作的class
     * @param typeReference 返回的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> list(Class<T> clazz, TypeReference<R> typeReference);

    /**
     * 根据条件查询
     * @param queryChainWrapper 条件
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> list(QueryChainWrapper<T,?> queryChainWrapper, Class<T> clazz, Class<R> rClazz);

    /**
     * 根据条件查询
     * @param queryChainWrapper 条件
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T> List<T> list(QueryChainWrapper<T,?> queryChainWrapper, Class<T> clazz){
        return list(queryChainWrapper, clazz, clazz);
    }

    /**
     * 根据条件查询
     * @param queryChainWrapper 条件
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> list(QueryChainWrapper<T,?> queryChainWrapper, Class<T> clazz, TypeReference<R> typeReference);

    /**
     * 管道查询
     * @param queryChainWrapper 管道构建
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, Class<R> rClazz);

    /**
     * 管道查询
     * @param queryChainWrapper 管道构建
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T> List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz){
        return aggregateList(queryChainWrapper, clazz, clazz);
    }

    /**
     * 管道查询
     * @param queryChainWrapper 管道构建
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz, TypeReference<R> typeReference);

    /**
     * 管道查询
     * @param aggregate 管道构建
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> aggregateList(Aggregate<?> aggregate, Class<T> clazz, Class<R> rClazz);

    /**
     * 管道查询
     * @param aggregate 管道构建
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T> List<T> aggregateList(Aggregate<?> aggregate, Class<T> clazz){
        return aggregateList(aggregate, clazz, clazz);
    }

    /**
     * 管道查询
     * @param aggregate 管道构建
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> List<R> aggregateList(Aggregate<?> aggregate, Class<T> clazz, TypeReference<R> typeReference);

    /**
     * 根据条件查询单个
     * @param queryChainWrapper 条件
     * @param clazz class
     * @return {@link T}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> R one(QueryChainWrapper<T,?> queryChainWrapper,Class<T> clazz,Class<R> rClazz);

    /**
     * 根据条件查询单个
     * @param queryChainWrapper 条件
     * @param clazz class
     * @return {@link T}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T> T one(QueryChainWrapper<T,?> queryChainWrapper,Class<T> clazz){
        return one(queryChainWrapper, clazz, clazz);
    }

    /**
     * 根据条件查询单个
     * @param queryChainWrapper 条件
     * @param clazz class
     * @return {@link T}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    <T,R> R one(QueryChainWrapper<T,?> queryChainWrapper,Class<T> clazz,TypeReference<R> typeReference);

    /**
     * 分页查询
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param clazz class
     * @return {@link PageResult <T>}
     * @author anwen
     * @date 2024/5/4 下午1:25
     */
    <T,R> PageResult<R> page(Integer pageNum, Integer pageSize, Class<T> clazz,Class<R> rClazz);

    /**
     * 分页查询
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param clazz class
     * @return {@link PageResult <T>}
     * @author anwen
     * @date 2024/5/4 下午1:25
     */
    default <T> PageResult<T> page(Integer pageNum, Integer pageSize, Class<T> clazz){
        return page(pageNum, pageSize, clazz, clazz);
    }

    /**
     * 分页查询，如果queryWrapper有条件，查询会慢，因为需要重新进行count查询
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param clazz class
     * @return {@link PageResult <T>}
     * @author anwen
     * @date 2024/5/4 下午1:25
     */
    <T,R> PageResult<R> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz,Class<R> rClazz);

    /**
     * 分页查询，如果queryWrapper有条件，查询会慢，因为需要重新进行count查询
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param clazz class
     * @return {@link PageResult <T>}
     * @author anwen
     * @date 2024/5/4 下午1:25
     */
    <T,R> PageResult<R> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz,TypeReference<R> typeReference);

    /**
     * 分页查询，返回List，不进行count查询，比page查询效率高
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:26
     */
    default <T,R> List<R> pageList(Integer pageNum, Integer pageSize, Class<T> clazz,Class<R> rClazz){
        return pageList(new QueryWrapper<>(),pageNum, pageSize, clazz, rClazz);
    }

    /**
     * 分页查询，返回List，不进行count查询，比page查询效率高
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:26
     */
    default <T> List<T> pageList(Integer pageNum, Integer pageSize, Class<T> clazz){
        return pageList(pageNum, pageSize, clazz, clazz);
    }

    /**
     * 分页查询，返回List，不进行count查询，比page查询效率高
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:26
     */
    <T,R> List<R> pageList(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz,Class<R> rClazz);

    /**
     * 分页查询，返回List，不进行count查询，比page查询效率高
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param clazz class
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:26
     */
    <T,R> List<R> pageList(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz,TypeReference<R> typeReference);

    /**
     * 分页查询，查询最近n页的数据
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近N页的数据
     * @param clazz class
     * @return {@link PageResult<T>}
     * @author anwen
     * @date 2024/5/4 下午1:30
     */
    default <T,R> PageResult<R> page(Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz,Class<R> rClazz){
        return page(new QueryWrapper<>(),pageNum, pageSize, recentPageNum, clazz, rClazz);
    }

    /**
     * 分页查询，查询最近n页的数据
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近N页的数据
     * @param clazz class
     * @return {@link PageResult<T>}
     * @author anwen
     * @date 2024/5/4 下午1:30
     */
    default <T> PageResult<T> page(Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz){
        return page(pageNum, pageSize, recentPageNum, clazz, clazz);
    }

    /**
     * 分页查询，查询最近n页的数据
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近N页的数据
     * @param clazz class
     * @return {@link PageResult<T>}
     * @author anwen
     * @date 2024/5/4 下午1:30
     */
    <T,R> PageResult<R> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz,Class<R> rClazz);

    /**
     * 分页查询，查询最近n页的数据
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近N页的数据
     * @param clazz class
     * @return {@link PageResult<T>}
     * @author anwen
     * @date 2024/5/4 下午1:30
     */
    <T,R> PageResult<R> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz,TypeReference<R> typeReference);

    /**
     * 根据多个id查询
     * @param ids ids
     * @param clazz class
     * @return {@link java.util.List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    <T,R> List<R> getByIds(Collection<? extends Serializable> ids, Class<T> clazz,Class<R> rClazz);

    /**
     * 根据多个id查询
     * @param ids ids
     * @param clazz class
     * @return {@link java.util.List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    default <T> List<T> getByIds(Collection<? extends Serializable> ids, Class<T> clazz){
        return getByIds(ids, clazz, clazz);
    }

    /**
     * 根据多个id查询
     * @param ids ids
     * @param clazz class
     * @return {@link java.util.List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    <T,R> List<R> getByIds(Collection<? extends Serializable> ids, Class<T> clazz,TypeReference<R> typeReference);

    /**
     * 根据id查询单个
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    <T,R> R getById(Serializable id,Class<T> clazz,Class<R> rClazz);

    /**
     * 根据id查询单个
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    default <T> T getById(Serializable id,Class<T> clazz){
        return getById(id, clazz, clazz);
    }

    /**
     * 根据id查询单个
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    <T,R> R getById(Serializable id,Class<T> clazz,TypeReference<R> typeReference);

    /**
     * 根据传入命令进行查询
     * @param command 命令，请传入mongo命令的find中完整的json
     * @param clazz class
     * @return {@link java.util.List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    <T,R> List<R> queryCommand(String command,Class<T> clazz,Class<R> rClazz);

    /**
     * 根据传入命令进行查询
     * @param command 命令，请传入mongo命令的find中完整的json
     * @param clazz class
     * @return {@link java.util.List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    default <T> List<T> queryCommand(String command,Class<T> clazz){
        return queryCommand(command, clazz, clazz);
    }

    /**
     * 根据传入命令进行查询
     * @param command 命令，请传入mongo命令的find中完整的json
     * @param clazz class
     * @return {@link java.util.List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    <T,R> List<R> queryCommand(String command,Class<T> clazz,TypeReference<R> typeReference);

    /**
     * 根据某列进行查询
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    <T,R> List<R> getByColumn(String column,Object value,Class<T> clazz,Class<R> rClazz);

    /**
     * 根据某列进行查询
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    default <T> List<T> getByColumn(String column,Object value,Class<T> clazz){
        return getByColumn(column, value, clazz, clazz);
    }

    /**
     * 根据某列进行查询
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    <T,R> List<R> getByColumn(String column,Object value,Class<T> clazz,TypeReference<R> typeReference);

    /**
     * 查询总数，estimatedDocumentCount高效率查询，但是不接收条件
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    long count(Class<?> clazz);

    String createIndex(Bson bson,Class<?> clazz);

    String createIndex(Bson bson, IndexOptions indexOptions, Class<?> clazz);

    List<String> createIndexes(List<IndexModel> indexes,Class<?> clazz);

    List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions,Class<?> clazz);

    List<Document> listIndexes(Class<?> clazz);

    void dropIndex(String indexName,Class<?> clazz);

    void dropIndex(String indexName,DropIndexOptions dropIndexOptions,Class<?> clazz);

    void dropIndex(Bson keys,Class<?> clazz);

    void dropIndex(Bson keys,DropIndexOptions dropIndexOptions,Class<?> clazz);

    void dropIndexes(Class<?> clazz);

    void dropIndexes(DropIndexOptions dropIndexOptions,Class<?> clazz);

}
