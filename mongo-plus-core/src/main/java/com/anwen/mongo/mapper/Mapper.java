package com.anwen.mongo.mapper;

import com.anwen.mongo.aggregate.Aggregate;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
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

import static com.anwen.mongo.toolkit.StringPool.EMPTY;

public interface Mapper extends SuperMapper {

    /**
     * 添加单个
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    default <T> boolean save(String collectionName, T entity){
        return save(EMPTY,collectionName,entity);
    }

    /**
     * 添加多个
     * @author anwen
     * @date 2024/5/4 下午1:20
     */
    default <T> Boolean saveBatch(String collectionName, Collection<T> entityList){
        return saveBatch(EMPTY,collectionName,entityList);
    }

    /**
     * 直接通过Bson条件更新，直接使用BaseMapper调用时，最好将构建的Bson，调用一下{@link MongoConverter#writeByUpdate(Object)}
     * @author anwen
     * @date 2024/5/4 下午1:21
     */
    default Long update(String collectionName, Bson queryBasic, Bson updateBasic){
        return update(EMPTY,collectionName,queryBasic,updateBasic);
    }

    /**
     * 批量操作
     * @param writeModelList writeModelList
     * @return {@link Integer}
     * @author anwen
     * @date 2024/5/4 下午1:22
     */
    default Integer bulkWrite(String collectionName, List<WriteModel<Document>> writeModelList){
        return bulkWrite(EMPTY,collectionName,writeModelList);
    }

    /**
     * 根据queryWrapper修改entity
     * @author anwen
     * @date 2024/5/4 下午1:23
     */
    default <T> Boolean update(String collectionName,T entity, QueryChainWrapper<T,?> queryChainWrapper){
        return update(EMPTY,collectionName,entity,queryChainWrapper);
    }

    /**
     * 是否存在
     * @param id id
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    default boolean isExist(String collectionName, Serializable id){
        return isExist(EMPTY,collectionName,id);
    }

    /**
     * 根据条件查询是否存在
     * @param queryChainWrapper 条件
     * @return {@link boolean}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    default boolean isExist(String collectionName,QueryChainWrapper<?,?> queryChainWrapper){
        return isExist(EMPTY,collectionName,queryChainWrapper);
    }

    /**
     * 修改，直接根据UpdateWrapper
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    default Boolean update(String collectionName, UpdateChainWrapper<?, ?> updateChainWrapper){
        return update(EMPTY,collectionName,updateChainWrapper);
    }

    /**
     * 删除，直接根据UpdateWrapper
     * @param updateChainWrapper 条件
     * @return {@link Boolean}
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    default Boolean remove(String collectionName,UpdateChainWrapper<?, ?> updateChainWrapper){
        return remove(EMPTY,collectionName,updateChainWrapper);
    }

    /**
     * 根据条件删除
     * @param filter 条件
     * @return {@link Long}
     * @author anwen
     * @date 2024/5/4 下午1:32
     */
    default Long remove(String collectionName,Bson filter){
        return remove(EMPTY,collectionName,filter);
    }

    /**
     * 根据条件查询总数
     * @param queryChainWrapper 条件
     * @return {@link long}
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    default long count(String collectionName,QueryChainWrapper<?, ?> queryChainWrapper){
        return count(EMPTY,collectionName,queryChainWrapper);
    }

    /**
     * 返回第N页
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    default long recentPageCount(String collectionName, List<CompareCondition> compareConditionList, Integer pageNum, Integer pageSize, Integer recentPageNum){
        return recentPageCount(EMPTY,collectionName,compareConditionList,pageNum,pageSize,recentPageNum);
    }


    /**
     * 查询所有
     * @param rClazz 返回的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <R> List<R> list(String collectionName,Class<R> rClazz){
        return list(EMPTY,collectionName,rClazz);
    }

    /**
     * 查询所有
     * @param typeReference 返回的class
     * @return {@link List <T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <R> List<R> list(String collectionName, TypeReference<R> typeReference){
        return list(EMPTY,collectionName,typeReference);
    }

    /**
     * 根据条件查询
     * @param queryChainWrapper 条件
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T,R> List<R> list(String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Class<R> rClazz){
        return list(EMPTY,collectionName,queryChainWrapper,rClazz);
    }

    /**
     * 根据条件查询
     * @param queryChainWrapper 条件
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T,R> List<R> list(String collectionName,QueryChainWrapper<T,?> queryChainWrapper, TypeReference<R> typeReference){
        return list(EMPTY,collectionName,queryChainWrapper,typeReference);
    }

    /**
     * 管道查询
     * @param queryChainWrapper 管道构建
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T,R> List<R> aggregateList(String collectionName, AggregateChainWrapper<T, ?> queryChainWrapper, Class<R> rClazz){
        return aggregateList(EMPTY,collectionName,queryChainWrapper,rClazz);
    }

    /**
     * 管道查询
     * @param queryChainWrapper 管道构建
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T,R> List<R> aggregateList(String collectionName,AggregateChainWrapper<T, ?> queryChainWrapper,TypeReference<R> typeReference){
        return aggregateList(EMPTY,collectionName,queryChainWrapper,typeReference);
    }

    /**
     * 管道查询
     * @param aggregate 管道构建
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <R> List<R> aggregateList(String collectionName, Aggregate<?> aggregate, Class<R> rClazz){
        return aggregateList(EMPTY,collectionName,aggregate,rClazz);
    }

    /**
     * 管道查询
     * @param aggregate 管道构建
     * @return {@link List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <R> List<R> aggregateList(String collectionName,Aggregate<?> aggregate, TypeReference<R> typeReference){
        return aggregateList(EMPTY,collectionName,aggregate,typeReference);
    }

    /**
     * 根据条件查询单个
     * @param queryChainWrapper 条件
     * @return {@link T}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T,R> R one(String collectionName,QueryChainWrapper<T,?> queryChainWrapper,Class<R> rClazz){
        return one(EMPTY,collectionName,queryChainWrapper,rClazz);
    }

    /**
     * 根据条件查询单个
     * @param queryChainWrapper 条件
     * @return {@link T}
     * @author anwen
     * @date 2024/5/4 下午1:24
     */
    default <T,R> R one(String collectionName,QueryChainWrapper<T,?> queryChainWrapper,TypeReference<R> typeReference){
        return one(EMPTY,collectionName,queryChainWrapper,typeReference);
    }

    /**
     * 分页查询，如果queryWrapper有条件，查询会慢，因为需要重新进行count查询
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link PageResult <T>}
     * @author anwen
     * @date 2024/5/4 下午1:25
     */
    default <T,R> PageResult<R> page(String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize,Class<R> rClazz){
        return page(EMPTY,collectionName,queryChainWrapper,pageNum,pageSize,rClazz);
    }

    /**
     * 分页查询，如果queryWrapper有条件，查询会慢，因为需要重新进行count查询
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link PageResult <T>}
     * @author anwen
     * @date 2024/5/4 下午1:25
     */
    default <T,R> PageResult<R> page(String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize,TypeReference<R> typeReference){
        return page(EMPTY,collectionName,queryChainWrapper,pageNum,pageSize,typeReference);
    }

    /**
     * 分页查询，返回List，不进行count查询，比page查询效率高
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:26
     */
    default <T,R> List<R> pageList(String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<R> rClazz){
        return pageList(EMPTY,collectionName,queryChainWrapper,pageNum,pageSize,rClazz);
    }

    /**
     * 分页查询，返回List，不进行count查询，比page查询效率高
     * @param queryChainWrapper 条件
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link List<T>}
     * @author anwen
     * @date 2024/5/4 下午1:26
     */
    default <T,R> List<R> pageList(String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, TypeReference<R> typeReference){
        return pageList(EMPTY,collectionName,queryChainWrapper,pageNum,pageSize,typeReference);
    }

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
    default <T,R> PageResult<R> page(String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum,Class<R> rClazz){
        return page(EMPTY,collectionName,queryChainWrapper,pageNum,pageSize,recentPageNum,rClazz);
    }

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
    default <T,R> PageResult<R> page(String collectionName,QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum,TypeReference<R> typeReference){
        return page(EMPTY,collectionName,queryChainWrapper,pageNum,pageSize,recentPageNum,typeReference);
    }

    /**
     * 根据多个id查询
     * @param ids ids
     * @return {@link java.util.List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    default <R> List<R> getByIds(String collectionName,Collection<? extends Serializable> ids,Class<R> rClazz){
        return getByIds(EMPTY,collectionName,ids,rClazz);
    }

    /**
     * 根据多个id查询
     * @param ids ids
     * @return {@link java.util.List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    default <R> List<R> getByIds(String collectionName,Collection<? extends Serializable> ids,TypeReference<R> typeReference){
        return getByIds(EMPTY,collectionName,ids,typeReference);
    }

    /**
     * 根据id查询单个
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    default <R> R getById(String collectionName,Serializable id,Class<R> rClazz){
        return getById(EMPTY,collectionName,id,rClazz);
    }

    /**
     * 根据id查询单个
     * @author anwen
     * @date 2024/5/4 下午1:31
     */
    default <R> R getById(String collectionName,Serializable id,TypeReference<R> typeReference){
        return getById(EMPTY,collectionName,id,typeReference);
    }

    /**
     * 根据传入命令进行查询
     * @param command 命令，请传入mongo命令的find中完整的json
     * @return {@link java.util.List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    default <R> List<R> queryCommand(String collectionName,String command,Class<R> rClazz){
        return queryCommand(EMPTY,collectionName,command,rClazz);
    }

    /**
     * 根据传入命令进行查询
     * @param command 命令，请传入mongo命令的find中完整的json
     * @return {@link java.util.List<R>}
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    default <R> List<R> queryCommand(String collectionName,String command,TypeReference<R> typeReference){
        return queryCommand(EMPTY,collectionName,command,typeReference);
    }

    /**
     * 根据某列进行查询
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    default <R> List<R> getByColumn(String collectionName,String column,Object value,Class<R> rClazz){
        return getByColumn(EMPTY,collectionName,column,value,rClazz);
    }

    /**
     * 根据某列进行查询
     * @author anwen
     * @date 2024/5/4 下午1:34
     */
    default <R> List<R> getByColumn(String collectionName,String column,Object value,TypeReference<R> typeReference){
        return getByColumn(EMPTY,collectionName,column,value,typeReference);
    }

    /**
     * 查询总数，estimatedDocumentCount高效率查询，但是不接收条件
     * @author anwen
     * @date 2024/5/4 下午1:33
     */
    default long count(String collectionName){
        return count(EMPTY,collectionName);
    }

    default String createIndex(String collectionName,Bson bson){
        return createIndex(EMPTY,collectionName,bson);
    }

    default String createIndex(String collectionName,Bson bson, IndexOptions indexOptions){
        return createIndex(EMPTY,collectionName,bson,indexOptions);
    }

    default List<String> createIndexes(String collectionName,List<IndexModel> indexes){
        return createIndexes(EMPTY,collectionName,indexes);
    }

    default List<String> createIndexes(String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions){
        return createIndexes(EMPTY,collectionName,indexes,createIndexOptions);
    }

    default List<Document> listIndexes(String collectionName){
        return listIndexes(EMPTY,collectionName);
    }

    default void dropIndex(String collectionName,String indexName){
        dropIndex(EMPTY,collectionName,indexName);
    }

    default void dropIndex(String collectionName,String indexName, DropIndexOptions dropIndexOptions){
        dropIndex(EMPTY,collectionName,indexName,dropIndexOptions);
    }

    default void dropIndex(String collectionName,Bson keys){
        dropIndex(EMPTY,collectionName,keys);
    }

    default void dropIndex(String collectionName,Bson keys,DropIndexOptions dropIndexOptions){
        dropIndex(EMPTY,collectionName,keys,dropIndexOptions);
    }

    default void dropIndexes(String collectionName){
        dropIndex(EMPTY,collectionName);
    }

    default void dropIndexes(String collectionName,DropIndexOptions dropIndexOptions){
        dropIndexes(EMPTY,collectionName,dropIndexOptions);
    }
    
}
