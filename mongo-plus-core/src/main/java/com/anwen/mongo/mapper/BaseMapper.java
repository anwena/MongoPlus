package com.anwen.mongo.mapper;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.model.PageResult;
import com.mongodb.BasicDBObject;
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
public interface BaseMapper{

    MongoPlusClient getMongoPlusClient();

    default Execute getExecute(){
        return new ExecutorFactory().getExecute();
    }

    <T> Document processIdField(T entity,Boolean skip);

    <T> boolean save(T entity);

    <T> Boolean saveBatch(Collection<T> entityList);

    Long update(Bson queryBasic,Bson updateBasic,Class<?> clazz);

    Integer bulkWrite(List<WriteModel<Document>> writeModelList,Class<?> clazz);

    <T> Boolean update(T entity,QueryChainWrapper<T,?> queryChainWrapper);

    <T> List<T> list(Class<T> clazz);

    <T> List<T> list(QueryChainWrapper<T,?> queryChainWrapper, Class<T> clazz);

    <T> List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz);

    <T> T one(QueryChainWrapper<T,?> queryChainWrapper,Class<T> clazz);

    <T> T limitOne(QueryChainWrapper<T, ?> queryChainWrapper,Class<T> clazz);

    <T> PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz);

    <T> PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz);

    <T> T getById(Serializable id,Class<T> clazz);

    boolean isExist(Serializable id,Class<?> clazz);

    boolean isExist(QueryChainWrapper<?,?> queryChainWrapper,Class<?> clazz);

    <T> List<T> getByIds(Collection<? extends Serializable> ids,Class<T> clazz);

    Boolean update(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz);

    Boolean remove(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz);

    Long remove(Bson filter,Class<?> clazz);

    long count(QueryChainWrapper<?, ?> queryChainWrapper,Class<?> clazz);

    long recentPageCount(List<CompareCondition> compareConditionList,Class<?> clazz, Integer pageNum, Integer pageSize, Integer recentPageNum);

    long count(Class<?> clazz);

    <T> List<T> queryCommand(String command,Class<T> clazz);

    <T> List<T> getByColumn(String column,Object value,Class<T> clazz);

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
