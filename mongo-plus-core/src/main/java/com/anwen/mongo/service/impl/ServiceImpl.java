package com.anwen.mongo.service.impl;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.aggregate.LambdaAggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.ChainWrappers;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.CreateIndexOptions;
import com.mongodb.client.model.DropIndexOptions;
import com.mongodb.client.model.IndexModel;
import com.mongodb.client.model.IndexOptions;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author JiaChaoYang
 * 接口实现
 * @since 2023-02-09 14:13
 **/
public class ServiceImpl<T> implements IService<T>{

    private ExecutorFactory factory;

    public void setFactory(ExecutorFactory factory){
        this.factory = factory;
    }

    public ExecutorFactory getFactory(){
        return factory;
    }

    private Class<T> clazz;

    private String database;

    public String getDatabase() {
        return database;
    }

    public void setDatabase(String database) {
        this.database = database;
    }

    public void setClazz(Class<?> clazz) {
        this.clazz = (Class<T>) clazz;
    }

    @Override
    public Class<T> getGenericityClass() {
        if (clazz != null) {
            return clazz;
        }
        Type superClassType = getClass().getGenericSuperclass();
        ParameterizedType pt = (ParameterizedType) superClassType;
        Type genType = pt.getActualTypeArguments()[0];

        if (genType instanceof Class) {
            clazz = (Class<T>) genType;
        } else if (genType instanceof TypeVariable) {
            // 处理泛型类型是 TypeVariable 的情况
            clazz = (Class<T>) Object.class;
        } else {
            throw new IllegalArgumentException("Unsupported generic type: " + genType);
        }
        return clazz;
    }

    @Override
    public MongoCollection<Document> getCollection() {
        return factory.getCollectionManager(database).getCollection(clazz);
    }

    @Override
    public MongoCollection<Document> getCollection(String database) {
        return factory.getCollectionManager(database).getCollection(clazz);
    }

    @Override
    public Boolean save(T entity) {
        return factory.getExecute(database).save(entity);
    }

    @Override
    public Boolean saveBatch(Collection<T> entityList) {
        return factory.getExecute(database).saveBatch(entityList);
    }

    @Override
    public Boolean saveOrUpdate(T entity) {
        return factory.getExecute(database).saveOrUpdate(entity);
    }

    @Override
    public Boolean saveOrUpdateWrapper(T entity, QueryChainWrapper<T, ?> queryChainWrapper) {
        return factory.getExecute(database).saveOrUpdateWrapper(entity,queryChainWrapper);
    }

    @Override
    public Boolean saveOrUpdateBatch(Collection<T> entityList) {
        return factory.getExecute(database).saveOrUpdateBatch(entityList);
    }

    @Override
    public Boolean updateById(T entity) {
        return factory.getExecute(database).updateById(entity);
    }

    @Override
    public Boolean updateBatchByIds(Collection<T> entityList) {
        return factory.getExecute(database).updateBatchByIds(entityList);
    }

    @Override
    public Boolean updateByColumn(T entity, SFunction<T, Object> column) {
        return factory.getExecute(database).updateByColumn(entity,column);
    }

    @Override
    public Boolean updateByColumn(T entity, String column) {
        return factory.getExecute(database).updateByColumn(entity,column);
    }

    @Override
    public Boolean remove(UpdateChainWrapper<T, ?> updateChainWrapper) {
        return factory.getExecute(database).remove(updateChainWrapper,clazz);
    }

    @Override
    public Boolean update(UpdateChainWrapper<T, ?> updateChainWrapper) {
        return factory.getExecute(database).update(updateChainWrapper,clazz);
    }

    @Override
    public Boolean removeById(Serializable id) {
        return factory.getExecute(database).removeById(id,clazz);
    }

    @Override
    public Boolean removeByColumn(SFunction<T, Object> column, Object value) {
        return factory.getExecute(database).removeByColumn(column,value,clazz);
    }

    @Override
    public Boolean removeByColumn(String column, Object value) {
        return factory.getExecute(database).removeByColumn(column,value,clazz);
    }

    @Override
    public Boolean removeBatchByIds(Collection<? extends Serializable> idList) {
        return factory.getExecute(database).removeBatchByIds(idList,clazz);
    }

    @Override
    public List<T> list() {
        return factory.getExecute(database).list(clazz);
    }

    @Override
    public List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper) {
        return factory.getExecute(database).aggregateList(queryChainWrapper,clazz);
    }

    @Override
    public T one(QueryChainWrapper<T,?> queryChainWrapper) {
        return factory.getExecute(database).one(queryChainWrapper,clazz);
    }

    @Override
    public T limitOne(QueryChainWrapper<T, ?> queryChainWrapper) {
        return factory.getExecute(database).limitOne(queryChainWrapper,clazz);
    }

    @Override
    public List<T> list(QueryChainWrapper<T,?> queryChainWrapper) {
        return factory.getExecute(database).list(queryChainWrapper,clazz);
    }

    @Override
    public List<T> list(AggregateChainWrapper<T,?> queryChainWrapper) {
        return factory.getExecute(database).aggregateList(queryChainWrapper,clazz);
    }

    @Override
    public long count() {
        return factory.getExecute(database).count(clazz);
    }

    @Override
    public long count(QueryChainWrapper<T, ?> queryChainWrapper) {
        return factory.getExecute(database).count(queryChainWrapper,clazz);
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize){
        return factory.getExecute(database).page(queryChainWrapper, pageNum,pageSize,clazz);
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T, ?> queryChainWrapper, PageParam pageParam) {
        return page(queryChainWrapper,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return page(pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return page(new QueryChainWrapper<>(),pageNum,pageSize);
    }

    @Override
    public T getById(Serializable id) {
        return factory.getExecute(database).getById(id,clazz);
    }

    @Override
    public List<T> getByIds(Collection<? extends Serializable> ids) {
        return factory.getExecute(database).getByIds(ids,clazz);
    }

    @Override
    public List<T> queryCommand(String command) {
        return factory.getExecute(database).queryCommand(command,clazz);
    }

    @Override
    public List<T> getByColumn(SFunction<T, Object> field, Object fieldValue) {
        return factory.getExecute(database).getByColumn(field.getFieldNameLine(), fieldValue,clazz);
    }

    @Override
    public List<T> getByColumn(String field, Object fieldValue) {
        return factory.getExecute(database).getByColumn(field,fieldValue,clazz);
    }

    @Override
    public String createIndex(Bson bson) {
        return factory.getExecute(database).createIndex(bson,clazz);
    }

    @Override
    public String createIndex(Bson bson, IndexOptions indexOptions) {
        return factory.getExecute(database).createIndex(bson,indexOptions,clazz);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes) {
        return factory.getExecute(database).createIndexes(indexes,clazz);
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return factory.getExecute(database).createIndexes(indexes,createIndexOptions,clazz);
    }

    @Override
    public List<Document> listIndexes() {
        return factory.getExecute(database).listIndexes(clazz);
    }

    @Override
    public void dropIndex(String indexName) {
        factory.getExecute(database).dropIndex(indexName,clazz);
    }

    @Override
    public void dropIndex(String indexName, DropIndexOptions dropIndexOptions) {
        factory.getExecute(database).dropIndex(indexName,dropIndexOptions,clazz);
    }

    @Override
    public void dropIndex(Bson keys) {
        factory.getExecute(database).dropIndex(keys,clazz);
    }

    @Override
    public void dropIndex(Bson keys, DropIndexOptions dropIndexOptions) {
        factory.getExecute(database).dropIndex(keys,dropIndexOptions,clazz);
    }

    @Override
    public void dropIndexes() {
        factory.getExecute(database).dropIndexes(clazz);
    }

    @Override
    public void dropIndexes(DropIndexOptions dropIndexOptions) {
        factory.getExecute(database).dropIndexes(dropIndexOptions,clazz);
    }

    public Class<T> getClazz() {
        return clazz;
    }

    @Override
    public LambdaQueryChainWrapper<T> lambdaQuery() {
        return ChainWrappers.lambdaQueryChain(factory,clazz,database);
    }

    @Override
    public LambdaAggregateChainWrapper<T> lambdaAggregate() {
        return ChainWrappers.lambdaAggregateChain(factory,clazz,database);
    }

    @Override
    public LambdaUpdateChainWrapper<T> lambdaUpdate() {
        return ChainWrappers.lambdaUpdateChain(factory,clazz,database);
    }
}
