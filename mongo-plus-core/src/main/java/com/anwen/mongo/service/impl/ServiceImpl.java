package com.anwen.mongo.service.impl;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.aggregate.LambdaAggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.ChainWrappers;
import com.mongodb.client.ClientSession;
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

    private SqlExecute sqlExecute;

    public void setSqlOperation(SqlExecute sqlExecute) {
        this.sqlExecute = sqlExecute;
    }

    private Class<T> clazz;

    /**
     * 获取当前操作对象的连接，以便使用MongoDriver的语法
     * @author JiaChaoYang
     * @date 2023/11/15 13:43
    */
    public MongoCollection<Document> getMongoCollection(){
        return this.sqlExecute.getCollection(clazz);
    }

    public void setClazz(Class<?> clazz) {
        this.clazz = (Class<T>) clazz;
    }

    @Override
    public Class<T> getGenericityClazz() {
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
    public Boolean save(T entity) {
        return sqlExecute.doSave(entity);
    }

    @Override
    public Boolean save(ClientSession clientSession, T entity) {
        return sqlExecute.doSave(clientSession,entity);
    }

    @Override
    public Boolean saveBatch(Collection<T> entityList) {
        return sqlExecute.doSaveBatch(entityList);
    }

    @Override
    public Boolean saveBatch(ClientSession clientSession, Collection<T> entityList) {
        return sqlExecute.doSaveBatch(clientSession,entityList);
    }

    @Override
    public Boolean saveOrUpdate(T entity) {
        return sqlExecute.doSaveOrUpdate(entity);
    }

    @Override
    public Boolean saveOrUpdate(ClientSession clientSession, T entity) {
        return sqlExecute.doSaveOrUpdate(clientSession,entity);
    }

    @Override
    public Boolean saveOrUpdateBatch(Collection<T> entityList) {
        return sqlExecute.doSaveOrUpdateBatch(entityList);
    }

    @Override
    public Boolean saveOrUpdateBatch(ClientSession clientSession, Collection<T> entityList) {
        return sqlExecute.doSaveOrUpdateBatch(clientSession,entityList);
    }

    @Override
    public Boolean updateById(T entity) {
        return sqlExecute.doUpdateById(entity);
    }

    @Override
    public Boolean updateById(ClientSession clientSession, T entity) {
        return sqlExecute.doUpdateById(clientSession,entity);
    }

    @Override
    public Boolean updateBatchByIds(Collection<T> entityList) {
        return sqlExecute.doUpdateBatchByIds(entityList);
    }

    @Override
    public Boolean updateBatchByIds(ClientSession clientSession, Collection<T> entityList) {
        return sqlExecute.doUpdateBatchByIds(clientSession,entityList);
    }

    @Override
    public Boolean updateByColumn(T entity, SFunction<T, Object> column) {
        return sqlExecute.doUpdateByColumn(entity,column);
    }

    @Override
    public Boolean updateByColumn(ClientSession clientSession, T entity, SFunction<T, Object> column) {
        return sqlExecute.doUpdateByColumn(clientSession,entity,column);
    }

    @Override
    public Boolean updateByColumn(T entity, String column) {
        return sqlExecute.doUpdateByColumn(entity,column);
    }

    @Override
    public Boolean updateByColumn(ClientSession clientSession, T entity, String column) {
        return sqlExecute.doUpdateByColumn(clientSession,entity,column);
    }

    @Override
    public Boolean remove(UpdateChainWrapper<T, ?> updateChainWrapper) {
        return sqlExecute.doRemove(updateChainWrapper.getCompareList(),clazz);
    }

    @Override
    public Boolean remove(ClientSession clientSession, UpdateChainWrapper<T, ?> updateChainWrapper) {
        return sqlExecute.doRemove(clientSession,updateChainWrapper.getCompareList(),clazz);
    }

    @Override
    public Boolean update(UpdateChainWrapper<T, ?> updateChainWrapper) {
        return update(null,updateChainWrapper);
    }

    @Override
    public Boolean update(ClientSession clientSession, UpdateChainWrapper<T, ?> updateChainWrapper) {
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(updateChainWrapper.getCompareList());
        compareConditionList.addAll(updateChainWrapper.getUpdateCompareList());
        return sqlExecute.doUpdate(clientSession,compareConditionList,clazz);
    }

    @Override
    public Boolean removeById(Serializable id) {
        return sqlExecute.doRemoveById(id,clazz);
    }

    @Override
    public Boolean removeById(ClientSession clientSession, Serializable id) {
        return sqlExecute.doRemoveById(clientSession,id,clazz);
    }

    @Override
    public Boolean removeByColumn(SFunction<T, Object> column, Object value) {
        return sqlExecute.doRemoveByColumn(column,value,clazz);
    }

    @Override
    public Boolean removeByColumn(ClientSession clientSession, SFunction<T, Object> column, Object value) {
        return sqlExecute.doRemoveByColumn(clientSession,column,value,clazz);
    }

    @Override
    public Boolean removeByColumn(String column, Object value) {
        return sqlExecute.doRemoveByColumn(column,value,clazz);
    }

    @Override
    public Boolean removeByColumn(ClientSession clientSession, String column, Object value) {
        return sqlExecute.doRemoveByColumn(clientSession,column,value,clazz);
    }

    @Override
    public Boolean removeBatchByIds(Collection<Serializable> idList) {
        return sqlExecute.doRemoveBatchByIds(idList,clazz);
    }

    @Override
    public Boolean removeBatchByIds(ClientSession clientSession, Collection<Serializable> idList) {
        return sqlExecute.doRemoveBatchByIds(clientSession,idList,clazz);
    }

    @Override
    public List<T> list() {
        return sqlExecute.doList(clazz);
    }

    @Override
    public List<T> list(ClientSession clientSession) {
        return sqlExecute.doList(clientSession,clazz);
    }

    @Override
    public List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doAggregateList(queryChainWrapper.getBaseAggregateList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOptionsBasicDBObject(),clazz);
    }

    @Override
    public List<T> aggregateList(ClientSession clientSession, AggregateChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doAggregateList(clientSession,queryChainWrapper.getBaseAggregateList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOptionsBasicDBObject(),clazz);
    }

    @Override
    public T one(QueryChainWrapper<T,?> queryChainWrapper) {
        return sqlExecute.doOne(queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),clazz);
    }

    @Override
    public T one(ClientSession clientSession, QueryChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doOne(clientSession,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),clazz);
    }

    @Override
    public T limitOne(QueryChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doLimitOne(queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOrderList(),clazz);
    }

    @Override
    public T limitOne(ClientSession clientSession, QueryChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doLimitOne(clientSession,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOrderList(),clazz);
    }

    @Override
    public List<T> list(QueryChainWrapper<T,?> queryChainWrapper) {
        return sqlExecute.doList(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),clazz);
    }

    @Override
    public List<T> list(ClientSession clientSession, QueryChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doList(clientSession,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),clazz);
    }

    @Override
    public List<T> list(AggregateChainWrapper<T,?> queryChainWrapper) {
        return sqlExecute.doAggregateList(queryChainWrapper.getBaseAggregateList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOptionsBasicDBObject(),clazz);
    }

    @Override
    public List<T> list(ClientSession clientSession, AggregateChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doAggregateList(clientSession,queryChainWrapper.getBaseAggregateList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOptionsBasicDBObject(),clazz);
    }

    @Override
    public long count() {
        return sqlExecute.doCount(clazz);
    }

    @Override
    public long count(ClientSession clientSession) {
        return sqlExecute.doCount(clientSession,clazz);
    }

    @Override
    public long count(QueryChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doCount(queryChainWrapper.getCompareList(),clazz);
    }

    @Override
    public long count(ClientSession clientSession, QueryChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doCount(clientSession,queryChainWrapper.getCompareList(),clazz);
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize){
        return sqlExecute.doPage(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(), pageNum,pageSize,clazz);
    }

    @Override
    public PageResult<T> page(ClientSession clientSession, QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(clientSession,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(), pageNum,pageSize,clazz);
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T, ?> queryChainWrapper, PageParam pageParam) {
        return page(null,queryChainWrapper,pageParam);
    }

    @Override
    public PageResult<T> page(ClientSession clientSession, QueryChainWrapper<T, ?> queryChainWrapper, PageParam pageParam) {
        return sqlExecute.doPage(clientSession,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return page(pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(ClientSession clientSession, PageParam pageParam) {
        return page(clientSession,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(null,null,null,null,pageNum,pageSize,clazz);
    }

    @Override
    public PageResult<T> page(ClientSession clientSession, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(clientSession,null,null,null,null,pageNum,pageSize,clazz);
    }

    @Override
    public T getById(Serializable id) {
        return sqlExecute.doGetById(id,clazz);
    }

    @Override
    public T getById(ClientSession clientSession, Serializable id) {
        return sqlExecute.doGetById(clientSession,id,clazz);
    }

    @Override
    public List<T> getByIds(Collection<Serializable> ids) {
        return sqlExecute.doGetByIds(ids,clazz);
    }

    @Override
    public List<T> getByIds(ClientSession clientSession, Collection<Serializable> ids) {
        return sqlExecute.doGetByIds(clientSession,ids,clazz);
    }

    @Override
    public List<T> sql(String sql) {
        return sqlExecute.doSql(sql,clazz);
    }

    @Override
    public List<T> sql(ClientSession clientSession,String sql) {
        return sqlExecute.doSql(clientSession,sql,clazz);
    }

    @Override
    public List<T> getByColumn(SFunction<T, Object> field, Object fieldValue) {
        return sqlExecute.doGetByColumn(field.getFieldNameLine(), fieldValue,clazz);
    }

    @Override
    public List<T> getByColumn(ClientSession clientSession, SFunction<T, Object> field, Object fieldValue) {
        return sqlExecute.doGetByColumn(clientSession,field.getFieldNameLine(),fieldValue,clazz);
    }

    @Override
    public List<T> getByColumn(String field, Object fieldValue) {
        return sqlExecute.doGetByColumn(field,fieldValue,clazz);
    }

    @Override
    public List<T> getByColumn(ClientSession clientSession, String field, Object fieldValue) {
        return sqlExecute.doGetByColumn(clientSession,field,fieldValue,clazz);
    }

    @Override
    public String createIndex(ClientSession clientSession,Bson bson) {
        return sqlExecute.createIndex(clientSession,bson,getMongoCollection());
    }

    @Override
    public String createIndex(Bson bson) {
        return sqlExecute.createIndex(bson,getMongoCollection());
    }

    @Override
    public String createIndex(ClientSession clientSession, Bson bson, IndexOptions indexOptions) {
        return sqlExecute.createIndex(clientSession,bson,indexOptions,getMongoCollection());
    }

    @Override
    public String createIndex(Bson bson, IndexOptions indexOptions) {
        return sqlExecute.createIndex(bson,indexOptions,getMongoCollection());
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes) {
        return sqlExecute.createIndexes(indexes,getMongoCollection());
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return sqlExecute.createIndexes(indexes,createIndexOptions,getMongoCollection());
    }

    @Override
    public List<String> createIndexes(ClientSession clientSession, List<IndexModel> indexes) {
        return sqlExecute.createIndexes(clientSession,indexes,getMongoCollection());
    }

    @Override
    public List<String> createIndexes(ClientSession clientSession, List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return sqlExecute.createIndexes(clientSession,indexes,createIndexOptions,getMongoCollection());
    }

    @Override
    public List<Document> listIndexes() {
        return sqlExecute.listIndexes(getMongoCollection());
    }

    @Override
    public List<Document> listIndexes(ClientSession clientSession) {
        return sqlExecute.listIndexes(clientSession,getMongoCollection());
    }

    @Override
    public void dropIndex(String indexName) {
        sqlExecute.dropIndex(indexName,getMongoCollection());
    }

    @Override
    public void dropIndex(String indexName, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(indexName,dropIndexOptions,getMongoCollection());
    }

    @Override
    public void dropIndex(Bson keys) {
        sqlExecute.dropIndex(keys,getMongoCollection());
    }

    @Override
    public void dropIndex(Bson keys, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(keys,dropIndexOptions,getMongoCollection());
    }

    @Override
    public void dropIndex(ClientSession clientSession, String indexName) {
        sqlExecute.dropIndex(clientSession,indexName,getMongoCollection());
    }

    @Override
    public void dropIndex(ClientSession clientSession, Bson keys) {
        sqlExecute.dropIndex(clientSession,keys,getMongoCollection());
    }

    @Override
    public void dropIndex(ClientSession clientSession, String indexName, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(clientSession,indexName,dropIndexOptions,getMongoCollection());
    }

    @Override
    public void dropIndex(ClientSession clientSession, Bson keys, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(clientSession,keys,dropIndexOptions,getMongoCollection());
    }

    @Override
    public void dropIndexes() {
        sqlExecute.dropIndexes(getMongoCollection());
    }

    @Override
    public void dropIndexes(ClientSession clientSession) {
        sqlExecute.dropIndexes(clientSession,getMongoCollection());
    }

    @Override
    public void dropIndexes(DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndexes(dropIndexOptions,getMongoCollection());
    }

    @Override
    public void dropIndexes(ClientSession clientSession, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndexes(clientSession,dropIndexOptions,getMongoCollection());
    }

    @Override
    public SqlExecute getSqlOperation() {
        return sqlExecute;
    }

    public Class<T> getClazz() {
        return clazz;
    }

    @Override
    public LambdaQueryChainWrapper<T> lambdaQuery() {
        return ChainWrappers.lambdaQueryChain(sqlExecute,clazz);
    }

    @Override
    public LambdaAggregateChainWrapper<T> lambdaAggregate() {
        return ChainWrappers.lambdaAggregateChain(getSqlOperation(),clazz);
    }

    @Override
    public LambdaUpdateChainWrapper<T> lambdaUpdate() {
        return ChainWrappers.lambdaUpdateChain(getSqlOperation(),clazz);
    }
}
