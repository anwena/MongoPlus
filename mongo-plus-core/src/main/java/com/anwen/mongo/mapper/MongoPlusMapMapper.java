package com.anwen.mongo.mapper;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.inject.aggregate.LambdaAggregateChainInjectWrapper;
import com.anwen.mongo.conditions.inject.query.LambdaQueryChainInjectWrapper;
import com.anwen.mongo.conditions.inject.update.LambdaUpdateChainInjectWrapper;
import com.anwen.mongo.conditions.interfaces.Inject.InjectQuery;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.CreateIndexOptions;
import com.mongodb.client.model.DropIndexOptions;
import com.mongodb.client.model.IndexModel;
import com.mongodb.client.model.IndexOptions;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class MongoPlusMapMapper implements InjectQuery {
    private final SqlExecute sqlExecute;

    private final ExecutorFactory factory;

    public MongoPlusMapMapper(SqlExecute sqlExecute, ExecutorFactory factory) {
        this.sqlExecute = sqlExecute;
        this.factory = factory;
    }

    /**
     * 获取当前操作对象的连接，以便使用MongoDriver的语法
     * @author JiaChaoYang
     * @date 2023/11/15 13:43
     */
    public MongoCollection<Document> getMongoCollection(String collectionName){
        return this.sqlExecute.getCollection(collectionName);
    }

    public LambdaQueryChainInjectWrapper lambdaQuery(){
        return new LambdaQueryChainInjectWrapper(sqlExecute);
    }

    public LambdaAggregateChainInjectWrapper lambdaAggregate(){
        return new LambdaAggregateChainInjectWrapper(sqlExecute);
    }

    public LambdaUpdateChainInjectWrapper lambdaUpdate(){
        return new LambdaUpdateChainInjectWrapper(sqlExecute);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName) {
        return sqlExecute.doList(collectionName);
    }

    @Override
    public List<Map<String, Object>> list(String database, String collectionName) {
        return null;
    }

    @Override
    public List<Map<String, Object>> list(ClientSession clientSession, String collectionName) {
        return sqlExecute.doList(clientSession,collectionName);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return sqlExecute.doList(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public List<Map<String, Object>> list(ClientSession clientSession, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doList(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public List<Map<String, Object>> aggregateList(String collectionName, AggregateChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doAggregateList(collectionName, queryChainWrapper.getBaseAggregateList(), queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOptionsBasicDBObject());
    }

    @Override
    public List<Map<String, Object>> aggregateList(ClientSession clientSession, String collectionName, AggregateChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doAggregateList(clientSession,collectionName, queryChainWrapper.getBaseAggregateList(), queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOptionsBasicDBObject());
    }

    @Override
    public Map<String, Object> one(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return sqlExecute.doOne(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public Map<String, Object> one(ClientSession clientSession, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doOne(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public Map<String, Object> limitOne(String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doLimitOne(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOrderList());
    }

    @Override
    public Map<String, Object> limitOne(ClientSession clientSession, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doLimitOne(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOrderList());
    }


    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam) {
        return sqlExecute.doPage(collectionName,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, PageParam pageParam) {
        return sqlExecute.doPage(clientSession,collectionName,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return sqlExecute.doPage(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, PageParam pageParam, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doPage(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(collectionName,pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(clientSession,collectionName,pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doPage(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, Integer pageNum, Integer pageSize, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doPage(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageNum,pageSize);
    }

    @Override
    public Map<String, Object> getById(String collectionName , Serializable id) {
        return sqlExecute.doGetById(collectionName,id);
    }

    @Override
    public Map<String, Object> getById(ClientSession clientSession, String collectionName, Serializable id) {
        return sqlExecute.doGetById(clientSession,collectionName,id);
    }

    @Override
    public List<Map<String, Object>> getByIds(String collectionName , Collection<? extends Serializable> ids) {
        return sqlExecute.doGetByIds(collectionName,ids);
    }

    @Override
    public List<Map<String, Object>> getByIds(ClientSession clientSession, String collectionName, Collection<? extends Serializable> ids) {
        return sqlExecute.doGetByIds(clientSession,collectionName,ids);
    }

    @Override
    public Boolean save(String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doSave(collectionName,entityMap);
    }

    @Override
    public Boolean save(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doSave(clientSession,collectionName,entityMap);
    }

    @Override
    public Boolean saveBatch(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doSaveBatch(collectionName,entityMapList);
    }

    @Override
    public Boolean saveBatch(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doSaveBatch(clientSession,collectionName,entityMapList);
    }

    @Override
    public Boolean saveOrUpdate(String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doSaveOrUpdate(collectionName,entityMap);
    }

    @Override
    public Boolean saveOrUpdate(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doSaveOrUpdate(clientSession,collectionName,entityMap);
    }

    @Override
    public Boolean saveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doSaveOrUpdateBatch(collectionName,entityMapList);
    }

    @Override
    public Boolean saveOrUpdateBatch(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doSaveOrUpdateBatch(clientSession,collectionName,entityMapList);
    }

    @Override
    public Boolean updateById(String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doUpdateById(collectionName,entityMap);
    }

    @Override
    public Boolean updateById(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doUpdateById(clientSession,collectionName,entityMap);
    }

    @Override
    public Boolean updateBatchByIds(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doUpdateBatchByIds(collectionName,entityMapList);
    }

    @Override
    public Boolean updateBatchByIds(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doUpdateBatchByIds(clientSession,collectionName,entityMapList);
    }

    @Override
    public Boolean updateByColumn(String collectionName, Map<String, Object> entityMap, String column) {
        return sqlExecute.doUpdateByColumn(collectionName,entityMap,column);
    }

    @Override
    public Boolean updateByColumn(ClientSession clientSession, String collectionName, Map<String, Object> entityMap, String column) {
        return sqlExecute.doUpdateByColumn(clientSession,collectionName,entityMap,column);
    }

    @Override
    public Boolean removeById(String collectionName, Serializable id) {
        return sqlExecute.doRemoveById(collectionName,id);
    }

    @Override
    public Boolean removeById(ClientSession clientSession, String collectionName, Serializable id) {
        return sqlExecute.doRemoveById(clientSession,collectionName,id);
    }

    @Override
    public Boolean removeByColumn(String collectionName, String column, String value) {
        return sqlExecute.doRemoveByColumn(collectionName,column,value);
    }

    @Override
    public Boolean removeByColumn(ClientSession clientSession, String collectionName, String column, String value) {
        return sqlExecute.doRemoveByColumn(clientSession,collectionName,column,value);
    }

    @Override
    public Boolean removeBatchByIds(String collectionName, Collection<? extends Serializable> idList) {
        return sqlExecute.doRemoveBatchByIds(collectionName,idList);
    }

    @Override
    public Boolean removeBatchByIds(ClientSession clientSession, String collectionName, Collection<? extends Serializable> idList) {
        return sqlExecute.doRemoveBatchByIds(clientSession,collectionName,idList);
    }

    @Override
    public long count(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return sqlExecute.doCount(collectionName,queryChainWrapper.getCompareList());
    }

    @Override
    public long count(ClientSession clientSession, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doCount(clientSession,collectionName,queryChainWrapper.getCompareList());
    }

    @Override
    public List<Map<String, Object>> getByColumn(String collectionName,String field, Object fieldValue) {
        return sqlExecute.doGetByColumn(collectionName,field,fieldValue);
    }

    @Override
    public List<Map<String, Object>> getByColumn(ClientSession clientSession, String collection, String field, Object fieldValue) {
        return sqlExecute.doGetByColumn(clientSession,collection,field,fieldValue);
    }

    @Override
    public Boolean remove(String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        return remove(null,collectionName,updateChainWrapper);
    }

    @Override
    public Boolean remove(ClientSession clientSession, String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        return sqlExecute.doRemove(clientSession,collectionName,updateChainWrapper.getCompareList());
    }

    @Override
    public Boolean update(String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        return update(null,collectionName,updateChainWrapper);
    }

    @Override
    public Boolean update(ClientSession clientSession, String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(updateChainWrapper.getCompareList());
        compareConditionList.addAll(updateChainWrapper.getUpdateCompareList());
        return sqlExecute.doUpdate(clientSession,collectionName,compareConditionList);
    }

    @Override
    public List<Map<String, Object>> sql(String collectionName, String sql) {
        return sqlExecute.doSql(collectionName,sql);
    }

    @Override
    public List<Map<String, Object>> sql(ClientSession clientSession, String collectionName, String sql) {
        return sqlExecute.doSql(clientSession, collectionName,sql);
    }

    @Override
    public String createIndex(ClientSession clientSession,String collectionName,Bson bson) {
        return sqlExecute.createIndex(clientSession,bson,getMongoCollection(collectionName));
    }

    @Override
    public String createIndex(String collectionName,Bson bson) {
        return sqlExecute.createIndex(bson,getMongoCollection(collectionName));
    }

    @Override
    public String createIndex(ClientSession clientSession,String collectionName, Bson bson, IndexOptions indexOptions) {
        return sqlExecute.createIndex(clientSession,bson,indexOptions,getMongoCollection(collectionName));
    }

    @Override
    public String createIndex(String collectionName,Bson bson, IndexOptions indexOptions) {
        return sqlExecute.createIndex(bson,indexOptions,getMongoCollection(collectionName));
    }

    @Override
    public List<String> createIndexes(String collectionName,List<IndexModel> indexes) {
        return sqlExecute.createIndexes(indexes,getMongoCollection(collectionName));
    }

    @Override
    public List<String> createIndexes(String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return sqlExecute.createIndexes(indexes,createIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public List<String> createIndexes(ClientSession clientSession, String collectionName,List<IndexModel> indexes) {
        return sqlExecute.createIndexes(clientSession,indexes,getMongoCollection(collectionName));
    }

    @Override
    public List<String> createIndexes(ClientSession clientSession, String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return sqlExecute.createIndexes(clientSession,indexes,createIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public List<Document> listIndexes(String collectionName) {
        return sqlExecute.listIndexes(getMongoCollection(collectionName));
    }

    @Override
    public List<Document> listIndexes(ClientSession clientSession,String collectionName) {
        return sqlExecute.listIndexes(clientSession,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(String collectionName,String indexName) {
        sqlExecute.dropIndex(indexName,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(String collectionName,String indexName, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(indexName,dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(String collectionName,Bson keys) {
        sqlExecute.dropIndex(keys,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(String collectionName,Bson keys, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(keys,dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(ClientSession clientSession,String collectionName, String indexName) {
        sqlExecute.dropIndex(clientSession,indexName,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(ClientSession clientSession, String collectionName,Bson keys) {
        sqlExecute.dropIndex(clientSession,keys,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(ClientSession clientSession, String collectionName,String indexName, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(clientSession,indexName,dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(ClientSession clientSession, String collectionName,Bson keys, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(clientSession,keys,dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndexes(String collectionName) {
        sqlExecute.dropIndexes(getMongoCollection(collectionName));
    }

    @Override
    public void dropIndexes(ClientSession clientSession,String collectionName) {
        sqlExecute.dropIndexes(clientSession,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndexes(String collectionName,DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndexes(dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndexes(ClientSession clientSession, String collectionName,DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndexes(clientSession,dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public long count(String collectionName) {
        return sqlExecute.doCount(collectionName);
    }

    @Override
    public long count(ClientSession clientSession, String collectionName) {
        return sqlExecute.doCount(clientSession,collectionName);
    }
}
