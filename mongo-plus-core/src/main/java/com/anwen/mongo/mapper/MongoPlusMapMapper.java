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

import static com.anwen.mongo.toolkit.StringPool.EMPTY;

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
        return new LambdaQueryChainInjectWrapper(sqlExecute,factory);
    }

    public LambdaAggregateChainInjectWrapper lambdaAggregate(){
        return new LambdaAggregateChainInjectWrapper(sqlExecute,factory);
    }

    public LambdaUpdateChainInjectWrapper lambdaUpdate(){
        return new LambdaUpdateChainInjectWrapper(sqlExecute,factory);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName) {
        return list(EMPTY,collectionName);
    }

    @Override
    public List<Map<String, Object>> list(String database, String collectionName) {
        return factory.getInjectExecute(database).list(collectionName);
    }

    @Override
    @Deprecated
    public List<Map<String, Object>> list(ClientSession clientSession, String collectionName) {
        return sqlExecute.doList(clientSession,collectionName);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return list(EMPTY,collectionName,queryChainWrapper);
    }

    @Override
    public List<Map<String, Object>> list(String database, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return factory.getInjectExecute(database).list(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    @Deprecated
    public List<Map<String, Object>> list(ClientSession clientSession, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doList(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public List<Map<String, Object>> aggregateList(String collectionName, AggregateChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return aggregateList(EMPTY,collectionName,queryChainWrapper);
    }

    @Override
    public List<Map<String, Object>> aggregateList(String database, String collectionName, AggregateChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return factory.getInjectExecute(database).aggregateList(collectionName, queryChainWrapper.getBaseAggregateList(), queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOptionsBasicDBObject());
    }

    @Override
    @Deprecated
    public List<Map<String, Object>> aggregateList(ClientSession clientSession, String collectionName, AggregateChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doAggregateList(clientSession,collectionName, queryChainWrapper.getBaseAggregateList(), queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOptionsBasicDBObject());
    }

    @Override
    public Map<String, Object> one(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return one(EMPTY,collectionName,queryChainWrapper);
    }

    @Override
    public Map<String, Object> one(String database, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return factory.getInjectExecute(database).one(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    @Deprecated
    public Map<String, Object> one(ClientSession clientSession, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doOne(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public Map<String, Object> limitOne(String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return limitOne(EMPTY,collectionName,queryChainWrapper);
    }

    @Override
    public Map<String, Object> limitOne(String database, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return factory.getInjectExecute(database).limitOne(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOrderList());
    }

    @Override
    @Deprecated
    public Map<String, Object> limitOne(ClientSession clientSession, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doLimitOne(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),queryChainWrapper.getOrderList());
    }


    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam) {
        return page(EMPTY,collectionName,pageParam);
    }

    @Override
    public PageResult<Map<String, Object>> page(String database, String collectionName, PageParam pageParam) {
        return factory.getInjectExecute(database).page(collectionName,null,null,null,null,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    @Deprecated
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, PageParam pageParam) {
        return sqlExecute.doPage(clientSession,collectionName,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return page(EMPTY,collectionName,pageParam,queryChainWrapper);
    }

    @Override
    public PageResult<Map<String, Object>> page(String database, String collectionName, PageParam pageParam, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return factory.getInjectExecute(database).page(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    @Deprecated
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, PageParam pageParam, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doPage(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize) {
        return page(EMPTY,collectionName,pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(String database, String collectionName, Integer pageNum, Integer pageSize) {
        return page(collectionName,new PageParam(pageNum,pageSize));
    }

    @Override
    @Deprecated
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(clientSession,collectionName,pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return page(EMPTY,collectionName,pageNum,pageSize,queryChainWrapper);
    }

    @Override
    public PageResult<Map<String, Object>> page(String database, String collectionName, Integer pageNum, Integer pageSize, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return factory.getInjectExecute(database).page(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageNum,pageSize);
    }

    @Override
    @Deprecated
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, Integer pageNum, Integer pageSize, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doPage(clientSession,collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(),pageNum,pageSize);
    }

    @Override
    public Map<String, Object> getById(String collectionName , Serializable id) {
        return getById(EMPTY,collectionName,id);
    }

    @Override
    public Map<String, Object> getById(String database, String collectionName, Serializable id) {
        return factory.getInjectExecute(database).getById(collectionName,id);
    }

    @Override
    @Deprecated
    public Map<String, Object> getById(ClientSession clientSession, String collectionName, Serializable id) {
        return sqlExecute.doGetById(clientSession,collectionName,id);
    }

    @Override
    public List<Map<String, Object>> getByIds(String collectionName , Collection<? extends Serializable> ids) {
        return getByIds(EMPTY,collectionName,ids);
    }

    @Override
    public List<Map<String, Object>> getByIds(String database, String collectionName, Collection<? extends Serializable> ids) {
        return factory.getInjectExecute(database).getByIds(collectionName,ids);
    }

    @Override
    @Deprecated
    public List<Map<String, Object>> getByIds(ClientSession clientSession, String collectionName, Collection<? extends Serializable> ids) {
        return sqlExecute.doGetByIds(clientSession,collectionName,ids);
    }

    @Override
    public Boolean save(String collectionName, Map<String, Object> entityMap) {
        return save(EMPTY,collectionName,entityMap);
    }

    @Override
    public Boolean save(String database, String collectionName, Map<String, Object> entityMap) {
        return factory.getInjectExecute(database).save(collectionName,entityMap);
    }

    @Override
    @Deprecated
    public Boolean save(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doSave(clientSession,collectionName,entityMap);
    }

    @Override
    public Boolean saveBatch(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return saveBatch(EMPTY,collectionName,entityMapList);
    }

    @Override
    public Boolean saveBatch(String database, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return factory.getInjectExecute(database).saveBatch(collectionName,entityMapList);
    }

    @Override
    @Deprecated
    public Boolean saveBatch(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doSaveBatch(clientSession,collectionName,entityMapList);
    }

    @Override
    public Boolean saveOrUpdate(String collectionName, Map<String, Object> entityMap) {
        return saveOrUpdate(EMPTY,collectionName,entityMap);
    }

    @Override
    public Boolean saveOrUpdate(String database, String collectionName, Map<String, Object> entityMap) {
        return factory.getInjectExecute(database).saveOrUpdate(collectionName,entityMap);
    }

    @Override
    @Deprecated
    public Boolean saveOrUpdate(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doSaveOrUpdate(clientSession,collectionName,entityMap);
    }

    @Override
    public Boolean saveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return saveOrUpdateBatch(EMPTY,collectionName,entityMapList);
    }

    @Override
    public Boolean saveOrUpdateBatch(String database, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return factory.getInjectExecute(database).saveOrUpdateBatch(collectionName,entityMapList);
    }

    @Override
    @Deprecated
    public Boolean saveOrUpdateBatch(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doSaveOrUpdateBatch(clientSession,collectionName,entityMapList);
    }

    @Override
    public Boolean updateById(String collectionName, Map<String, Object> entityMap) {
        return updateById(EMPTY,collectionName,entityMap);
    }

    @Override
    public Boolean updateById(String database, String collectionName, Map<String, Object> entityMap) {
        return factory.getInjectExecute(database).updateById(collectionName,entityMap);
    }

    @Override
    @Deprecated
    public Boolean updateById(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return sqlExecute.doUpdateById(clientSession,collectionName,entityMap);
    }

    @Override
    public Boolean updateBatchByIds(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return updateBatchByIds(EMPTY,collectionName,entityMapList);
    }

    @Override
    public Boolean updateBatchByIds(String database, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return factory.getInjectExecute(database).updateBatchByIds(collectionName,entityMapList);
    }

    @Override
    @Deprecated
    public Boolean updateBatchByIds(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlExecute.doUpdateBatchByIds(clientSession,collectionName,entityMapList);
    }

    @Override
    public Boolean updateByColumn(String collectionName, Map<String, Object> entityMap, String column) {
        return updateByColumn(EMPTY,collectionName,entityMap,column);
    }

    @Override
    public Boolean updateByColumn(String database, String collectionName, Map<String, Object> entityMap, String column) {
        return factory.getInjectExecute(database).updateByColumn(collectionName,entityMap,column);
    }

    @Override
    @Deprecated
    public Boolean updateByColumn(ClientSession clientSession, String collectionName, Map<String, Object> entityMap, String column) {
        return sqlExecute.doUpdateByColumn(clientSession,collectionName,entityMap,column);
    }

    @Override
    public Boolean removeById(String collectionName, Serializable id) {
        return removeById(EMPTY,collectionName,id);
    }

    @Override
    public Boolean removeById(String database, String collectionName, Serializable id) {
        return factory.getInjectExecute(database).removeById(collectionName,id);
    }

    @Override
    @Deprecated
    public Boolean removeById(ClientSession clientSession, String collectionName, Serializable id) {
        return sqlExecute.doRemoveById(clientSession,collectionName,id);
    }

    @Override
    public Boolean removeByColumn(String collectionName, String column, String value) {
        return removeByColumn(EMPTY,collectionName,column,value);
    }

    @Override
    public Boolean removeByColumn(String database, String collectionName, String column, String value) {
        return factory.getInjectExecute(database).removeByColumn(collectionName,column,value);
    }

    @Override
    @Deprecated
    public Boolean removeByColumn(ClientSession clientSession, String collectionName, String column, String value) {
        return sqlExecute.doRemoveByColumn(clientSession,collectionName,column,value);
    }

    @Override
    public Boolean removeBatchByIds(String collectionName, Collection<? extends Serializable> idList) {
        return removeBatchByIds(EMPTY,collectionName,idList);
    }

    @Override
    public Boolean removeBatchByIds(String database, String collectionName, Collection<? extends Serializable> idList) {
        return factory.getInjectExecute(database).removeBatchByIds(collectionName,idList);
    }

    @Override
    @Deprecated
    public Boolean removeBatchByIds(ClientSession clientSession, String collectionName, Collection<? extends Serializable> idList) {
        return sqlExecute.doRemoveBatchByIds(clientSession,collectionName,idList);
    }

    @Override
    public long count(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return count(EMPTY,collectionName,queryChainWrapper);
    }

    @Override
    public long count(String database, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return factory.getInjectExecute(database).count(collectionName,queryChainWrapper.getCompareList());
    }

    @Override
    @Deprecated
    public long count(ClientSession clientSession, String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlExecute.doCount(clientSession,collectionName,queryChainWrapper.getCompareList());
    }

    @Override
    public List<Map<String, Object>> getByColumn(String collectionName,String field, Object fieldValue) {
        return getByColumn(EMPTY,collectionName,field,fieldValue);
    }

    @Override
    public List<Map<String, Object>> getByColumn(String database, String collectionName, String field, Object fieldValue) {
        return factory.getInjectExecute(database).getByColumn(collectionName,field,fieldValue);
    }

    @Override
    @Deprecated
    public List<Map<String, Object>> getByColumn(ClientSession clientSession, String collection, String field, Object fieldValue) {
        return sqlExecute.doGetByColumn(clientSession,collection,field,fieldValue);
    }

    @Override
    public Boolean remove(String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        return remove(EMPTY,collectionName,updateChainWrapper);
    }

    @Override
    public Boolean remove(String database, String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        return factory.getInjectExecute(database).remove(collectionName,updateChainWrapper.getCompareList());
    }

    @Override
    @Deprecated
    public Boolean remove(ClientSession clientSession, String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        return sqlExecute.doRemove(clientSession,collectionName,updateChainWrapper.getCompareList());
    }

    @Override
    public Boolean update(String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        return update(EMPTY,collectionName,updateChainWrapper);
    }

    @Override
    public Boolean update(String database, String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(updateChainWrapper.getCompareList());
        compareConditionList.addAll(updateChainWrapper.getUpdateCompareList());
        return factory.getInjectExecute(database).update(collectionName,compareConditionList);
    }

    @Override
    @Deprecated
    public Boolean update(ClientSession clientSession, String collectionName, UpdateChainWrapper<Map<String, Object>, ?> updateChainWrapper) {
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(updateChainWrapper.getCompareList());
        compareConditionList.addAll(updateChainWrapper.getUpdateCompareList());
        return sqlExecute.doUpdate(clientSession,collectionName,compareConditionList);
    }

    @Override
    @Deprecated
    public List<Map<String, Object>> sql(String collectionName, String sql) {
        return sqlExecute.doSql(collectionName,sql);
    }

    @Override
    @Deprecated
    public List<Map<String, Object>> sql(ClientSession clientSession, String collectionName, String sql) {
        return sqlExecute.doSql(clientSession, collectionName,sql);
    }

    @Override
    public List<Map<String, Object>> queryCommand(String collectionName,String command) {
        return queryCommand(EMPTY,collectionName,command);
    }

    @Override
    public List<Map<String, Object>> queryCommand(String database, String collectionName, String command) {
        return factory.getInjectExecute(database).queryCommand(collectionName,command);
    }

    @Override
    @Deprecated
    public String createIndex(ClientSession clientSession,String collectionName,Bson bson) {
        return sqlExecute.createIndex(clientSession,bson,getMongoCollection(collectionName));
    }

    @Override
    public String createIndex(String collectionName,Bson bson) {
        return createIndex(EMPTY,collectionName,bson);
    }

    @Override
    public String createIndex(String database, String collectionName, Bson bson) {
        return factory.getInjectExecute(database).createIndex(collectionName,bson);
    }

    @Override
    @Deprecated
    public String createIndex(ClientSession clientSession,String collectionName, Bson bson, IndexOptions indexOptions) {
        return sqlExecute.createIndex(clientSession,bson,indexOptions,getMongoCollection(collectionName));
    }

    @Override
    public String createIndex(String collectionName,Bson bson, IndexOptions indexOptions) {
        return createIndex(EMPTY,collectionName,bson,indexOptions);
    }

    @Override
    public String createIndex(String database, String collectionName, Bson bson, IndexOptions indexOptions) {
        return factory.getInjectExecute(database).createIndex(collectionName,bson,indexOptions);
    }

    @Override
    public List<String> createIndexes(String collectionName,List<IndexModel> indexes) {
        return createIndexes(EMPTY,collectionName,indexes);
    }

    @Override
    public List<String> createIndexes(String database, String collectionName, List<IndexModel> indexes) {
        return factory.getInjectExecute(database).createIndexes(collectionName,indexes);
    }

    @Override
    public List<String> createIndexes(String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return createIndexes(EMPTY,collectionName,indexes,createIndexOptions);
    }

    @Override
    public List<String> createIndexes(String database, String collectionName, List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return factory.getInjectExecute(database).createIndexes(collectionName,indexes,createIndexOptions);
    }

    @Override
    @Deprecated
    public List<String> createIndexes(ClientSession clientSession, String collectionName,List<IndexModel> indexes) {
        return sqlExecute.createIndexes(clientSession,indexes,getMongoCollection(collectionName));
    }

    @Override
    @Deprecated
    public List<String> createIndexes(ClientSession clientSession, String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return sqlExecute.createIndexes(clientSession,indexes,createIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public List<Document> listIndexes(String collectionName) {
        return listIndexes(EMPTY,collectionName);
    }

    @Override
    public List<Document> listIndexes(String database, String collectionName) {
        return factory.getInjectExecute(database).listIndexes(collectionName);
    }

    @Override
    @Deprecated
    public List<Document> listIndexes(ClientSession clientSession,String collectionName) {
        return sqlExecute.listIndexes(clientSession,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndex(String collectionName,String indexName) {
        dropIndex(EMPTY,collectionName,indexName);
    }

    @Override
    public void dropIndex(String database, String collectionName, String indexName) {
        factory.getInjectExecute(database).dropIndex(collectionName,indexName);
    }

    @Override
    public void dropIndex(String collectionName,String indexName, DropIndexOptions dropIndexOptions) {
        dropIndex(EMPTY,collectionName,indexName,dropIndexOptions);
    }

    @Override
    public void dropIndex(String database, String collectionName, String indexName, DropIndexOptions dropIndexOptions) {
        factory.getInjectExecute(database).dropIndex(collectionName,indexName,dropIndexOptions);
    }

    @Override
    public void dropIndex(String collectionName,Bson keys) {
        dropIndex(EMPTY,collectionName,keys);
    }

    @Override
    public void dropIndex(String database, String collectionName, Bson keys) {
        factory.getInjectExecute(database).dropIndex(collectionName,keys);
    }

    @Override
    public void dropIndex(String collectionName,Bson keys, DropIndexOptions dropIndexOptions) {
        dropIndex(EMPTY,collectionName,keys,dropIndexOptions);
    }

    @Override
    public void dropIndex(String database, String collectionName, Bson keys, DropIndexOptions dropIndexOptions) {
        factory.getInjectExecute(database).dropIndex(collectionName,keys,dropIndexOptions);
    }

    @Override
    @Deprecated
    public void dropIndex(ClientSession clientSession,String collectionName, String indexName) {
        sqlExecute.dropIndex(clientSession,indexName,getMongoCollection(collectionName));
    }

    @Override
    @Deprecated
    public void dropIndex(ClientSession clientSession, String collectionName,Bson keys) {
        sqlExecute.dropIndex(clientSession,keys,getMongoCollection(collectionName));
    }

    @Override
    @Deprecated
    public void dropIndex(ClientSession clientSession, String collectionName,String indexName, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(clientSession,indexName,dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    @Deprecated
    public void dropIndex(ClientSession clientSession, String collectionName,Bson keys, DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndex(clientSession,keys,dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndexes(String collectionName) {
        dropIndexes(EMPTY,collectionName);
    }

    @Override
    public void dropIndexes(String database, String collectionName) {
        factory.getInjectExecute(database).dropIndexes(collectionName);
    }

    @Override
    @Deprecated
    public void dropIndexes(ClientSession clientSession,String collectionName) {
        sqlExecute.dropIndexes(clientSession,getMongoCollection(collectionName));
    }

    @Override
    public void dropIndexes(String collectionName,DropIndexOptions dropIndexOptions) {
        dropIndexes(EMPTY,collectionName,dropIndexOptions);
    }

    @Override
    public void dropIndexes(String database, String collectionName, DropIndexOptions dropIndexOptions) {
        factory.getInjectExecute(database).dropIndexes(collectionName,dropIndexOptions);
    }

    @Override
    @Deprecated
    public void dropIndexes(ClientSession clientSession, String collectionName,DropIndexOptions dropIndexOptions) {
        sqlExecute.dropIndexes(clientSession,dropIndexOptions,getMongoCollection(collectionName));
    }

    @Override
    public long count(String collectionName) {
        return count(EMPTY,collectionName);
    }

    @Override
    public long count(String database, String collectionName) {
        return factory.getInjectExecute(database).count(collectionName);
    }

    @Override
    @Deprecated
    public long count(ClientSession clientSession, String collectionName) {
        return sqlExecute.doCount(clientSession,collectionName);
    }
}
