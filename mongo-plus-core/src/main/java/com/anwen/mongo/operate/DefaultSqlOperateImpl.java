package com.anwen.mongo.operate;

import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoCollection;
import org.bson.Document;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * sql操作实现
 *
 * @author JiaChaoYang
 **/
@Deprecated
public class DefaultSqlOperateImpl implements SqlOperate {
    @Override
    public <T> Boolean doSave(T entity) {
        return doSave(MongoTransactionContext.getClientSessionContext(),entity);
    }

    @Override
    public <T> Boolean doSave(ClientSession clientSession, T entity) {
        return null;
    }

    @Override
    public Boolean doSave(String collectionName, Map<String, Object> entityMap) {
        return null;
    }

    @Override
    public Boolean doSave(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return null;
    }

    @Override
    public <T> Boolean doSaveBatch(Collection<T> entityList) {
        return null;
    }

    @Override
    public <T> Boolean doSaveBatch(ClientSession clientSession, Collection<T> entityList) {
        return null;
    }

    @Override
    public Boolean doSaveBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        return null;
    }

    @Override
    public Boolean doSaveBatch(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityList) {
        return null;
    }

    @Override
    public <T> Boolean doSaveOrUpdate(T entity) {
        return null;
    }

    @Override
    public <T> Boolean doSaveOrUpdate(ClientSession clientSession, T entity) {
        return null;
    }

    @Override
    public Boolean doSaveOrUpdate(String collectionName, Map<String, Object> entityMap) {
        return null;
    }

    @Override
    public Boolean doSaveOrUpdate(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return null;
    }

    @Override
    public <T> Boolean doSaveOrUpdateBatch(Collection<T> entityList) {
        return null;
    }

    @Override
    public <T> Boolean doSaveOrUpdateBatch(ClientSession clientSession, Collection<T> entityList) {
        return null;
    }

    @Override
    public Boolean doSaveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        return null;
    }

    @Override
    public Boolean doSaveOrUpdateBatch(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityList) {
        return null;
    }

    @Override
    public <T> Boolean doUpdateById(T entity) {
        return null;
    }

    @Override
    public <T> Boolean doUpdateById(ClientSession clientSession, T entity) {
        return null;
    }

    @Override
    public Boolean doUpdateById(String collectionName, Map<String, Object> entityMap) {
        return null;
    }

    @Override
    public Boolean doUpdateById(ClientSession clientSession, String collectionName, Map<String, Object> entityMap) {
        return null;
    }

    @Override
    public <T> Boolean doUpdateBatchByIds(Collection<T> entityList) {
        return null;
    }

    @Override
    public <T> Boolean doUpdateBatchByIds(ClientSession clientSession, Collection<T> entityList) {
        return null;
    }

    @Override
    public Boolean doUpdateBatchByIds(String collectionName, Collection<Map<String, Object>> entityList) {
        return null;
    }

    @Override
    public Boolean doUpdateBatchByIds(ClientSession clientSession, String collectionName, Collection<Map<String, Object>> entityList) {
        return null;
    }

    @Override
    public <T> Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        return null;
    }

    @Override
    public <T> Boolean doUpdateByColumn(ClientSession clientSession, T entity, SFunction<T, Object> column) {
        return null;
    }

    @Override
    public <T> Boolean doUpdateByColumn(T entity, String column) {
        return null;
    }

    @Override
    public <T> Boolean doUpdateByColumn(ClientSession clientSession, T entity, String column) {
        return null;
    }

    @Override
    public Boolean doUpdateByColumn(String collectionName, Map<String, Object> entityMap, String column) {
        return null;
    }

    @Override
    public Boolean doUpdateByColumn(ClientSession clientSession, String collectionName, Map<String, Object> entityMap, String column) {
        return null;
    }

    @Override
    public Boolean doRemoveById(Serializable id, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doRemoveById(ClientSession clientSession, Serializable id, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doRemoveById(String collectionName, Serializable id) {
        return null;
    }

    @Override
    public Boolean doRemoveById(ClientSession clientSession, String collectionName, Serializable id) {
        return null;
    }

    @Override
    public <T> Boolean doRemoveByColumn(SFunction<T, Object> column, String value, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> Boolean doRemoveByColumn(ClientSession clientSession, SFunction<T, Object> column, String value, Class<T> clazz) {
        return null;
    }

    @Override
    public Boolean doRemoveByColumn(String column, String value, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doRemoveByColumn(ClientSession clientSession, String column, String value, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doRemoveByColumn(String collectionName, String column, String value) {
        return null;
    }

    @Override
    public Boolean doRemoveByColumn(ClientSession clientSession, String collectionName, String column, String value) {
        return null;
    }

    @Override
    public Boolean doRemoveBatchByIds(Collection<? extends Serializable> idList, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doRemoveBatchByIds(ClientSession clientSession, Collection<? extends Serializable> idList, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doRemoveBatchByIds(String collectionName, Collection<? extends Serializable> idList) {
        return null;
    }

    @Override
    public Boolean doRemoveBatchByIds(ClientSession clientSession, String collectionName, Collection<? extends Serializable> idList) {
        return null;
    }

    @Override
    public Boolean executeRemoveBatchByIds(ClientSession clientSession, Collection<? extends Serializable> idList, MongoCollection<Document> collection) {
        return null;
    }

    @Override
    public <T> List<T> doList(Class<T> clazz) {
        return null;
    }

    @Override
    public <T> List<T> doList(ClientSession clientSession, Class<T> clazz) {
        return null;
    }

    @Override
    public List<Map<String, Object>> doList(String collectionName) {
        return null;
    }

    @Override
    public List<Map<String, Object>> doList(ClientSession clientSession, String collectionName) {
        return null;
    }

    @Override
    public List<Map<String, Object>> doList(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList) {
        return null;
    }

    @Override
    public List<Map<String, Object>> doList(ClientSession clientSession, String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList) {
        return null;
    }

    @Override
    public PageResult<Map<String, Object>> doPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        return null;
    }

    @Override
    public PageResult<Map<String, Object>> doPage(ClientSession clientSession, String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        return null;
    }

    @Override
    public PageResult<Map<String, Object>> doPage(String collectionName, Integer pageNum, Integer pageSize) {
        return null;
    }

    @Override
    public PageResult<Map<String, Object>> doPage(ClientSession clientSession, String collectionName, Integer pageNum, Integer pageSize) {
        return null;
    }

    @Override
    public Map<String, Object> doOne(String collectionName, List<CompareCondition> compareConditionList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList) {
        return null;
    }

    @Override
    public Map<String, Object> doOne(ClientSession clientSession, String collectionName, List<CompareCondition> compareConditionList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList) {
        return null;
    }

    @Override
    public Map<String, Object> doLimitOne(String collectionName, List<CompareCondition> compareConditionList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, List<Order> orderList) {
        return null;
    }

    @Override
    public Map<String, Object> doLimitOne(ClientSession clientSession, String collectionName, List<CompareCondition> compareConditionList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, List<Order> orderList) {
        return null;
    }

    @Override
    public Map<String, Object> doGetById(String collectionName, Serializable id) {
        return null;
    }

    @Override
    public Map<String, Object> doGetById(ClientSession clientSession, String collectionName, Serializable id) {
        return null;
    }

    @Override
    public List<Map<String, Object>> doGetByIds(String collectionName, Collection<? extends Serializable> ids) {
        return null;
    }

    @Override
    public List<Map<String, Object>> doGetByIds(ClientSession clientSession, String collectionName, Collection<? extends Serializable> ids) {
        return null;
    }

    @Override
    public <T> List<T> doList(List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> List<T> doList(ClientSession clientSession, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> T doOne(List<CompareCondition> compareConditionList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> T doOne(ClientSession clientSession, List<CompareCondition> compareConditionList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> T doLimitOne(List<CompareCondition> compareConditionList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, List<Order> orderList, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> T doLimitOne(ClientSession clientSession, List<CompareCondition> compareConditionList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, List<Order> orderList, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> PageResult<T> doPage(List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> PageResult<T> doPage(ClientSession clientSession, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> T doGetById(Serializable id, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> T doGetById(ClientSession clientSession, Serializable id, Class<T> clazz) {
        return null;
    }

    @Override
    public boolean doIsExist(Serializable id, Class<?> clazz) {
        return false;
    }

    @Override
    public boolean doIsExist(ClientSession clientSession, Serializable id, Class<?> clazz) {
        return false;
    }

    @Override
    public boolean doIsExistMap(String collectionName, Serializable id) {
        return false;
    }

    @Override
    public boolean doIsExistMap(ClientSession clientSession, String collectionName, Serializable id) {
        return false;
    }

    @Override
    public <T> List<T> doGetByIds(Collection<? extends Serializable> ids, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> List<T> doGetByIds(ClientSession clientSession, Collection<? extends Serializable> ids, Class<T> clazz) {
        return null;
    }

    @Override
    public Boolean doUpdate(List<CompareCondition> compareConditionList, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doUpdate(ClientSession clientSession, List<CompareCondition> compareConditionList, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doUpdate(String collectionName, List<CompareCondition> compareConditionList) {
        return null;
    }

    @Override
    public Boolean doUpdate(ClientSession clientSession, String collectionName, List<CompareCondition> compareConditionList) {
        return null;
    }

    @Override
    public Boolean executeUpdate(ClientSession clientSession, List<CompareCondition> compareConditionList, MongoCollection<Document> collection) {
        return null;
    }

    @Override
    public Boolean doRemove(List<CompareCondition> compareConditionList, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doRemove(ClientSession clientSession, List<CompareCondition> compareConditionList, Class<?> clazz) {
        return null;
    }

    @Override
    public Boolean doRemove(String collectionName, List<CompareCondition> compareConditionList) {
        return null;
    }

    @Override
    public Boolean doRemove(ClientSession clientSession, String collectionName, List<CompareCondition> compareConditionList) {
        return null;
    }

    @Override
    public Boolean executeRemove(ClientSession clientSession, List<CompareCondition> compareConditionList, MongoCollection<Document> collection) {
        return null;
    }

    @Override
    public long doCount(String collectionName, List<CompareCondition> compareConditionList) {
        return 0;
    }

    @Override
    public long doCount(ClientSession clientSession, String collectionName, List<CompareCondition> compareConditionList) {
        return 0;
    }

    @Override
    public long doCount(List<CompareCondition> compareConditionList, Class<?> clazz) {
        return 0;
    }

    @Override
    public long doCount(ClientSession clientSession, List<CompareCondition> compareConditionList, Class<?> clazz) {
        return 0;
    }

    @Override
    public long executeCountByCondition(ClientSession clientSession, List<CompareCondition> compareConditionList, MongoCollection<Document> collection) {
        return 0;
    }

    @Override
    public long doCount(String collectionName) {
        return 0;
    }

    @Override
    public long doCount(ClientSession clientSession, String collectionName) {
        return 0;
    }

    @Override
    public long doCount(Class<?> clazz) {
        return 0;
    }

    @Override
    public long doCount(ClientSession clientSession, Class<?> clazz) {
        return 0;
    }

    @Override
    public long executeCount(ClientSession clientSession, MongoCollection<Document> collection) {
        return 0;
    }

    @Override
    public <T> List<T> doAggregateList(List<BaseAggregate> aggregateList, List<BasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject, Class<T> clazz) {
        return null;
    }

    @Override
    public <T> List<T> doAggregateList(ClientSession clientSession, List<BaseAggregate> aggregateList, List<BasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject, Class<T> clazz) {
        return null;
    }

    @Override
    public List<Map<String, Object>> doAggregateList(String collectionName, List<BaseAggregate> aggregateList, List<BasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject) {
        return null;
    }

    @Override
    public List<Map<String, Object>> doAggregateList(ClientSession clientSession, String collectionName, List<BaseAggregate> aggregateList, List<BasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject) {
        return null;
    }

    @Override
    public <E> List<E> doAggregateList(String collectionName, List<BaseAggregate> aggregateList, List<BasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject, Class<E> clazz) {
        return null;
    }

    @Override
    public <E> List<E> doAggregateList(ClientSession clientSession, String collectionName, List<BaseAggregate> aggregateList, List<BasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject, Class<E> clazz) {
        return null;
    }
    

}
