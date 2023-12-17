package com.anwen.mongo.operate;

import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoCollection;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description sql操作
 * @date 2023-09-26 17:19
 **/
@Deprecated
public interface SqlOperate {

    Logger logger = LoggerFactory.getLogger(SqlOperate.class);

    <T> Boolean doSave(T entity);

    <T> Boolean doSave(ClientSession clientSession,T entity);

    Boolean doSave(String collectionName, Map<String, Object> entityMap);

    Boolean doSave(ClientSession clientSession,String collectionName, Map<String, Object> entityMap);

    <T> Boolean doSaveBatch(Collection<T> entityList);
    <T> Boolean doSaveBatch(ClientSession clientSession,Collection<T> entityList);

    Boolean doSaveBatch(String collectionName, Collection<Map<String, Object>> entityList);

    Boolean doSaveBatch(ClientSession clientSession,String collectionName, Collection<Map<String, Object>> entityList);

    <T> Boolean doSaveOrUpdate(T entity);

    <T> Boolean doSaveOrUpdate(ClientSession clientSession,T entity);
    Boolean doSaveOrUpdate(String collectionName, Map<String, Object> entityMap);

    Boolean doSaveOrUpdate(ClientSession clientSession,String collectionName, Map<String, Object> entityMap);

    <T> Boolean doSaveOrUpdateBatch(Collection<T> entityList);

    <T> Boolean doSaveOrUpdateBatch(ClientSession clientSession,Collection<T> entityList);

    Boolean doSaveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityList);

    Boolean doSaveOrUpdateBatch(ClientSession clientSession,String collectionName, Collection<Map<String, Object>> entityList);


    <T> Boolean doUpdateById(T entity);

    <T> Boolean doUpdateById(ClientSession clientSession,T entity);

    Boolean doUpdateById(String collectionName, Map<String, Object> entityMap);

    Boolean doUpdateById(ClientSession clientSession,String collectionName, Map<String, Object> entityMap);

    <T> Boolean doUpdateBatchByIds(Collection<T> entityList);

    <T> Boolean doUpdateBatchByIds(ClientSession clientSession,Collection<T> entityList);

    Boolean doUpdateBatchByIds(String collectionName, Collection<Map<String, Object>> entityList);

    Boolean doUpdateBatchByIds(ClientSession clientSession,String collectionName, Collection<Map<String, Object>> entityList);

    <T> Boolean doUpdateByColumn(T entity, SFunction<T, Object> column);

    <T> Boolean doUpdateByColumn(ClientSession clientSession,T entity, SFunction<T, Object> column);

    <T> Boolean doUpdateByColumn(T entity, String column);

    <T> Boolean doUpdateByColumn(ClientSession clientSession,T entity, String column);

    Boolean doUpdateByColumn(String collectionName,Map<String,Object> entityMap, String column);

    Boolean doUpdateByColumn(ClientSession clientSession,String collectionName,Map<String,Object> entityMap, String column);

    Boolean doRemoveById(Serializable id, Class<?> clazz);

    Boolean doRemoveById(ClientSession clientSession,Serializable id,Class<?> clazz);

    Boolean doRemoveById(String collectionName,Serializable id);

    Boolean doRemoveById(ClientSession clientSession,String collectionName,Serializable id);

    <T> Boolean doRemoveByColumn(SFunction<T, Object> column, String value,Class<T> clazz);

    <T> Boolean doRemoveByColumn(ClientSession clientSession,SFunction<T, Object> column, String value,Class<T> clazz);

    Boolean doRemoveByColumn(String column, String value,Class<?> clazz);

    Boolean doRemoveByColumn(ClientSession clientSession,String column, String value,Class<?> clazz);

    Boolean doRemoveByColumn(String collectionName,String column, String value);

    Boolean doRemoveByColumn(ClientSession clientSession,String collectionName,String column, String value);

    Boolean doRemoveBatchByIds(Collection<? extends Serializable> idList,Class<?> clazz);

    Boolean doRemoveBatchByIds(ClientSession clientSession,Collection<? extends Serializable> idList,Class<?> clazz);

    Boolean doRemoveBatchByIds(String collectionName,Collection<? extends Serializable> idList);

    Boolean doRemoveBatchByIds(ClientSession clientSession,String collectionName,Collection<? extends Serializable> idList);

    Boolean executeRemoveBatchByIds(ClientSession clientSession,Collection<? extends Serializable> idList,MongoCollection<Document> collection);

    <T> List<T> doList(Class<T> clazz);

    <T> List<T> doList(ClientSession clientSession, Class<T> clazz);

    List<Map<String, Object>> doList(String collectionName);

    List<Map<String, Object>> doList(ClientSession clientSession,String collectionName);

    List<Map<String, Object>> doList(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList);

    List<Map<String, Object>> doList(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList,List<BasicDBObject> basicDBObjectList);

    PageResult<Map<String, Object>> doPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize);

    PageResult<Map<String, Object>> doPage(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize);

    PageResult<Map<String,Object>> doPage(String collectionName,Integer pageNum,Integer pageSize);

    PageResult<Map<String,Object>> doPage(ClientSession clientSession,String collectionName,Integer pageNum,Integer pageSize);

    Map<String, Object> doOne(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList);

    Map<String, Object> doOne(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList);

    Map<String, Object> doLimitOne(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList);

    Map<String, Object> doLimitOne(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList);

    Map<String, Object> doGetById(String collectionName, Serializable id);

    Map<String, Object> doGetById(ClientSession clientSession,String collectionName, Serializable id);

    List<Map<String,Object>> doGetByIds(String collectionName, Collection<? extends Serializable> ids);
    List<Map<String,Object>> doGetByIds(ClientSession clientSession,String collectionName, Collection<? extends Serializable> ids);
    <T> List<T> doList(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz);
    <T> List<T> doList(ClientSession clientSession,List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz);
    <T> T doOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz);

    <T> T doOne(ClientSession clientSession,List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz);
    <T> T doLimitOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList,Class<T> clazz);

    <T> T doLimitOne(ClientSession clientSession,List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList,Class<T> clazz);

    <T> PageResult<T> doPage(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize,Class<T> clazz);

    <T> PageResult<T> doPage(ClientSession clientSession,List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize,Class<T> clazz);

    <T> T doGetById(Serializable id,Class<T> clazz);

    <T> T doGetById(ClientSession clientSession,Serializable id,Class<T> clazz);

    boolean doIsExist(Serializable id,Class<?> clazz);

    boolean doIsExist(ClientSession clientSession,Serializable id,Class<?> clazz);

    boolean doIsExistMap(String collectionName, Serializable id);

    boolean doIsExistMap(ClientSession clientSession,String collectionName, Serializable id);

    <T> List<T> doGetByIds(Collection<? extends Serializable> ids,Class<T> clazz);

    <T> List<T> doGetByIds(ClientSession clientSession,Collection<? extends Serializable> ids,Class<T> clazz);

    Boolean doUpdate(List<CompareCondition> compareConditionList,Class<?> clazz);

    Boolean doUpdate(ClientSession clientSession,List<CompareCondition> compareConditionList,Class<?> clazz);

    Boolean doUpdate(String collectionName,List<CompareCondition> compareConditionList);

    Boolean doUpdate(ClientSession clientSession,String collectionName,List<CompareCondition> compareConditionList);

    Boolean executeUpdate(ClientSession clientSession,List<CompareCondition> compareConditionList,MongoCollection<Document> collection);

    Boolean doRemove(List<CompareCondition> compareConditionList,Class<?> clazz);

    Boolean doRemove(ClientSession clientSession,List<CompareCondition> compareConditionList,Class<?> clazz);

    Boolean doRemove(String collectionName,List<CompareCondition> compareConditionList);

    Boolean doRemove(ClientSession clientSession,String collectionName,List<CompareCondition> compareConditionList);

    Boolean executeRemove(ClientSession clientSession,List<CompareCondition> compareConditionList,MongoCollection<Document> collection);

    long doCount(String collectionName,List<CompareCondition> compareConditionList);

    long doCount(ClientSession clientSession,String collectionName,List<CompareCondition> compareConditionList);

    long doCount(List<CompareCondition> compareConditionList,Class<?> clazz);

    long doCount(ClientSession clientSession,List<CompareCondition> compareConditionList,Class<?> clazz);

    long executeCountByCondition(ClientSession clientSession,List<CompareCondition> compareConditionList,MongoCollection<Document> collection);

    long doCount(String collectionName);

    long doCount(ClientSession clientSession,String collectionName);

    long doCount(Class<?> clazz);

    long doCount(ClientSession clientSession,Class<?> clazz);

    long executeCount(ClientSession clientSession,MongoCollection<Document> collection);

    <T> List<T> doAggregateList(List<BaseAggregate> aggregateList, List<BasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject, Class<T> clazz);

    <T> List<T> doAggregateList(ClientSession clientSession,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<T> clazz);

    List<Map<String,Object>> doAggregateList(String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject);

    List<Map<String,Object>> doAggregateList(ClientSession clientSession,String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject);

    <E> List<E> doAggregateList(String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<E> clazz);

    <E> List<E> doAggregateList(ClientSession clientSession,String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<E> clazz);

}
