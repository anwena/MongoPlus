package com.anwen.mongo.execute.inject;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.convert.Converter;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.model.*;
import com.anwen.mongo.toolkit.*;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoException;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.ClientSession;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.*;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * Map类型
 * @author JiaChaoYang
 **/
public class InjectAbstractExecute {

    private final CollectionManager collectionManager;

    private final LambdaOperate lambdaOperate = new LambdaOperate();

    private final Logger logger = LoggerFactory.getLogger(InjectAbstractExecute.class);

    private final Execute execute;

    public InjectAbstractExecute(CollectionManager collectionManager, Execute execute) {
        this.collectionManager = collectionManager;
        this.execute = execute;
    }

    public Boolean save(String collectionName, Map<String, Object> entityMap) {
        try {
            return execute.doSave(DocumentUtil.handleMap(entityMap,true),collectionManager.getCollection(collectionName)).wasAcknowledged();
        } catch (Exception e) {
            logger.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean saveBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        try {
            List<Document> documentList = DocumentUtil.handleDocumentList(BeanMapUtilByReflect.mapListToDocumentList(entityList),true);
            return execute.doSaveBatch(documentList, collectionManager.getCollection(collectionName)).getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            logger.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean saveOrUpdate(String collectionName, Map<String, Object> entityMap) {
        String idValue = String.valueOf(entityMap.getOrDefault(SqlOperationConstant._ID,""));
        if (StringUtils.isBlank(idValue)) {
            return save(collectionName,entityMap);
        }
        return isExist(collectionName,idValue) ? updateById(collectionName,entityMap) : save(collectionName,entityMap);
    }

    public Boolean saveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        List<Map<String,Object>> saveList = new ArrayList<>();
        List<Map<String,Object>> updateList = new ArrayList<>();
        entityList.parallelStream().forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if ((StringUtils.isBlank(idByEntity) || !isExist(collectionName,idByEntity))) {
                saveList.add(entity);
            } else {
                updateList.addAll(entityList);
            }
        });
        boolean save = false;
        boolean update = false;
        if (!saveList.isEmpty()){
            save = saveBatch(collectionName,saveList);
        }
        if (!updateList.isEmpty()){
            update = updateBatchByIds(collectionName,updateList);
        }
        return save == update;
    }

    public Boolean updateById(String collectionName, Map<String, Object> entityMap) {
        BasicDBObject filter = ExecuteUtil.getFilter(entityMap);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), DocumentUtil.handleMap(entityMap,false));
        return execute.doUpdateById(filter,update, collectionManager.getCollection(collectionName)).getModifiedCount() >= 1;
    }

    public Boolean updateBatchByIds(String collectionName, Collection<Map<String, Object>> entityList) {
        AtomicInteger line = new AtomicInteger();
        entityList.forEach(entity -> {
            line.addAndGet(updateById(collectionName, entity) ? 1 : 0);
        });
        return line.get() == entityList.size();
    }

    public Boolean updateByColumn(String collectionName, Map<String,Object> entityMap, String column) {
        if (!entityMap.containsKey(column)){
            throw new MongoException(column+" undefined");
        }
        Object columnValue = entityMap.get(column);
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(columnValue)) ? new ObjectId(String.valueOf(columnValue)) : columnValue);
        Document document = DocumentUtil.handleMap(entityMap,false);
        return execute.doUpdateByColumn(filter,document, collectionManager.getCollection(collectionName)).getModifiedCount() >= 1;
    }

    public Boolean removeById(String collectionName, Serializable id) {
        Bson filterId = Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id);
        return execute.executeRemove(filterId, collectionManager.getCollection(collectionName)).getDeletedCount() >= 1;
    }

    public Boolean removeByColumn(String collectionName,String column, Object value) {
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return execute.executeRemoveByColumn(filter,collectionManager.getCollection(collectionName)).getDeletedCount() >= 1;
    }

    public Boolean removeBatchByIds(String collectionName,Collection<? extends Serializable> idList) {
        List<Serializable> convertedIds = idList.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        Bson objectIdBson = Filters.in(SqlOperationConstant._ID, convertedIds);
        return execute.executeRemoveBatchByIds(objectIdBson, collectionManager.getCollection(collectionName)).getDeletedCount() >= 1;
    }

    public List<Map<String, Object>> list(String collectionName) {
        return Converter.convertDocumentToMap(execute.doList(collectionManager.getCollection(collectionName), Map.class));
    }

    public List<Map<String, Object>> list(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        FindIterable<Map> mapFindIterable = execute.doList(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(collectionName), Map.class);
        return Converter.convertDocumentToMap(mapFindIterable);
    }

    public PageResult<Map<String, Object>> page(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        FindIterable<Map> mapFindIterable = execute.doList(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(collectionName), Map.class);
        return lambdaOperate.getLambdaQueryResultPage(mapFindIterable,count(collectionName,compareConditionList),new PageParam(pageNum,pageSize));
    }

    public Map<String, Object> one(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, null, projectionList, basicDBObjectList);
        List<Map<String, Object>> result = Converter.convertDocumentToMap(execute.doList(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(collectionName), Map.class));
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : null;
    }

    public Map<String, Object> limitOne(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, null, projectionList, basicDBObjectList);
        List<Map<String, Object>> result = Converter.convertDocumentToMap(execute.doList(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(collectionName), Map.class));
        return !result.isEmpty() ? result.get(0) : new HashMap<>();
    }

    public boolean isExist(String collectionName, Serializable id){
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return execute.executeExist(queryBasic,collectionManager.getCollection(collectionName)) >= 1;
    }

    public List<Map<String,Object>> getByColumn(String collectionName,String column,Object value){
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return Converter.convertDocumentToMap(execute.doGetByColumn(filter,collectionManager.getCollection(collectionName),Map.class));
    }

    public List<Map<String,Object>> queryCommand(String collectionName,String sql){
        BasicDBObject basicDBObject = BasicDBObject.parse(sql);
        return Converter.convertDocumentToMap(execute.doQueryCommand(basicDBObject,collectionManager.getCollection(collectionName),Map.class));
    }

    public Boolean update(String collectionName,List<CompareCondition> compareConditionList){
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        BasicDBObject updateBasic = BuildCondition.buildUpdateValue(compareConditionList);
        BasicDBObject basicDBObject = new BasicDBObject() {{
            append(SpecialConditionEnum.SET.getCondition(), updateBasic);
        }};
        return execute.executeUpdate(queryBasic,basicDBObject,collectionManager.getCollection(collectionName)).getModifiedCount() >= 1;
    }

    public Boolean remove(String collectionName,List<CompareCondition> compareConditionList){
        return execute.executeRemove(BuildCondition.buildQueryCondition(compareConditionList),collectionManager.getCollection(collectionName)).getDeletedCount() >= 1;
    }

    public long count(String collectionName,List<CompareCondition> compareConditionList){
        return execute.executeCountByCondition(BuildCondition.buildQueryCondition(compareConditionList),collectionManager.getCollection(collectionName));
    }

    public List<Map<String,Object>> doAggregateList(String collectionName, List<BaseAggregate> aggregateList, List<AggregateBasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject){
        MongoCollection<Document> collection = getCollection(collectionName);
        List<BasicDBObject> aggregateConditionList = new ArrayList<BasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
            addAll(basicDBObjectList);
        }};
        AggregateIterable<Map> aggregateIterable = Optional.ofNullable(clientSession).map(session -> collection.aggregate(session,aggregateConditionList,Map.class)).orElseGet(() -> collection.aggregate(aggregateConditionList,Map.class));
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return Converter.convertDocumentToMap(aggregateIterable.iterator());
    }

    public long count(String collectionName){
        return execute.doCount(collectionManager.getCollection(collectionName));
    }

    public String createIndex(Bson bson, MongoCollection<Document> collection){
        return execute.doCreateIndex(bson,collection);
    }

    public String createIndex(Bson bson, IndexOptions indexOptions, MongoCollection<Document> collection){
        return execute.doCreateIndex(bson,indexOptions,collection);
    }

    public List<String> createIndexes(List<IndexModel> indexes, MongoCollection<Document> collection){
        return execute.doCreateIndexes(indexes,collection);
    }


    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions, MongoCollection<Document> collection){
        return execute.doCreateIndexes(indexes,createIndexOptions,collection);
    }

    public List<Document> listIndexes(MongoCollection<Document> collection){
        return execute.doListIndexes(collection);
    }

    public void dropIndex(String indexName,MongoCollection<Document> collection){
        execute.doDropIndex(indexName,collection);
    }

    public void dropIndex(String indexName, DropIndexOptions dropIndexOptions, MongoCollection<Document> collection){
        execute.doDropIndex(indexName,dropIndexOptions,collection);
    }

    public void dropIndex(Bson keys,MongoCollection<Document> collection){
        execute.doDropIndex(keys,collection);
    }

    public void dropIndex(Bson keys,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection){
        execute.doDropIndex(keys,dropIndexOptions,collection);
    }

    public void dropIndexes(MongoCollection<Document> collection){
        execute.doDropIndexes(collection);
    }

    public void dropIndexes(DropIndexOptions dropIndexOptions,MongoCollection<Document> collection){
        execute.doDropIndexes(dropIndexOptions,collection);
    }

}
