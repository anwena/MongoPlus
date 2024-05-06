package com.anwen.mongo.execute.inject;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.convert.Converter;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.MongoPlusFieldException;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.model.*;
import com.anwen.mongo.toolkit.*;
import com.mongodb.BasicDBObject;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.FindIterable;
import com.mongodb.client.model.*;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;

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

    private final Log log = LogFactory.getLog(InjectAbstractExecute.class);

    private final Execute execute;

    private MongoConverter mongoConverter;

    public CollectionManager getCollectionManager() {
        return collectionManager;
    }

    public Execute getExecute() {
        return execute;
    }

    public InjectAbstractExecute(CollectionManager collectionManager, Execute execute, MongoConverter mongoConverter) {
        this.collectionManager = collectionManager;
        this.execute = execute;
        this.mongoConverter = mongoConverter;
    }

    public Boolean save(String collectionName, Map<String, Object> entityMap) {
        try {
            return execute.executeSave(Collections.singletonList(mongoConverter.write(entityMap)),collectionManager.getCollection(collectionName)).wasAcknowledged();
        } catch (Exception e) {
            log.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean saveBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        try {
//            List<Document> documentList = DocumentUtil.handleMapList(entityList,true);
            return execute.executeSave(mongoConverter.writeByUpdateBatch(entityList), collectionManager.getCollection(collectionName)).getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            log.error("saveBatch fail , error info : {}", e.getMessage(), e);
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

    public <T> Boolean saveOrUpdateWrapper(String collectionName, Map<String, Object> entityMap,List<CompareCondition> compareConditionList){
        long count = count(collectionName,compareConditionList);
        if (count > 0){
            BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
            Document document = mongoConverter.write(entityMap);
            document.remove(SqlOperationConstant._ID);
            BasicDBObject updateField = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
            return execute.executeUpdate(queryBasic,updateField,collectionManager.getCollection(collectionName)).getModifiedCount() >= 1;
        }
        return save(collectionName,entityMap);
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
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), mongoConverter.write(entityMap));
        return execute.executeUpdate(filter,update, collectionManager.getCollection(collectionName)).getModifiedCount() >= 1;
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
            throw new MongoPlusFieldException(column+" undefined");
        }
        Object columnValue = entityMap.get(column);
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(columnValue)) ? new ObjectId(String.valueOf(columnValue)) : columnValue);
        Document document = mongoConverter.write(entityMap);
        return execute.executeUpdate(filter,document, collectionManager.getCollection(collectionName)).getModifiedCount() >= 1;
    }

    public Boolean removeById(String collectionName, Serializable id) {
        Bson filterId = Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id);
        return execute.executeRemove(filterId, collectionManager.getCollection(collectionName)).getDeletedCount() >= 1;
    }

    public Boolean removeByColumn(String collectionName,String column, Object value) {
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return execute.executeRemove(filter,collectionManager.getCollection(collectionName)).getDeletedCount() >= 1;
    }

    public Boolean removeBatchByIds(String collectionName,Collection<? extends Serializable> idList) {
        List<Serializable> convertedIds = idList.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        Bson objectIdBson = Filters.in(SqlOperationConstant._ID, convertedIds);
        return execute.executeRemove(objectIdBson, collectionManager.getCollection(collectionName)).getDeletedCount() >= 1;
    }

    public List<Map<String, Object>> list(String collectionName) {
        return Converter.convertDocumentToMap(execute.executeQuery(null,null,null,collectionManager.getCollection(collectionName), Map.class));
    }

    public List<Map<String, Object>> list(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        FindIterable<Map> mapFindIterable = execute.executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(collectionName), Map.class);
        return Converter.convertDocumentToMap(mapFindIterable);
    }

    public PageResult<Map<String, Object>> page(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        FindIterable<Map> mapFindIterable = execute.executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(collectionName), Map.class);
        return lambdaOperate.getLambdaQueryResultPage(mapFindIterable,count(collectionName,compareConditionList),new PageParam(pageNum,pageSize));
    }

    public Map<String, Object> one(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, null, projectionList, basicDBObjectList);
        return Converter.convertDocumentToMapOne(execute.executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(), collectionManager.getCollection(collectionName),Map.class).limit(1));
    }

    public Map<String, Object> limitOne(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, null, projectionList, basicDBObjectList);
        return Converter.convertDocumentToMapOne(execute.executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(), collectionManager.getCollection(collectionName),Map.class).limit(1));
    }

    public boolean isExist(String collectionName, Serializable id){
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return execute.executeCount(queryBasic,null,collectionManager.getCollection(collectionName)) >= 1;
    }

    public boolean isExist(String collectionName,QueryChainWrapper<?,?> queryChainWrapper){
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(queryChainWrapper.getCompareList());
        return execute.executeCount(basicDBObject,null,collectionManager.getCollection(collectionName)) >= 1;
    }

    public List<Map<String,Object>> getByColumn(String collectionName,String column,Object value){
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return Converter.convertDocumentToMap(execute.executeQuery(filter,null,null,collectionManager.getCollection(collectionName),Map.class));
    }

    public Map<String, Object> getById(String collectionName,Serializable id) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return execute.executeQuery(queryBasic,null,null,collectionManager.getCollection(collectionName),Map.class).first();
    }

    public List<Map<String, Object>> getByIds(String collectionName,Collection<? extends Serializable> ids) {
        BasicDBObject basicDBObject = checkIdType(ids);
        FindIterable<Map> iterable = execute.executeQuery(basicDBObject,null,null, collectionManager.getCollection(collectionName),Map.class);
        return Converter.convertDocumentToMap(iterable);
    }

    private BasicDBObject checkIdType(Collection<? extends Serializable> ids) {
        List<Serializable> convertedIds = ids.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        return new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), convertedIds));
    }

    public List<Map<String,Object>> queryCommand(String collectionName,String sql){
        BasicDBObject basicDBObject = BasicDBObject.parse(sql);
        return Converter.convertDocumentToMap(execute.executeQuery(basicDBObject,null,null,collectionManager.getCollection(collectionName),Map.class));
    }

    public Boolean update(String collectionName,List<CompareCondition> compareConditionList){
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        List<CompareCondition> pushConditionList = compareConditionList.stream().filter(compareCondition -> Objects.equals(compareCondition.getCondition(), SpecialConditionEnum.PUSH.getSubCondition())).collect(Collectors.toList());
        List<CompareCondition> setConditionList = compareConditionList.stream().filter(compareCondition -> Objects.equals(compareCondition.getCondition(), SpecialConditionEnum.SET.getSubCondition())).collect(Collectors.toList());
        BasicDBObject basicDBObject = new BasicDBObject() {{
            if (CollUtil.isNotEmpty(setConditionList)){
                append(SpecialConditionEnum.SET.getCondition(), BuildCondition.buildUpdateValue(setConditionList));
            }
            if (CollUtil.isNotEmpty(pushConditionList)){
                append(SpecialConditionEnum.PUSH.getCondition(), BuildCondition.buildPushUpdateValue(pushConditionList));
            }
        }};
        BasicDBObject targetBasicDBObject = new BasicDBObject();
        mongoConverter.write(basicDBObject,targetBasicDBObject);
        return execute.executeUpdate(queryBasic,targetBasicDBObject,collectionManager.getCollection(collectionName)).getModifiedCount() >= 1;
    }

    public Boolean remove(String collectionName,List<CompareCondition> compareConditionList){
        return execute.executeRemove(BuildCondition.buildQueryCondition(compareConditionList),collectionManager.getCollection(collectionName)).getDeletedCount() >= 1;
    }

    public long count(String collectionName,List<CompareCondition> compareConditionList){
        return execute.executeCount(BuildCondition.buildQueryCondition(compareConditionList),null,collectionManager.getCollection(collectionName));
    }

    public List<Map<String,Object>> aggregateList(String collectionName, List<BaseAggregate> aggregateList, List<AggregateBasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject){
        List<AggregateBasicDBObject> aggregateConditionList = new ArrayList<AggregateBasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new AggregateBasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate(),aggregate.getOrder())));
            addAll(basicDBObjectList);
        }};
        aggregateConditionList.sort(Comparator.comparingInt(AggregateBasicDBObject::getOrder));
        AggregateIterable<Map> aggregateIterable = execute.executeAggregate(aggregateConditionList, collectionManager.getCollection(collectionName), Map.class);
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return Converter.convertDocumentToMap(aggregateIterable.iterator());
    }

    public <E> List<E> aggregateList(String collectionName, List<BaseAggregate> aggregateList, List<AggregateBasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject, Class<E> clazz){
        List<AggregateBasicDBObject> aggregateConditionList = new ArrayList<AggregateBasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new AggregateBasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate(),aggregate.getOrder())));
            addAll(basicDBObjectList);
        }};
        aggregateConditionList.sort(Comparator.comparingInt(AggregateBasicDBObject::getOrder));
        AggregateIterable<Document> aggregateIterable = execute.executeAggregate(aggregateConditionList, collectionManager.getCollection(collectionName),Document.class);
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz);
    }

    public long count(String collectionName){
        return execute.executeCount(null,null,collectionManager.getCollection(collectionName));
    }

    public String createIndex(String collectionName,Bson bson){
        return execute.doCreateIndex(bson,collectionManager.getCollection(collectionName));
    }

    public String createIndex(String collectionName,Bson bson, IndexOptions indexOptions){
        return execute.doCreateIndex(bson,indexOptions,collectionManager.getCollection(collectionName));
    }

    public List<String> createIndexes(String collectionName,List<IndexModel> indexes){
        return execute.doCreateIndexes(indexes,collectionManager.getCollection(collectionName));
    }


    public List<String> createIndexes(String collectionName,List<IndexModel> indexes, CreateIndexOptions createIndexOptions){
        return execute.doCreateIndexes(indexes,createIndexOptions,collectionManager.getCollection(collectionName));
    }

    public List<Document> listIndexes(String collectionName){
        return execute.doListIndexes(collectionManager.getCollection(collectionName));
    }

    public void dropIndex(String collectionName,String indexName){
        execute.doDropIndex(indexName,collectionManager.getCollection(collectionName));
    }

    public void dropIndex(String collectionName,String indexName, DropIndexOptions dropIndexOptions){
        execute.doDropIndex(indexName,dropIndexOptions,collectionManager.getCollection(collectionName));
    }

    public void dropIndex(String collectionName,Bson keys){
        execute.doDropIndex(keys,collectionManager.getCollection(collectionName));
    }

    public void dropIndex(String collectionName,Bson keys,DropIndexOptions dropIndexOptions){
        execute.doDropIndex(keys,dropIndexOptions,collectionManager.getCollection(collectionName));
    }

    public void dropIndexes(String collectionName){
        execute.doDropIndexes(collectionManager.getCollection(collectionName));
    }

    public void dropIndexes(String collectionName,DropIndexOptions dropIndexOptions){
        execute.doDropIndexes(dropIndexOptions,collectionManager.getCollection(collectionName));
    }

    protected void aggregateOptions(AggregateIterable<?> aggregateIterable,BasicDBObject optionsBasicDBObject){
        AggregateUtil.options(aggregateIterable, optionsBasicDBObject);
    }

}
