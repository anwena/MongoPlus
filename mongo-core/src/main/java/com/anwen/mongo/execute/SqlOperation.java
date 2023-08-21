package com.anwen.mongo.execute;

import cn.hutool.core.collection.CollUtil;
import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conn.ConnectMongoDB;
import com.anwen.mongo.constant.IdAutoConstant;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.convert.Converter;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.enums.QueryOperatorEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.model.*;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.BeanMapUtilByReflect;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.anwen.mongo.toolkit.codec.RegisterCodecUtil;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoException;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.FindOneAndUpdateOptions;
import com.mongodb.client.model.ReturnDocument;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.InsertManyResult;
import com.mongodb.client.result.InsertOneResult;
import com.mongodb.client.result.UpdateResult;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import org.bson.Document;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.types.ObjectId;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.stream.Collectors;

import static com.anwen.mongo.toolkit.BeanMapUtilByReflect.checkTableField;

/**
 * @Description: sql执行
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.sql
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-16 20:35
 * @Version: 1.0
 */
@Data
@Log4j2
public class SqlOperation<T> {

    private Map<String, MongoCollection<Document>> collectionMap = new HashMap<>();

    private List<SlaveDataSource> slaveDataSources;

    private BaseProperty baseProperty;

    private MongoClient mongoClient;

    // 实例化 ConnectMongoDB 对象，用于保存连接
    private ConnectMongoDB connectMongoDB;

    private Class<T> mongoEntity;

    private String createIndex = null;

    private String collectionName;

    private Boolean isItAutoId;

    public void setMongoEntity(Class<T> mongoEntity) {
        this.mongoEntity = mongoEntity;
    }

    public void init(Class<?> clazz) {
        String tableName = clazz.getSimpleName().toLowerCase();
        if (clazz.isAnnotationPresent(CollectionName.class)) {
            CollectionName annotation = clazz.getAnnotation(CollectionName.class);
            tableName = annotation.value();
            String dataSource = annotation.dataSource();
            if (StringUtils.isNotBlank(dataSource)) {
                Optional<SlaveDataSource> matchingSlave = slaveDataSources.stream()
                        .filter(slave -> Objects.equals(dataSource, slave.getSlaveName()))
                        .findFirst();
                if (matchingSlave.isPresent()) {
                    SlaveDataSource slave = matchingSlave.get();
                    baseProperty.setHost(slave.getHost());
                    baseProperty.setPort(slave.getPort());
                    baseProperty.setDatabase(slave.getDatabase());
                    baseProperty.setUsername(slave.getUsername());
                    baseProperty.setPassword(slave.getPassword());
                } else {
                    throw new InitMongoCollectionException("No matching slave data source configured");
                }
            }
        }
        try {
            connectMongoDB = new ConnectMongoDB(mongoClient, baseProperty.getDatabase(), tableName);
            MongoCollection<Document> collection = connectMongoDB.open();
            collectionMap.put(tableName, collection);
        } catch (MongoException e) {
            log.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    public Boolean doSave(T entity) {
        try {
            InsertOneResult insertOneResult = getCollection(entity).insertOne(processIdField(entity));
            return insertOneResult.wasAcknowledged();
        } catch (Exception e) {
            log.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSave(String collectionName, Map<String, Object> entityMap) {
        try {
            InsertOneResult insertOneResult = getCollection(collectionName).insertOne(new Document(entityMap));
            return insertOneResult.wasAcknowledged();
        } catch (Exception e) {
            log.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSaveBatch(Collection<T> entityList) {
        try {
            InsertManyResult insertManyResult = getCollection(entityList.iterator().next()).insertMany(processIdFieldList(entityList));
            return insertManyResult.getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            log.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSaveBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        try {
            InsertManyResult insertManyResult = getCollection(collectionName).insertMany(BeanMapUtilByReflect.mapListToDocumentList(entityList));
            return insertManyResult.getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            log.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }


    public Boolean doSaveOrUpdate(T entity) {
        try {
            Class<?> entityClass = entity.getClass().getSuperclass();
            Field field = entityClass.getFields()[0];
            String id = String.valueOf(field.get(entity));
            if (doGetById(id) == null) return doSave(entity);
            return doUpdateById(entity);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    public Boolean doSaveOrUpdate(String collectionName, Map<String, Object> entityMap) {
        if (entityMap.containsKey(SqlOperationConstant._ID)) {
            return doUpdateById(collectionName, entityMap);
        }
        return doSave(collectionName, entityMap);
    }


    public Boolean doSaveOrUpdateBatch(Collection<T> entityList) {
        List<T> insertList = new ArrayList<>();
        for (Document document : getCollection(entityList.iterator().next()).find(
                Filters.in(SqlOperationConstant._ID, entityList.stream().map(entity -> {
                    try {
                        return String.valueOf(entity.getClass().getSuperclass().getMethod("getId").invoke(entity));
                    } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                        throw new RuntimeException(e);
                    }
                }).collect(Collectors.toList()))
        )) {
            insertList.add((T) document);
            entityList.remove(document);
        }
        if (!insertList.isEmpty()) {
            return doSaveBatch(insertList);
        }
        return doUpdateBatchByIds(entityList);
    }

    public Boolean doSaveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        List<Map<String,Object>> insertList = new ArrayList<>();
        for (Document document : getCollection(collectionName).find(
                Filters.in(SqlOperationConstant._ID, entityList.stream().map(entity -> entity.get(SqlOperationConstant._ID)).collect(Collectors.toList()))
        )) {
            insertList.add(document);
            entityList.remove(document);
        }
        if (!insertList.isEmpty()) {
            return doSaveBatch(collectionName,insertList);
        }
        return doUpdateBatchByIds(collectionName,entityList);
    }


    public Boolean doUpdateById(T entity) {
        UpdateResult updateResult;
        Document document = new Document(checkTableField(entity));
        if (!document.containsKey(SqlOperationConstant._ID)){
            throw new MongoException("_id undefined");
        }
        Object _idValue = document.get(SqlOperationConstant._ID);
        BasicDBObject filter = new BasicDBObject(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(_idValue)) ? new ObjectId(String.valueOf(document.get(SqlOperationConstant._ID))) : String.valueOf(_idValue));
        document.remove(SqlOperationConstant._ID);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        updateResult = getCollection(entity).updateOne(filter, update);
        return updateResult.getModifiedCount() >= 1;
    }

    public Boolean doUpdateById(String collectionName, Map<String, Object> entityMap) {
        if (!entityMap.containsKey(SqlOperationConstant._ID)) {
            throw new MongoException("_id undefined");
        }
        UpdateResult updateResult;
        Object _idValue = entityMap.get(SqlOperationConstant._ID);
        BasicDBObject filter = new BasicDBObject(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(_idValue)) ? new ObjectId(String.valueOf(entityMap.get(SqlOperationConstant._ID))) : String.valueOf(_idValue));
        entityMap.remove(SqlOperationConstant._ID);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), new Document(entityMap));
        updateResult = getCollection(collectionName).updateOne(filter, update);
        return updateResult.getModifiedCount() >= 1;
    }

    @Deprecated
    public Boolean doUpdateBatchByIds(Collection<T> entityList) {
        for (T entity : entityList) {
            UpdateResult updateResult;
            try {
                updateResult = getCollection(entity).updateMany(Filters.eq(SqlOperationConstant._ID, entity.getClass().getSuperclass().getMethod("getId").invoke(entity)), new Document(checkTableField(entity)));
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
            return updateResult.getModifiedCount() == entityList.size();
        }
        return false;
    }

    @Deprecated
    public Boolean doUpdateBatchByIds(String collectionName, Collection<Map<String, Object>> entityList) {
        for (Map<String, Object> entity : entityList) {
            UpdateResult updateResult = getCollection(collectionName).updateOne(Filters.eq(SqlOperationConstant._ID, new ObjectId(String.valueOf(entity.get(SqlOperationConstant._ID)))), new Document(entity));
            return updateResult.getModifiedCount() == entityList.size();
        }
        return false;
    }


    public Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        try {
            String filterCondition = column.getFieldNameLine();
            String filterValue = String.valueOf(entity.getClass().getMethod(column.getFieldName()).invoke(entity));
            UpdateResult updateResult = getCollection(entity).updateOne(Filters.eq(filterCondition, ObjectId.isValid(filterValue) ? new ObjectId(filterValue) : filterValue), new Document(checkTableField(entity)));
            return updateResult.getModifiedCount() >= 1;
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            log.error("update fail , fail info : {}", e.getMessage(), e);
            return false;
        }
    }


    public Boolean doUpdateByColumn(T entity, String column) {
        String filterValue = String.valueOf(ClassTypeUtil.getClassFieldValue(entity,column));
        UpdateResult updateResult = getCollection(entity).updateOne(Filters.eq(column, ObjectId.isValid(filterValue) ? new ObjectId(filterValue) : filterValue), new Document(checkTableField(entity)));
        return updateResult.getModifiedCount() >= 1;
    }

    public Boolean doUpdateByColumn(String collectionName,Map<String,Object> entityMap, String column) {
        if (!entityMap.containsKey(column)){
            throw new MongoException(column+" undefined");
        }
        String columnValue = String.valueOf(entityMap.get(column));
        UpdateResult updateResult = getCollection(collectionName).updateOne(Filters.eq(column, ObjectId.isValid(String.valueOf(columnValue)) ? new ObjectId(columnValue) : columnValue), new Document(entityMap));
        return updateResult.getModifiedCount() >= 1;
    }


    public Boolean doRemoveById(Serializable id) {
        return getCollection().deleteOne(Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id))).getDeletedCount() >= 1;
    }

    public Boolean doRemoveById(String collectionName,Serializable id) {
        return getCollection(collectionName).deleteOne(Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id))).getDeletedCount() >= 1;
    }


    public Boolean doRemoveByColumn(SFunction<T, Object> column, String value) {
        return getCollection().deleteOne(Filters.eq(column.getFieldNameLine(), ObjectId.isValid(value) ? new ObjectId(value) : value)).getDeletedCount() >= 1;
    }


    public Boolean doRemoveByColumn(String column, String value) {
        return getCollection().deleteOne(Filters.eq(column, ObjectId.isValid(value) ? new ObjectId(value) : value)).getDeletedCount() >= 1;
    }

    public Boolean doRemoveByColumn(String collectionName,String column, String value) {
        return getCollection(collectionName).deleteOne(Filters.eq(column, ObjectId.isValid(value) ? new ObjectId(value) : value)).getDeletedCount() >= 1;
    }


    public Boolean doRemoveBatchByIds(Collection<Serializable> idList) {
        List<Serializable> serializableList = idList.stream().filter(id -> ObjectId.isValid(String.valueOf(id))).collect(Collectors.toList());
        if (idList.size() == serializableList.size()){
            return getCollection().deleteMany(Filters.in(SqlOperationConstant._ID, idList.stream().map(id -> new ObjectId(String.valueOf(id))).collect(Collectors.toList()))).getDeletedCount() >= 1;
        } else if (CollUtil.isEmpty(serializableList)){
            return getCollection().deleteMany(Filters.in(SqlOperationConstant._ID, idList.stream().map(String::valueOf))).getDeletedCount() >= 1;
        } else {
            int ling = 0;
            for (Serializable serializable : idList) {
                Boolean _b = doRemoveById(serializable);
                if (_b) ling++;
            }
            return ling >= 1;
        }
    }

    public Boolean doRemoveBatchByIds(String collectionName,Collection<Serializable> idList) {
        List<Serializable> serializableList = idList.stream().filter(id -> ObjectId.isValid(String.valueOf(id))).collect(Collectors.toList());
        if (idList.size() == serializableList.size()){
            return getCollection(collectionName).deleteMany(Filters.in(SqlOperationConstant._ID, idList.stream().map(id -> new ObjectId(String.valueOf(id))).collect(Collectors.toList()))).getDeletedCount() >= 1;
        } else if (CollUtil.isEmpty(serializableList)){
            return getCollection(collectionName).deleteMany(Filters.in(SqlOperationConstant._ID, idList.stream().map(String::valueOf))).getDeletedCount() >= 1;
        } else {
            int ling = 0;
            for (Serializable serializable : idList) {
                Boolean _b = doRemoveById(serializable);
                if (_b) ling++;
            }
            return ling >= 1;
        }
    }

    public List<T> doList() {
        MongoCollection<Document> collection = getCollection();
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return DocumentMapperConvert.mapDocumentList(collection.find(),mongoEntity);
    }

    public List<Map<String, Object>> doList(String collectionName) {
        MongoCollection<Document> collection = getCollection(collectionName);
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return Converter.convertDocumentToMap(collection.find(Map.class), Math.toIntExact(doCount(collectionName)));
    }

    public List<Map<String, Object>> doList(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return getLambdaQueryResult(collectionName, compareConditionList, orderList,projectionList,basicDBObjectList);
    }

    public PageResult<Map<String, Object>> doPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        return getLambdaQueryResultPage(collectionName, compareConditionList, orderList,projectionList, basicDBObjectList,new PageParam(pageNum, pageSize));
    }

    public PageResult<Map<String,Object>> doPage(String collectionName,Integer pageNum,Integer pageSize){
        return getLambdaQueryResultPage(collectionName,null,null,null,null,new PageParam(pageNum,pageSize));
    }

    public Map<String, Object> doOne(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        List<Map<String, Object>> result = getLambdaQueryResult(collectionName, compareConditionList, null,projectionList,basicDBObjectList);
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : new HashMap<>();
    }

    public Map<String, Object> doLimitOne(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        List<Map<String, Object>> result = getLambdaQueryResult(collectionName, compareConditionList, null,projectionList,basicDBObjectList);
        return !result.isEmpty() ? result.get(0) : new HashMap<>();
    }


    public T doGetById(String collectionName, Serializable id) {
        BasicDBObject byId = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), id));
        FindIterable<Document> iterable = getCollection(collectionName).find(byId);
        return (T) iterable.first();
    }

    public List<T> doGetByIds(String collectionName, Collection<Serializable> ids) {
        List<Serializable> serializableList = ids.stream().filter(id -> ObjectId.isValid(String.valueOf(id))).collect(Collectors.toList());
        if (ids.size() == serializableList.size()){
            ids = ids.stream().map(id -> new ObjectId(String.valueOf(id))).collect(Collectors.toList());
        } else if (CollUtil.isEmpty(serializableList)){
            ids = ids.stream().map(String::valueOf).collect(Collectors.toList());
        }else {
            List<Serializable> idList = new ArrayList<>();
            ids.forEach(id -> {
                idList.add(ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id));
            });
            ids = idList;
        }
        BasicDBObject query = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), ids));
        FindIterable<Document> iterable = getCollection(collectionName).find(query);
        return DocumentMapperConvert.mapDocumentList(iterable, mongoEntity);
    }


    public List<T> doList(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return getLambdaQueryResult(compareConditionList, orderList,projectionList,basicDBObjectList);
    }

    public T doOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        List<T> result = getLambdaQueryResult(compareConditionList, null,projectionList,basicDBObjectList);
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : null;
    }

    public T doLimitOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        List<T> result = getLambdaQueryResult(compareConditionList, null,projectionList,basicDBObjectList);
        return !result.isEmpty() ? result.get(0) : null;
    }

    public PageResult<T> doPage(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        return getLambdaQueryResultPage(compareConditionList, orderList,projectionList, basicDBObjectList,new PageParam(pageNum, pageSize));
    }

    public T doGetById(Serializable id) {
        BasicDBObject byId = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id)));
        FindIterable<Document> iterable = getCollection().find(byId);
        return DocumentMapperConvert.mapDocument(iterable.first(),mongoEntity);
    }

    public List<T> doGetByIds(Collection<Serializable> ids) {

        BasicDBObject query = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), ids));
        FindIterable<Document> iterable = getCollection().find(query);
        return DocumentMapperConvert.mapDocumentList(iterable, mongoEntity);
    }

    public Boolean doUpdate(List<CompareCondition> compareConditionList) {
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        BasicDBObject updateBasic = BuildCondition.buildUpdateValue(compareConditionList);
        UpdateResult updateResult = getCollection().updateMany(queryBasic, new BasicDBObject() {{
            append(SpecialConditionEnum.SET.getCondition(), updateBasic);
        }});
        return updateResult.getModifiedCount() >= 1;
    }

    public Boolean doUpdate(String collectionName,List<CompareCondition> compareConditionList){
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        BasicDBObject updateBasic = BuildCondition.buildUpdateValue(compareConditionList);
        UpdateResult updateResult = getCollection(collectionName).updateMany(queryBasic, new BasicDBObject() {{
            append(SpecialConditionEnum.SET.getCondition(), updateBasic);
        }});
        return updateResult.getModifiedCount() >= 1;
    }

    public Boolean doRemove(List<CompareCondition> compareConditionList) {
        BasicDBObject deleteBasic = BuildCondition.buildQueryCondition(compareConditionList);
        DeleteResult deleteResult = getCollection().deleteMany(deleteBasic);
        return deleteResult.getDeletedCount() >= 1;
    }

    public Boolean doRemove(String collectionName,List<CompareCondition> compareConditionList){
        BasicDBObject deleteBasic = BuildCondition.buildQueryCondition(compareConditionList);
        DeleteResult deleteResult = getCollection(collectionName).deleteMany(deleteBasic);
        return deleteResult.getDeletedCount() >= 1;
    }

    public long doCount(String collectionName,List<CompareCondition> compareConditionList){
        return getCollection(collectionName).countDocuments(BuildCondition.buildQueryCondition(compareConditionList));
    }

    public long doCount(String collectionName){
        return getCollection(collectionName).countDocuments();
    }

    public long doCount(){
        return getCollection().countDocuments();
    }

    public long doCount(List<CompareCondition> compareConditionList){
        return getCollection().countDocuments(BuildCondition.buildQueryCondition(compareConditionList));
    }

    public List<T> doAggregateList(List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList){
        AggregateIterable<Document> aggregateIterable = getCollection().aggregate(
                new ArrayList<BasicDBObject>(){{
                    aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
                    addAll(basicDBObjectList);
                }}
        );
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),mongoEntity);
    }

    public List<Map<String,Object>> doAggregateList(String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList){
        AggregateIterable<Map> aggregateIterable = getCollection(collectionName).aggregate(
                new ArrayList<BasicDBObject>(){{
                    aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
                    addAll(basicDBObjectList);
                }}, Map.class);
        return Converter.convertDocumentToMap(aggregateIterable.iterator());
    }

    public <E> List<E> doAggregateList(List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,Class<E> clazz){
        AggregateIterable<Document> aggregateIterable = getCollection().aggregate(
                new ArrayList<BasicDBObject>(){{
                    aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
                    addAll(basicDBObjectList);
                }}
        );
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz != null ? clazz : mongoEntity);
    }

    public <E> List<E> doAggregateList(String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,Class<E> clazz){
        AggregateIterable<Document> aggregateIterable = getCollection(collectionName).aggregate(
                new ArrayList<BasicDBObject>(){{
                    aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
                    addAll(basicDBObjectList);
                }}
        );
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz != null ? clazz : mongoEntity);
    }


    private List<T> getLambdaQueryResult(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return DocumentMapperConvert.mapDocumentList(baseLambdaQuery(compareConditionList, orderList,projectionList,basicDBObjectList), mongoEntity);
    }

    private List<Map<String, Object>> getLambdaQueryResult(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return Converter.convertDocumentToMap(baseLambdaQuery(collectionName, compareConditionList, orderList,projectionList,basicDBObjectList), Math.toIntExact(doCount(collectionName, compareConditionList)));
    }

    /**
     * 查询执行
     *
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:51
     */
    private FindIterable<Document> baseLambdaQuery(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        BasicDBObject sortCond = new BasicDBObject();
        orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        MongoCollection<Document> collection = getCollection();
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(compareConditionList);
        if (CollUtil.isNotEmpty(basicDBObjectList)){
            basicDBObjectList.forEach(basic -> {
                basicDBObject.putAll(basic.toMap());
            });
        }
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return collection.find(basicDBObject).projection(BuildCondition.buildProjection(projectionList)).sort(sortCond);
    }

    private FindIterable<Map> baseLambdaQuery(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        BasicDBObject sortCond = new BasicDBObject();
        orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        MongoCollection<Document> collection = getCollection(collectionName);
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(compareConditionList);
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        if (CollUtil.isNotEmpty(basicDBObjectList)){
            basicDBObjectList.forEach(basic -> {
                basicDBObject.putAll(basic.toMap());
            });
        }
        return collection.find(basicDBObject,Map.class).projection(BuildCondition.buildProjection(projectionList)).sort(sortCond);
    }

    private PageResult<T> getLambdaQueryResultPage(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams) {
        PageResult<T> pageResult = new PageResult<>();
        FindIterable<Document> documentFindIterable = baseLambdaQuery(compareConditionList, orderList,projectionList,basicDBObjectList);
        long totalSize = doCount(compareConditionList);
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData(DocumentMapperConvert.mapDocumentList(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize()), mongoEntity));
        return pageResult;
    }

    private PageResult<Map<String, Object>> getLambdaQueryResultPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams) {
        PageResult<Map<String, Object>> pageResult = new PageResult<>();
        FindIterable<Map> documentFindIterable = baseLambdaQuery(collectionName, compareConditionList, orderList,projectionList,basicDBObjectList);
        long totalSize = doCount(collectionName,compareConditionList);
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData(Converter.convertDocumentToMap(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize())));
        return pageResult;
    }

    private MongoCollection<Document> getCollection(T entity) {
        return getCollection().withCodecRegistry(CodecRegistries.fromRegistries(RegisterCodecUtil.registerCodec(entity)));
    }

    private MongoCollection<Document> getCollection(Map<String, Object> entityMap, String collectionName) {
        return getCollection(collectionName).withCodecRegistry(CodecRegistries.fromRegistries(RegisterCodecUtil.registerCodec(entityMap)));
    }

    private MongoCollection<Document> getCollection() {
        createIndex = null;
        Class<?> clazz = mongoEntity;
        String collectionName = clazz.getSimpleName().toLowerCase();
        if (clazz.isAnnotationPresent(CollectionName.class)) {
            collectionName = clazz.getAnnotation(CollectionName.class).value();
        }
        return getCollection(collectionName);
    }

    private MongoCollection<Document> getCollection(String collectionName) {
        createIndex = null;
        this.collectionName = collectionName;
        // 检查连接是否需要重新创建
        if (!this.collectionMap.containsKey(collectionName)) {
            if (connectMongoDB == null || !Objects.equals(connectMongoDB.getCollection(), collectionName)){
                connectMongoDB = new ConnectMongoDB(mongoClient, baseProperty.getDatabase(), collectionName);
            }
            MongoCollection<Document> mongoCollection = connectMongoDB.open();
            this.collectionMap.put(collectionName, mongoCollection);
            return mongoCollection;
        }
        return this.collectionMap.get(collectionName);
    }

    private Document processIdField(T entity){
        Map<String, Object> tableFieldMap = checkTableField(entity);
        if (IdAutoConstant.IS_IT_AUTO_ID){
            long num = 1L;
            MongoCollection<Document> collection = getCollection("counters");
            Document query = new Document(SqlOperationConstant._ID, collectionName);
            Document update = new Document("$inc", new Document(SqlOperationConstant.AUTO_NUM, num));
            Document document = collection.findOneAndUpdate(query,update,new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER));
            if (document == null){
                Long finalNum = num;
                collection.insertOne(new Document(new HashMap<String,Object>(){{
                    put(SqlOperationConstant._ID,collectionName);
                    put(SqlOperationConstant.AUTO_NUM, finalNum);
                }}));
            }else {
                num = Long.parseLong(String.valueOf(document.get(SqlOperationConstant.AUTO_NUM)));
            }
            tableFieldMap.put(SqlOperationConstant._ID,num);
            tableFieldMap.remove(SqlOperationConstant.IS_IT_AUTO_ID);
        }
        return new Document(tableFieldMap);
    }

    private List<Document> processIdFieldList(Collection<T> entityList){
        return entityList.stream().map(this::processIdField).collect(Collectors.toList());
    }
}
