package com.anwen.mongo.execute;

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
import com.anwen.mongo.enums.AggregateOptionsEnum;
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
import com.mongodb.client.model.Collation;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.FindOneAndUpdateOptions;
import com.mongodb.client.model.ReturnDocument;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.InsertManyResult;
import com.mongodb.client.result.InsertOneResult;
import com.mongodb.client.result.UpdateResult;
import org.bson.BsonValue;
import org.bson.Document;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.TimeUnit;
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
public class SqlExecute {

    private static final Logger logger = LoggerFactory.getLogger(SqlExecute.class);

    private Map<String, MongoCollection<Document>> collectionMap = new HashMap<>();

    private List<SlaveDataSource> slaveDataSources;

    private BaseProperty baseProperty;

    private MongoClient mongoClient;

    // 实例化 ConnectMongoDB 对象，用于保存连接
    private ConnectMongoDB connectMongoDB;

    private Class<?> mongoEntity;

    private String createIndex = null;

    private String collectionName;

    public void setMongoEntity(Class<?> mongoEntity) {
        this.mongoEntity = mongoEntity;
    }

    public void init() {
        String tableName = mongoEntity.getSimpleName().toLowerCase();
        if (mongoEntity.isAnnotationPresent(CollectionName.class)) {
            CollectionName annotation = mongoEntity.getAnnotation(CollectionName.class);
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
            logger.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    public <T> Boolean doSave(T entity) {
        try {
            InsertOneResult insertOneResult = getCollection(entity).insertOne(processIdField(entity));
            return insertOneResult.wasAcknowledged();
        } catch (Exception e) {
            logger.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSave(String collectionName, Map<String, Object> entityMap) {
        try {
            InsertOneResult insertOneResult = getCollection(collectionName).insertOne(new Document(entityMap));
            return insertOneResult.wasAcknowledged();
        } catch (Exception e) {
            logger.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public <T> Boolean doSaveBatch(Collection<T> entityList) {
        try {
            InsertManyResult insertManyResult = getCollection(entityList.iterator().next()).insertMany(processIdFieldList(entityList));
            return insertManyResult.getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            logger.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSaveBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        try {
            InsertManyResult insertManyResult = getCollection(collectionName).insertMany(BeanMapUtilByReflect.mapListToDocumentList(entityList));
            return insertManyResult.getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            logger.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public <T> Boolean doSaveOrUpdate(T entity) {
        String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
        if (StringUtils.isBlank(idByEntity)){
            return doSave(entity);
        }
        return doIsExist(idByEntity) ? doUpdateById(entity) : doSave(entity);
    }

    public Boolean doSaveOrUpdate(String collectionName, Map<String, Object> entityMap) {
        String idValue = String.valueOf(entityMap.getOrDefault(SqlOperationConstant._ID,""));
        if (StringUtils.isBlank(idValue)) {
            return doSave(collectionName, entityMap);
        }
        return doIsExist(collectionName,idValue) ? doUpdateById(collectionName,entityMap) : doSave(collectionName,entityMap);
    }

    public <T> Boolean doSaveOrUpdateBatch(Collection<T> entityList) {
        List<T> saveList = new ArrayList<>();
        List<T> updateList = new ArrayList<>();
        entityList.parallelStream().forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if ((StringUtils.isBlank(idByEntity) || !doIsExist(idByEntity))) {
                saveList.add(entity);
            } else {
                updateList.addAll(entityList);
            }
        });
        boolean save = false;
        boolean update = false;
        if (!saveList.isEmpty()){
            save = doSaveBatch(saveList);
        }
        if (!updateList.isEmpty()){
            update = doUpdateBatchByIds(updateList);
        }
        return save == update;
    }

    public Boolean doSaveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        List<Map<String,Object>> saveList = new ArrayList<>();
        List<Map<String,Object>> updateList = new ArrayList<>();
        entityList.parallelStream().forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if ((StringUtils.isBlank(idByEntity) || !doIsExist(idByEntity))) {
                saveList.add(entity);
            } else {
                updateList.addAll(entityList);
            }
        });
        boolean save = false;
        boolean update = false;
        if (!saveList.isEmpty()){
            save = doSaveBatch(saveList);
        }
        if (!updateList.isEmpty()){
            update = doUpdateBatchByIds(updateList);
        }
        return save == update;
    }


    public <T> Boolean doUpdateById(T entity) {
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

    public <T> Boolean doUpdateBatchByIds(Collection<T> entityList) {
        int line = 0;
        for (T entity : entityList) {
            line+=doUpdateById(entity) ? 1 : 0;
        }
        return line == entityList.size();
    }

    public Boolean doUpdateBatchByIds(String collectionName, Collection<Map<String, Object>> entityList) {
        int line = 0;
        for (Map<String,Object> entity : entityList) {
            line+=doUpdateById(collectionName,entity) ? 1 : 0;
        }
        return line == entityList.size();
    }


    public <T> Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        try {
            String filterCondition = column.getFieldNameLine();
            String filterValue = String.valueOf(entity.getClass().getMethod(column.getFieldName()).invoke(entity));
            UpdateResult updateResult = getCollection(entity).updateOne(Filters.eq(filterCondition, ObjectId.isValid(filterValue) ? new ObjectId(filterValue) : filterValue), new Document(checkTableField(entity)));
            return updateResult.getModifiedCount() >= 1;
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            logger.error("update fail , fail info : {}", e.getMessage(), e);
            return false;
        }
    }


    public <T> Boolean doUpdateByColumn(T entity, String column) {
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


    public <T> Boolean doRemoveByColumn(SFunction<T, Object> column, String value) {
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
        } else if (serializableList.isEmpty()){
            return getCollection().deleteMany(Filters.in(SqlOperationConstant._ID, idList.stream().map(String::valueOf))).getDeletedCount() >= 1;
        } else {
            int line = 0;
            for (Serializable serializable : idList) {
                line+= doRemoveById(serializable) ? 1 : 0;
            }
            return line >= 1;
        }
    }

    public Boolean doRemoveBatchByIds(String collectionName,Collection<Serializable> idList) {
        List<Serializable> serializableList = idList.stream().filter(id -> ObjectId.isValid(String.valueOf(id))).collect(Collectors.toList());
        if (idList.size() == serializableList.size()){
            return getCollection(collectionName).deleteMany(Filters.in(SqlOperationConstant._ID, idList.stream().map(id -> new ObjectId(String.valueOf(id))).collect(Collectors.toList()))).getDeletedCount() >= 1;
        } else if (serializableList.isEmpty()){
            return getCollection(collectionName).deleteMany(Filters.in(SqlOperationConstant._ID, idList.stream().map(String::valueOf))).getDeletedCount() >= 1;
        } else {
            int line = 0;
            for (Serializable serializable : idList) {
                line+=doRemoveById(serializable) ? 1 : 0;
            }
            return line >= 1;
        }
    }

    public <T> List<T> doList() {
        MongoCollection<Document> collection = getCollection();
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return (List<T>) DocumentMapperConvert.mapDocumentList(collection.find(),mongoEntity);
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


    public Map<String, Object> doGetById(String collectionName, Serializable id) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id)));
        return getCollection(collectionName).find(queryBasic,Map.class).first();
    }

    public boolean doIsExist(String collectionName, Serializable id){
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id)));
        return getCollection(collectionName).countDocuments(queryBasic) >= 1;
    }

    public List<Map<String,Object>> doGetByIds(String collectionName, Collection<Serializable> ids) {
        return Converter.convertDocumentToMap(getCollection(collectionName).find(checkIdType(ids),Map.class));
    }


    public <T> List<T> doList(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return getLambdaQueryResult(compareConditionList, orderList,projectionList,basicDBObjectList);
    }

    public <T> T doOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        List<T> result = getLambdaQueryResult(compareConditionList, null,projectionList,basicDBObjectList);
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : null;
    }

    public <T> T doLimitOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        List<T> result = getLambdaQueryResult(compareConditionList, null,projectionList,basicDBObjectList);
        return !result.isEmpty() ? result.get(0) : null;
    }

    public <T> PageResult<T> doPage(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        return getLambdaQueryResultPage(compareConditionList, orderList,projectionList, basicDBObjectList,new PageParam(pageNum, pageSize));
    }

    public <T> T doGetById(Serializable id) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id)));
        FindIterable<Document> iterable = getCollection().find(queryBasic);
        return (T) DocumentMapperConvert.mapDocument(iterable.first(),mongoEntity);
    }

    public boolean doIsExist(Serializable id){
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id)));
        return getCollection().countDocuments(queryBasic) >= 1;
    }

    public <T> List<T> doGetByIds(Collection<Serializable> ids) {
        FindIterable<Document> iterable = getCollection().find(checkIdType(ids));
        return (List<T>) DocumentMapperConvert.mapDocumentList(iterable, mongoEntity);
    }

    private BasicDBObject checkIdType(Collection<Serializable> ids) {
        List<Serializable> serializableList = ids.stream().filter(id -> ObjectId.isValid(String.valueOf(id))).collect(Collectors.toList());
        if (ids.size() == serializableList.size()){
            ids = ids.stream().map(id -> new ObjectId(String.valueOf(id))).collect(Collectors.toList());
        } else if (serializableList.isEmpty()){
            ids = ids.stream().map(String::valueOf).collect(Collectors.toList());
        }else {
            List<Serializable> idList = new ArrayList<>();
            ids.forEach(id -> {
                idList.add(ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : String.valueOf(id));
            });
            ids = idList;
        }
        return new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), ids));
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

    public <T> List<T> doAggregateList(List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject){
        MongoCollection<Document> collection = getCollection();
        AggregateIterable<Document> aggregateIterable = collection.aggregate(
                new ArrayList<BasicDBObject>(){{
                    aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
                    addAll(basicDBObjectList);
                }}
        );
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),mongoEntity);
    }

    public List<Map<String,Object>> doAggregateList(String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject){
        AggregateIterable<Map> aggregateIterable = getCollection(collectionName).aggregate(
                new ArrayList<BasicDBObject>(){{
                    aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
                    addAll(basicDBObjectList);
                }}, Map.class);
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return Converter.convertDocumentToMap(aggregateIterable.iterator());
    }

    public <E> List<E> doAggregateList(List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<E> clazz){
        AggregateIterable<Document> aggregateIterable = getCollection().aggregate(
                new ArrayList<BasicDBObject>(){{
                    aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
                    addAll(basicDBObjectList);
                }}
        );
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz != null ? clazz : mongoEntity);
    }

    public <E> List<E> doAggregateList(String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<E> clazz){
        AggregateIterable<Document> aggregateIterable = getCollection(collectionName).aggregate(
                new ArrayList<BasicDBObject>(){{
                    aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
                    addAll(basicDBObjectList);
                }}
        );
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz != null ? clazz : mongoEntity);
    }

    private void aggregateOptions(AggregateIterable<?> aggregateIterable,BasicDBObject optionsBasicDBObject){
        Set<String> keyedSet = optionsBasicDBObject.keySet();
        for (String key : keyedSet) {
            AggregateOptionsEnum aggregateOptionsEnum = AggregateOptionsEnum.getByOptions(key);
            switch (Objects.requireNonNull(aggregateOptionsEnum)){
                case ALLOW_DISK_USE:
                    aggregateIterable.allowDiskUse(optionsBasicDBObject.getBoolean(key));
                    break;
                case COLLATION:
                    aggregateIterable.collation((Collation) optionsBasicDBObject.get(key));
                    break;
                case BATCH_SIZE:
                    aggregateIterable.batchSize(optionsBasicDBObject.getInt(key));
                    break;
                case MAX_TIME_MS:
                    aggregateIterable.maxTime(optionsBasicDBObject.getLong(key), TimeUnit.MILLISECONDS);
                    break;
                case MAX_AWAIT_TIME_MS:
                    aggregateIterable.maxAwaitTime(optionsBasicDBObject.getLong(key),TimeUnit.MILLISECONDS);
                    break;
                case BYPASS_DOCUMENT_VALIDATION:
                    aggregateIterable.bypassDocumentValidation(optionsBasicDBObject.getBoolean(key));
                    break;
                case COMMENT:
                    aggregateIterable.comment((BsonValue) optionsBasicDBObject.get(key));
                    break;
                case COMMENT_STR:
                    aggregateIterable.comment(optionsBasicDBObject.getString(key));
                    break;
                case HINT:
                    aggregateIterable.hint((Bson) optionsBasicDBObject.get(key));
                    break;
                case LET:
                    aggregateIterable.let((Bson) optionsBasicDBObject.get(key));
                    break;
            }
        }
    }


    private <T> List<T> getLambdaQueryResult(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return (List<T>) DocumentMapperConvert.mapDocumentList(baseLambdaQuery(compareConditionList, orderList,projectionList,basicDBObjectList), mongoEntity);
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
        if (orderList != null && !orderList.isEmpty()) {
            orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        }
        MongoCollection<Document> collection = getCollection();
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(compareConditionList);
        if (null != basicDBObjectList && !basicDBObjectList.isEmpty()){
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
        if (orderList != null && !orderList.isEmpty()) {
            orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        }
        MongoCollection<Document> collection = getCollection(collectionName);
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(compareConditionList);
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        if (null != basicDBObjectList && !basicDBObjectList.isEmpty()){
            basicDBObjectList.forEach(basic -> {
                basicDBObject.putAll(basic.toMap());
            });
        }
        return collection.find(basicDBObject,Map.class).projection(BuildCondition.buildProjection(projectionList)).sort(sortCond);
    }

    private <T> PageResult<T> getLambdaQueryResultPage(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams) {
        PageResult<T> pageResult = new PageResult<>();
        FindIterable<Document> documentFindIterable = baseLambdaQuery(compareConditionList, orderList,projectionList,basicDBObjectList);
        long totalSize = doCount(compareConditionList);
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData((List<T>) DocumentMapperConvert.mapDocumentList(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize()), mongoEntity));
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

    private <T> MongoCollection<Document> getCollection(T entity) {
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

    private <T> Document processIdField(T entity){
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
        }
        return new Document(tableFieldMap);
    }

    private <T> List<Document> processIdFieldList(Collection<T> entityList){
        return entityList.stream().map(this::processIdField).collect(Collectors.toList());
    }

    public Map<String, MongoCollection<Document>> getCollectionMap() {
        return collectionMap;
    }

    public void setCollectionMap(Map<String, MongoCollection<Document>> collectionMap) {
        this.collectionMap = collectionMap;
    }

    public List<SlaveDataSource> getSlaveDataSources() {
        return slaveDataSources;
    }

    public void setSlaveDataSources(List<SlaveDataSource> slaveDataSources) {
        this.slaveDataSources = slaveDataSources;
    }

    public BaseProperty getBaseProperty() {
        return baseProperty;
    }

    public void setBaseProperty(BaseProperty baseProperty) {
        this.baseProperty = baseProperty;
    }

    public MongoClient getMongoClient() {
        return mongoClient;
    }

    public void setMongoClient(MongoClient mongoClient) {
        this.mongoClient = mongoClient;
    }

    public ConnectMongoDB getConnectMongoDB() {
        return connectMongoDB;
    }

    public void setConnectMongoDB(ConnectMongoDB connectMongoDB) {
        this.connectMongoDB = connectMongoDB;
    }

    public Class<?> getMongoEntity() {
        return mongoEntity;
    }

    public String getCreateIndex() {
        return createIndex;
    }

    public void setCreateIndex(String createIndex) {
        this.createIndex = createIndex;
    }

    public String getCollectionName() {
        return collectionName;
    }

    public void setCollectionName(String collectionName) {
        this.collectionName = collectionName;
    }
}
