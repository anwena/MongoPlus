package com.anwen.mongo.execute;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conn.ConnectMongoDB;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.convert.Converter;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.enums.AggregateOptionsEnum;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.enums.QueryOperatorEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.model.*;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.*;
import com.anwen.mongo.toolkit.codec.RegisterCodecUtil;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoException;
import com.mongodb.client.*;
import com.mongodb.client.model.*;
import com.mongodb.client.result.InsertOneResult;
import com.mongodb.client.result.UpdateResult;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static com.anwen.mongo.toolkit.BeanMapUtilByReflect.getIdField;

/**
 * @Description: sql执行
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.sql
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-16 20:35
 * @Version: 1.0
 */
@Deprecated
public class SqlExecute {

    private static final Logger logger = LoggerFactory.getLogger(SqlExecute.class);

    /**
     * 缓存mongoCollection
     * @author JiaChaoYang
     * @date 2023/12/28 10:58
    */
    private Map<String, MongoCollection<Document>> collectionMap = new ConcurrentHashMap<>();

    /**
     * 从数据源
     * @author JiaChaoYang
     * @date 2023/12/28 10:58
    */
    private List<SlaveDataSource> slaveDataSources;

    /**
     * 属性配置
     * @author JiaChaoYang
     * @date 2023/12/28 10:59
    */
    private BaseProperty baseProperty;

    /**
     * mongo客户端
     * @author JiaChaoYang
     * @date 2023/12/28 10:59
    */
    private MongoClient mongoClient;

    /**
     * 实例化 ConnectMongoDB 对象，用于保存连接
     * @author JiaChaoYang
     * @date 2023/12/28 10:59
    */
    private ConnectMongoDB connectMongoDB;

    /**
     * 集合名转换策略
     * @author JiaChaoYang
     * @date 2023/12/28 10:59
    */
    private CollectionNameConvert collectionNameConvert;

    private String createIndex = null;

    public void init(Class<?> clazz) {
        String tableName = clazz.getSimpleName().toLowerCase();
        String database = baseProperty.getDatabase();
        if (clazz.isAnnotationPresent(CollectionName.class)) {
            CollectionName annotation = clazz.getAnnotation(CollectionName.class);
            tableName = annotation.value();
            String dataSource = annotation.dataSource();
            if (StringUtils.isNotBlank(dataSource) && CollUtil.isNotEmpty(slaveDataSources)) {
                Optional<SlaveDataSource> matchingSlave = slaveDataSources.stream()
                        .filter(slave -> Objects.equals(dataSource, slave.getSlaveName()))
                        .findFirst();
                if (matchingSlave.isPresent()) {
                    SlaveDataSource slave = matchingSlave.get();
                    database = slave.getDatabase();
                } else {
                    throw new InitMongoCollectionException("No matching slave data source configured");
                }
            }
        }
        try {
            connectMongoDB = new ConnectMongoDB(mongoClient, database, tableName);
            MongoCollection<Document> collection = connectMongoDB.open();
            collectionMap.put(tableName, collection);
        } catch (MongoException e) {
            logger.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    public <T> Boolean doSave(T entity) {
        return doSave(MongoTransactionContext.getClientSessionContext(),entity);
    }

    public <T> Boolean doSave(ClientSession clientSession,T entity) {
        try {
            MongoCollection<Document> collection = getCollection(ClassTypeUtil.getClass(entity));
            Document document = processIdField(entity,false);
            InsertOneResult insertOneResult = Optional.ofNullable(clientSession)
                    .map(session -> collection.insertOne(session, document))
                    .orElseGet(() -> collection.insertOne(document));
            setBackIdValue(document, entity);
            return insertOneResult.wasAcknowledged();
        } catch (Exception e) {
            logger.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSave(String collectionName, Map<String, Object> entityMap) {
        return doSave(MongoTransactionContext.getClientSessionContext(),collectionName,entityMap);
    }

    public Boolean doSave(ClientSession clientSession,String collectionName, Map<String, Object> entityMap) {
        try {
            MongoCollection<Document> collection = getCollection(collectionName);
            Document document = DocumentUtil.handleMap(entityMap,true);
            return Optional.ofNullable(clientSession)
                    .map(session -> collection.insertOne(session, document))
                    .orElseGet(() -> collection.insertOne(document)).wasAcknowledged();
        } catch (Exception e) {
            logger.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public <T> Boolean doSaveBatch(Collection<T> entityList) {
        return doSaveBatch(MongoTransactionContext.getClientSessionContext(),entityList);
    }

    public <T> Boolean doSaveBatch(ClientSession clientSession,Collection<T> entityList) {
        try {
            List<Document> documentList = processIdFieldList(entityList);
            MongoCollection<Document> collection = getCollection(entityList.iterator().next().getClass());
            return Optional.ofNullable(clientSession)
                    .map(session -> collection.insertMany(session,documentList))
                    .orElseGet(() -> collection.insertMany(documentList)).getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            logger.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSaveBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        return doSaveBatch(MongoTransactionContext.getClientSessionContext(),collectionName,entityList);
    }

    public Boolean doSaveBatch(ClientSession clientSession,String collectionName, Collection<Map<String, Object>> entityList) {
        try {
            MongoCollection<Document> collection = getCollection(collectionName);
            List<Document> documentList = DocumentUtil.handleDocumentList(BeanMapUtilByReflect.mapListToDocumentList(entityList),true);
            return Optional.ofNullable(clientSession)
                    .map(session -> collection.insertMany(session,documentList))
                    .orElseGet(() -> collection.insertMany(documentList)).getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            logger.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public <T> Boolean doSaveOrUpdate(T entity) {
        return doSaveOrUpdate(MongoTransactionContext.getClientSessionContext(),entity);
    }

    public <T> Boolean doSaveOrUpdate(ClientSession clientSession,T entity) {
        String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
        if (StringUtils.isBlank(idByEntity)){
            return Optional.ofNullable(clientSession)
                    .map(session -> doSave(session,entity))
                    .orElseGet(() -> doSave(entity));
        }
        if (clientSession == null){
            return doIsExist(idByEntity,entity.getClass()) ? doUpdateById(entity) : doSave(entity);
        }
        return doIsExist(clientSession,idByEntity,entity.getClass()) ? doUpdateById(clientSession,entity) : doSave(clientSession,entity);
    }

    public Boolean doSaveOrUpdate(String collectionName, Map<String, Object> entityMap) {
        return doSaveOrUpdate(MongoTransactionContext.getClientSessionContext(),collectionName,entityMap);
    }

    public Boolean doSaveOrUpdate(ClientSession clientSession,String collectionName, Map<String, Object> entityMap) {
        String idValue = String.valueOf(entityMap.getOrDefault(SqlOperationConstant._ID,""));
        if (StringUtils.isBlank(idValue)) {
            return Optional.ofNullable(clientSession)
                    .map(session -> doSave(session,collectionName,entityMap))
                    .orElseGet(() -> doSave(collectionName,entityMap));
        }
        if (clientSession == null){
            return doIsExistMap(collectionName,idValue) ? doUpdateById(collectionName,entityMap) : doSave(collectionName,entityMap);
        }
        return doIsExistMap(clientSession,collectionName,idValue) ? doUpdateById(clientSession,collectionName,entityMap) : doSave(clientSession,collectionName,entityMap);
    }

    public <T> Boolean doSaveOrUpdateBatch(Collection<T> entityList) {
        return doSaveOrUpdateBatch(MongoTransactionContext.getClientSessionContext(),entityList);
    }

    public <T> Boolean doSaveOrUpdateBatch(ClientSession clientSession,Collection<T> entityList) {
        List<T> saveList = new ArrayList<>();
        List<T> updateList = new ArrayList<>();
        entityList.parallelStream().forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if ((StringUtils.isBlank(idByEntity) || !doIsExist(clientSession,idByEntity, entity.getClass()))) {
                saveList.add(entity);
            } else {
                updateList.addAll(entityList);
            }
        });
        boolean save = false;
        boolean update = false;
        if (!saveList.isEmpty()){
            save = clientSession != null ? doSaveBatch(clientSession,saveList) : doSaveBatch(saveList);
        }
        if (!updateList.isEmpty()){
            update = clientSession != null ? doUpdateBatchByIds(clientSession,updateList) : doUpdateBatchByIds(updateList);
        }
        return save == update;
    }

    public Boolean doSaveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        return doSaveOrUpdateBatch(MongoTransactionContext.getClientSessionContext(),collectionName,entityList);
    }

    public Boolean doSaveOrUpdateBatch(ClientSession clientSession,String collectionName, Collection<Map<String, Object>> entityList) {
        List<Map<String,Object>> saveList = new ArrayList<>();
        List<Map<String,Object>> updateList = new ArrayList<>();
        entityList.parallelStream().forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if ((StringUtils.isBlank(idByEntity) || !doIsExistMap(clientSession,collectionName,idByEntity))) {
                saveList.add(entity);
            } else {
                updateList.addAll(entityList);
            }
        });
        boolean save = false;
        boolean update = false;
        if (!saveList.isEmpty()){
            save = clientSession != null ? doSaveBatch(clientSession,collectionName,saveList) : doSaveBatch(saveList);
        }
        if (!updateList.isEmpty()){
            update = clientSession != null ? doUpdateBatchByIds(clientSession,collectionName,updateList) : doUpdateBatchByIds(updateList);
        }
        return save == update;
    }


    public <T> Boolean doUpdateById(T entity) {
        return doUpdateById(MongoTransactionContext.getClientSessionContext(),entity);
    }

    public <T> Boolean doUpdateById(ClientSession clientSession,T entity) {
        Document document = DocumentUtil.checkUpdateField(entity,false);
        BasicDBObject filter = getFilter(document);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        MongoCollection<Document> collection = getCollection(ClassTypeUtil.getClass(entity));
        return Optional.ofNullable(clientSession)
                .map(session -> collection.updateOne(session,filter,update))
                .orElseGet(() -> collection.updateOne(filter,update)).getModifiedCount() >= 1;
    }

    public Boolean doUpdateById(String collectionName, Map<String, Object> entityMap) {
        return doUpdateById(MongoTransactionContext.getClientSessionContext(),collectionName,entityMap);
    }

    public Boolean doUpdateById(ClientSession clientSession,String collectionName, Map<String, Object> entityMap) {
        BasicDBObject filter = getFilter(entityMap);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), DocumentUtil.handleMap(entityMap,false));
        MongoCollection<Document> collection = getCollection(collectionName,entityMap);
        return Optional.ofNullable(clientSession)
                .map(session -> collection.updateOne(session,filter,update))
                .orElseGet(() -> collection.updateOne(filter,update)).getModifiedCount() >= 1;
    }

    private BasicDBObject getFilter(Map<String, Object> entityMap) {
        if (!entityMap.containsKey(SqlOperationConstant._ID)) {
            throw new MongoException("_id undefined");
        }
        Object _idValue = entityMap.get(SqlOperationConstant._ID);
        BasicDBObject filter = new BasicDBObject(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(_idValue)) ? new ObjectId(String.valueOf(entityMap.get(SqlOperationConstant._ID))) : _idValue);
        entityMap.remove(SqlOperationConstant._ID);
        return filter;
    }

    public <T> Boolean doUpdateBatchByIds(Collection<T> entityList) {
        return doUpdateBatchByIds(MongoTransactionContext.getClientSessionContext(),entityList);
    }

    public <T> Boolean doUpdateBatchByIds(ClientSession clientSession,Collection<T> entityList) {
        int line = 0;
        for (T entity : entityList) {
            line += Optional.ofNullable(clientSession).map(session -> doUpdateById(clientSession,entity)).orElseGet(() -> doUpdateById(entity)) ? 1 : 0;
        }
        return line == entityList.size();
    }

    public Boolean doUpdateBatchByIds(String collectionName, Collection<Map<String, Object>> entityList) {
        return doUpdateBatchByIds(MongoTransactionContext.getClientSessionContext(),collectionName,entityList);
    }

    public Boolean doUpdateBatchByIds(ClientSession clientSession,String collectionName, Collection<Map<String, Object>> entityList) {
        int line = 0;
        for (Map<String,Object> entity : entityList) {
            line += Optional.ofNullable(clientSession).map(session -> doUpdateById(clientSession,collectionName,entity)).orElseGet(() -> doUpdateById(collectionName,entity)) ? 1 : 0;
        }
        return line == entityList.size();
    }

    public <T> Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        return doUpdateByColumn(MongoTransactionContext.getClientSessionContext(),entity,column);
    }

    public <T> Boolean doUpdateByColumn(ClientSession clientSession,T entity, SFunction<T, Object> column) {
        return doUpdateByColumn(clientSession,entity,column.getFieldNameLine());
    }


    public <T> Boolean doUpdateByColumn(T entity, String column) {
        return doUpdateByColumn(MongoTransactionContext.getClientSessionContext(),entity,column);
    }

    public <T> Boolean doUpdateByColumn(ClientSession clientSession,T entity, String column) {
        Object filterValue = ClassTypeUtil.getClassFieldValue(entity,column);
        String valueOf = String.valueOf(filterValue);
        Bson filter = Filters.eq(column, ObjectId.isValid(valueOf) ? new ObjectId(valueOf) : filterValue);
        Document document = DocumentUtil.checkUpdateField(entity,false);
        MongoCollection<Document> collection = getCollection(ClassTypeUtil.getClass(entity));
        return Optional.ofNullable(clientSession).map(session -> collection.updateMany(session,filter,document)).orElseGet(() -> collection.updateMany(filter,document)).getModifiedCount() >= 1;
    }

    public Boolean doUpdateByColumn(String collectionName,Map<String,Object> entityMap, String column) {
        return doUpdateByColumn(MongoTransactionContext.getClientSessionContext(),collectionName,entityMap,column);
    }

    public Boolean doUpdateByColumn(ClientSession clientSession,String collectionName,Map<String,Object> entityMap, String column) {
        if (!entityMap.containsKey(column)){
            throw new MongoException(column+" undefined");
        }
        Object columnValue = entityMap.get(column);
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(columnValue)) ? new ObjectId(String.valueOf(columnValue)) : columnValue);
        Document document = DocumentUtil.handleMap(entityMap,false);
        MongoCollection<Document> collection = getCollection(collectionName);
        return Optional.ofNullable(clientSession).map(session -> collection.updateMany(session,filter,document)).orElseGet(() -> collection.updateMany(filter,document)).getModifiedCount() >= 1;
    }


    public Boolean doRemoveById(Serializable id,Class<?> clazz) {
        return doRemoveById(MongoTransactionContext.getClientSessionContext(),id,clazz);
    }

    public Boolean doRemoveById(ClientSession clientSession,Serializable id,Class<?> clazz) {
        return executeRemove(clientSession, id, getCollection(clazz));
    }

    public Boolean doRemoveById(String collectionName,Serializable id) {
        return doRemoveById(MongoTransactionContext.getClientSessionContext(),collectionName,id);
    }

    public Boolean doRemoveById(ClientSession clientSession,String collectionName,Serializable id) {
        return executeRemove(clientSession, id, getCollection(collectionName));
    }

    private Boolean executeRemove(ClientSession clientSession, Serializable id, MongoCollection<Document> collection) {
        Bson filterId = Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id);
        return Optional.ofNullable(clientSession).map(session -> collection.deleteOne(session,filterId)).orElseGet(() -> collection.deleteOne(filterId)).getDeletedCount() >= 1;
    }

    public <T> Boolean doRemoveByColumn(SFunction<T, Object> column, Object value,Class<T> clazz) {
        return doRemoveByColumn(column.getFieldNameLine(),value,clazz);
    }

    public <T> Boolean doRemoveByColumn(ClientSession clientSession,SFunction<T, Object> column, Object value,Class<T> clazz) {
        return doRemoveByColumn(clientSession,column.getFieldNameLine(),value,clazz);
    }

    public Boolean doRemoveByColumn(String column, Object value,Class<?> clazz) {
        return doRemoveByColumn(MongoTransactionContext.getClientSessionContext(),column,value,clazz);
    }

    public Boolean doRemoveByColumn(ClientSession clientSession,String column, Object value,Class<?> clazz) {
        return executeRemoveByColumn(clientSession,column,value,getCollection(clazz));
    }

    public Boolean doRemoveByColumn(String collectionName,String column, Object value) {
        return doRemoveByColumn(MongoTransactionContext.getClientSessionContext(),collectionName,column,value);
    }

    public Boolean doRemoveByColumn(ClientSession clientSession,String collectionName,String column, Object value) {
        return executeRemoveByColumn(clientSession,column,value,getCollection(collectionName));
    }

    public Boolean executeRemoveByColumn(ClientSession clientSession,String column,Object value,MongoCollection<Document> collection){
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return Optional.ofNullable(clientSession).map(session -> collection.deleteMany(session,filter)).orElseGet(() -> collection.deleteMany(filter)).getDeletedCount() >= 1;
    }


    public Boolean doRemoveBatchByIds(Collection<? extends Serializable> idList,Class<?> clazz) {
        return doRemoveBatchByIds(MongoTransactionContext.getClientSessionContext(),idList,clazz);
    }

    public Boolean doRemoveBatchByIds(ClientSession clientSession,Collection<? extends Serializable> idList,Class<?> clazz) {
        return executeRemoveBatchByIds(clientSession,idList,getCollection(clazz));
    }

    public Boolean doRemoveBatchByIds(String collectionName,Collection<? extends Serializable> idList) {
        return doRemoveBatchByIds(MongoTransactionContext.getClientSessionContext(),collectionName,idList);
    }

    public Boolean doRemoveBatchByIds(ClientSession clientSession,String collectionName,Collection<? extends Serializable> idList) {
        return executeRemoveBatchByIds(clientSession,idList,getCollection(collectionName));
    }

    public Boolean executeRemoveBatchByIds(ClientSession clientSession,Collection<? extends Serializable> idList,MongoCollection<Document> collection){
        List<Serializable> convertedIds = idList.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        Bson objectIdBson = Filters.in(SqlOperationConstant._ID, convertedIds);
        return Optional.ofNullable(clientSession)
                .map(session -> collection.deleteMany(session,objectIdBson))
                .orElseGet(() -> collection.deleteMany(objectIdBson)).getDeletedCount() >= 1;
    }

    public <T> List<T> doList(Class<T> clazz) {
        return doList(MongoTransactionContext.getClientSessionContext(),clazz);
    }

    public <T> List<T> doList(ClientSession clientSession, Class<T> clazz) {
        MongoCollection<Document> collection = getCollection(clazz);
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return DocumentMapperConvert.mapDocumentList(Optional.ofNullable(clientSession).map(collection::find).orElseGet(collection::find),clazz);
    }

    public List<Map<String, Object>> doList(String collectionName) {
        return doList(MongoTransactionContext.getClientSessionContext(),collectionName);
    }

    public List<Map<String, Object>> doList(ClientSession clientSession,String collectionName) {
        MongoCollection<Document> collection = getCollection(collectionName);
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return Converter.convertDocumentToMap(Optional.ofNullable(clientSession).map(session -> collection.find(session,Map.class)).orElseGet(() -> collection.find(Map.class)), Math.toIntExact(doCount(collectionName)));
    }

    public List<Map<String, Object>> doList(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return doList(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList,orderList,projectionList,basicDBObjectList);
    }

    public List<Map<String, Object>> doList(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return getLambdaQueryResult(clientSession,collectionName, compareConditionList, orderList,projectionList,basicDBObjectList);
    }

    public PageResult<Map<String, Object>> doPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        return doPage(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList,orderList,projectionList,basicDBObjectList,pageNum,pageSize);
    }

    public PageResult<Map<String, Object>> doPage(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize) {
        return getLambdaQueryResultPage(clientSession,collectionName, compareConditionList, orderList,projectionList, basicDBObjectList,new PageParam(pageNum, pageSize));
    }

    public PageResult<Map<String,Object>> doPage(String collectionName,Integer pageNum,Integer pageSize){
        return doPage(MongoTransactionContext.getClientSessionContext(),collectionName,pageNum,pageSize);
    }

    public PageResult<Map<String,Object>> doPage(ClientSession clientSession,String collectionName,Integer pageNum,Integer pageSize){
        return getLambdaQueryResultPage(clientSession,collectionName,null,null,null,null,new PageParam(pageNum,pageSize));
    }

    public Map<String, Object> doOne(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return doOne(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList,projectionList,basicDBObjectList);
    }

    public Map<String, Object> doOne(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        List<Map<String, Object>> result = getLambdaQueryResult(clientSession,collectionName, compareConditionList, null,projectionList,basicDBObjectList);
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : new HashMap<>();
    }

    public Map<String, Object> doLimitOne(String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList) {
        return doLimitOne(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList,projectionList,basicDBObjectList,orderList);
    }

    public Map<String, Object> doLimitOne(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList) {
        List<Map<String, Object>> result = getLambdaQueryResult(clientSession,collectionName, compareConditionList, orderList,projectionList,basicDBObjectList);
        return !result.isEmpty() ? result.get(0) : new HashMap<>();
    }

    public Map<String, Object> doGetById(String collectionName, Serializable id) {
        return doGetById(null,collectionName,id);
    }

    public Map<String, Object> doGetById(ClientSession clientSession,String collectionName, Serializable id) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        MongoCollection<Document> collection = getCollection(collectionName);
        return Optional.ofNullable(clientSession).map(session -> collection.find(session,queryBasic)).orElseGet(() -> collection.find(queryBasic)).first();
    }

    public List<Map<String,Object>> doGetByIds(String collectionName, Collection<? extends Serializable> ids) {
        return doGetByIds(null,collectionName,ids);
    }

    public List<Map<String,Object>> doGetByIds(ClientSession clientSession,String collectionName, Collection<? extends Serializable> ids) {
        BasicDBObject basicDBObject = checkIdType(ids);
        MongoCollection<Document> collection = getCollection(collectionName);
        return Converter.convertDocumentToMap(Optional.ofNullable(clientSession).map(session -> collection.find(session,basicDBObject,Map.class)).orElseGet(() -> collection.find(basicDBObject,Map.class)));
    }

    public <T> List<T> doList(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz) {
        return doList(MongoTransactionContext.getClientSessionContext(),compareConditionList,orderList,projectionList,basicDBObjectList,clazz);
    }

    public <T> List<T> doList(ClientSession clientSession,List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz) {
        return getLambdaQueryResult(clientSession,compareConditionList, orderList,projectionList,basicDBObjectList,clazz);
    }

    public <T> T doOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz) {
        return doOne(MongoTransactionContext.getClientSessionContext(),compareConditionList,projectionList,basicDBObjectList,clazz);
    }

    public <T> T doOne(ClientSession clientSession,List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz) {
        List<T> result = getLambdaQueryResult(clientSession,compareConditionList, null,projectionList,basicDBObjectList,clazz);
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : null;
    }


    public <T> T doLimitOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList,Class<T> clazz) {
        return doLimitOne(MongoTransactionContext.getClientSessionContext(),compareConditionList,projectionList,basicDBObjectList,orderList,clazz);
    }

    public <T> T doLimitOne(ClientSession clientSession,List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList,Class<T> clazz) {
        List<T> result = getLambdaQueryResult(clientSession,compareConditionList, orderList,projectionList,basicDBObjectList,clazz);
        return !result.isEmpty() ? result.get(0) : null;
    }

    public <T> PageResult<T> doPage(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize,Class<T> clazz) {
        return doPage(MongoTransactionContext.getClientSessionContext(),compareConditionList,orderList,projectionList,basicDBObjectList,pageNum,pageSize,clazz);
    }

    public <T> PageResult<T> doPage(ClientSession clientSession,List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize,Class<T> clazz) {
        return getLambdaQueryResultPage(clientSession,compareConditionList, orderList,projectionList, basicDBObjectList,new PageParam(pageNum, pageSize),clazz);
    }

    public <T> T doGetById(Serializable id,Class<T> clazz) {
        return doGetById(MongoTransactionContext.getClientSessionContext(),id,clazz);
    }

    public <T> T doGetById(ClientSession clientSession,Serializable id,Class<T> clazz) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        MongoCollection<Document> collection = getCollection(clazz);
        FindIterable<Document> iterable = Optional.ofNullable(clientSession).map(session -> collection.find(session,queryBasic)).orElseGet(() -> collection.find(queryBasic));
        return DocumentMapperConvert.mapDocument(iterable.first(),clazz);
    }

    public boolean doIsExist(Serializable id,Class<?> clazz){
        return doIsExist(MongoTransactionContext.getClientSessionContext(),id,clazz);
    }

    public boolean doIsExist(ClientSession clientSession,Serializable id,Class<?> clazz){
        return executeExist(clientSession, id, getCollection(clazz));
    }

    public boolean doIsExistMap(String collectionName, Serializable id){
        return doIsExistMap(MongoTransactionContext.getClientSessionContext(),collectionName,id);
    }

    public boolean doIsExistMap(ClientSession clientSession,String collectionName, Serializable id){
        return executeExist(clientSession, id, getCollection(collectionName));
    }

    private boolean executeExist(ClientSession clientSession, Serializable id, MongoCollection<Document> collection) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return Optional.ofNullable(clientSession).map(session -> collection.countDocuments(session,queryBasic)).orElseGet(() -> collection.countDocuments(queryBasic)) >= 1;
    }

    public <T> List<T> doGetByIds(Collection<? extends Serializable> ids,Class<T> clazz) {
        return doGetByIds(MongoTransactionContext.getClientSessionContext(),ids,clazz);
    }

    public <T> List<T> doGetByIds(ClientSession clientSession,Collection<? extends Serializable> ids,Class<T> clazz) {
        MongoCollection<Document> collection = getCollection(clazz);
        BasicDBObject basicDBObject = checkIdType(ids);
        return DocumentMapperConvert.mapDocumentList(Optional.ofNullable(clientSession).map(session -> collection.find(session,basicDBObject)).orElseGet(() -> collection.find(basicDBObject)), clazz);
    }

    public <T> List<T> doGetByColumn(String column,Object value,Class<T> clazz){
        return doGetByColumn(null,column,value,clazz);
    }

    public <T> List<T> doGetByColumn(ClientSession clientSession,String column,Object value,Class<T> clazz){
        MongoCollection<Document> collection = getCollection(clazz);
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return DocumentMapperConvert.mapDocumentList(Optional.ofNullable(clientSession).map(session -> collection.find(session,filter)).orElseGet(() -> collection.find(filter)),clazz);
    }

    public List<Map<String,Object>> doGetByColumn(String collectionName,String column,Object value){
        return doGetByColumn(null,collectionName,column,value);
    }

    public List<Map<String,Object>> doGetByColumn(ClientSession clientSession,String collectionName,String column,Object value){
        MongoCollection<Document> collection = getCollection(collectionName);
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return Converter.convertDocumentToMap(Optional.ofNullable(clientSession).map(session -> collection.find(session,filter,Map.class)).orElseGet(() -> collection.find(filter,Map.class)));
    }

    public <T> List<T> doSql(String sql,Class<T> clazz){
        return doSql(null,sql,clazz);
    }

    public <T> List<T> doSql(ClientSession clientSession,String sql,Class<T> clazz){
        MongoCollection<Document> collection = getCollection(clazz);
        BasicDBObject basicDBObject = BasicDBObject.parse(sql);
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return DocumentMapperConvert.mapDocumentList(Optional.ofNullable(clientSession).map(session -> collection.find(session,basicDBObject)).orElseGet(() -> collection.find(basicDBObject)),clazz);
    }

    public List<Map<String,Object>> doSql(String collectionName,String sql){
        return doSql(null,collectionName,sql);
    }

    public List<Map<String,Object>> doSql(ClientSession clientSession,String collectionName,String sql){
        MongoCollection<Document> collection = getCollection(collectionName);
        BasicDBObject basicDBObject = BasicDBObject.parse(sql);
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return Converter.convertDocumentToMap(Optional.ofNullable(clientSession).map(session -> collection.find(session,basicDBObject,Map.class)).orElseGet(() -> collection.find(basicDBObject,Map.class)));
    }

    private BasicDBObject checkIdType(Collection<? extends Serializable> ids) {
        List<Serializable> convertedIds = ids.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        return new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), convertedIds));
    }

    public Boolean doUpdate(List<CompareCondition> compareConditionList,Class<?> clazz) {
        return doUpdate(MongoTransactionContext.getClientSessionContext(),compareConditionList,clazz);
    }

    public Boolean doUpdate(ClientSession clientSession,List<CompareCondition> compareConditionList,Class<?> clazz) {
        return executeUpdate(clientSession,compareConditionList,getCollection(clazz));
    }

    public Boolean doUpdate(String collectionName,List<CompareCondition> compareConditionList){
        return doUpdate(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList);
    }

    public Boolean doUpdate(ClientSession clientSession,String collectionName,List<CompareCondition> compareConditionList){
        return executeUpdate(clientSession,compareConditionList,getCollection(collectionName));
    }

    public Boolean executeUpdate(ClientSession clientSession,List<CompareCondition> compareConditionList,MongoCollection<Document> collection){
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        BasicDBObject updateBasic = BuildCondition.buildUpdateValue(compareConditionList);
        BasicDBObject basicDBObject = new BasicDBObject() {{
            append(SpecialConditionEnum.SET.getCondition(), updateBasic);
        }};
        UpdateResult updateResult = Optional.ofNullable(clientSession).map(session -> collection.updateMany(session,queryBasic,basicDBObject)).orElseGet(() -> collection.updateMany(queryBasic,basicDBObject));
        return updateResult.getModifiedCount() >= 1;
    }

    public Boolean doRemove(List<CompareCondition> compareConditionList,Class<?> clazz) {
        return doRemove(MongoTransactionContext.getClientSessionContext(),compareConditionList,clazz);
    }

    public Boolean doRemove(ClientSession clientSession,List<CompareCondition> compareConditionList,Class<?> clazz) {
        return executeRemove(clientSession,compareConditionList,getCollection(clazz));
    }

    public Boolean doRemove(String collectionName,List<CompareCondition> compareConditionList){
        return doRemove(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList);
    }

    public Boolean doRemove(ClientSession clientSession,String collectionName,List<CompareCondition> compareConditionList){
        return executeRemove(clientSession,compareConditionList,getCollection(collectionName));
    }

    public Boolean executeRemove(ClientSession clientSession,List<CompareCondition> compareConditionList,MongoCollection<Document> collection){
        BasicDBObject deleteBasic = BuildCondition.buildQueryCondition(compareConditionList);
        return Optional.ofNullable(clientSession).map(session -> collection.deleteMany(session,deleteBasic)).orElseGet(() -> collection.deleteMany(deleteBasic)).getDeletedCount() >= 1;
    }

    public long doCount(String collectionName,List<CompareCondition> compareConditionList){
        return doCount(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList);
    }

    public long doCount(ClientSession clientSession,String collectionName,List<CompareCondition> compareConditionList){
        return executeCountByCondition(clientSession,compareConditionList,getCollection(collectionName));
    }

    public long doCount(List<CompareCondition> compareConditionList,Class<?> clazz){
        return doCount(MongoTransactionContext.getClientSessionContext(),compareConditionList,clazz);
    }

    public long doCount(ClientSession clientSession,List<CompareCondition> compareConditionList,Class<?> clazz){
        return executeCountByCondition(clientSession,compareConditionList,getCollection(clazz));
    }

    public long executeCountByCondition(ClientSession clientSession,List<CompareCondition> compareConditionList,MongoCollection<Document> collection){
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(compareConditionList);
        return Optional.ofNullable(clientSession).map(session -> collection.countDocuments(session,basicDBObject)).orElseGet(() -> collection.countDocuments(basicDBObject));
    }

    public long doCount(String collectionName){
        return doCount(MongoTransactionContext.getClientSessionContext(),collectionName);
    }

    public long doCount(ClientSession clientSession,String collectionName){
        return executeCount(clientSession,getCollection(collectionName));
    }

    public long doCount(Class<?> clazz){
        return executeCount(MongoTransactionContext.getClientSessionContext(),getCollection(clazz));
    }

    public long doCount(ClientSession clientSession,Class<?> clazz){
        return executeCount(clientSession,getCollection(clazz));
    }

    public long executeCount(ClientSession clientSession,MongoCollection<Document> collection){
        return Optional.ofNullable(clientSession).map(collection::countDocuments).orElseGet(collection::countDocuments);
    }

    public String createIndex(Bson bson,MongoCollection<Document> collection){
        return createIndex(MongoTransactionContext.getClientSessionContext(),bson,collection);
    }

    public String createIndex(ClientSession clientSession,Bson bson,MongoCollection<Document> collection){
        return Optional.ofNullable(clientSession).map(session -> collection.createIndex(clientSession,bson)).orElseGet(() -> collection.createIndex(bson));
    }

    public String createIndex(Bson bson,IndexOptions indexOptions,MongoCollection<Document> collection){
        return createIndex(MongoTransactionContext.getClientSessionContext(),bson,indexOptions,collection);
    }

    public String createIndex(ClientSession clientSession,Bson bson,IndexOptions indexOptions,MongoCollection<Document> collection){
        return Optional.ofNullable(clientSession).map(session -> collection.createIndex(clientSession,bson,indexOptions)).orElseGet(() -> collection.createIndex(bson,indexOptions));
    }

    public List<String> createIndexes(List<IndexModel> indexes,MongoCollection<Document> collection){
        return createIndexes(MongoTransactionContext.getClientSessionContext(),indexes,collection);
    }

    public List<String> createIndexes(ClientSession clientSession,List<IndexModel> indexes,MongoCollection<Document> collection){
        return Optional.ofNullable(clientSession).map(session -> collection.createIndexes(clientSession,indexes)).orElseGet(() -> collection.createIndexes(indexes));
    }

    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions,MongoCollection<Document> collection){
        return createIndexes(MongoTransactionContext.getClientSessionContext(),indexes,createIndexOptions,collection);
    }

    public List<String> createIndexes(ClientSession clientSession, List<IndexModel> indexes, CreateIndexOptions createIndexOptions,MongoCollection<Document> collection){
        return Optional.ofNullable(clientSession).map(session -> collection.createIndexes(clientSession,indexes,createIndexOptions)).orElseGet(() -> collection.createIndexes(indexes,createIndexOptions));
    }

    public List<Document> listIndexes(MongoCollection<Document> collection){
        return listIndexes(MongoTransactionContext.getClientSessionContext(),collection);
    }

    public List<Document> listIndexes(ClientSession clientSession,MongoCollection<Document> collection){
        return DocumentMapperConvert.indexesIterableToDocument(Optional.ofNullable(clientSession).map(collection::listIndexes).orElseGet(collection::listIndexes));
    }

    public void dropIndex(String indexName,MongoCollection<Document> collection){
        dropIndex(MongoTransactionContext.getClientSessionContext(),indexName,collection);
    }

    public void dropIndex(ClientSession clientSession,String indexName,MongoCollection<Document> collection){
        ifPresentOrElse(clientSession,session -> collection.dropIndex(session,indexName),() -> collection.dropIndex(indexName));
    }

    public void dropIndex(String indexName,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection){
        dropIndex(MongoTransactionContext.getClientSessionContext(),indexName,dropIndexOptions,collection);
    }

    public void dropIndex(ClientSession clientSession,String indexName,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection){
        ifPresentOrElse(clientSession,session -> collection.dropIndex(session,indexName,dropIndexOptions),() -> collection.dropIndex(indexName,dropIndexOptions));
    }

    public void dropIndex(Bson keys,MongoCollection<Document> collection){
        dropIndex(MongoTransactionContext.getClientSessionContext(),keys,collection);
    }

    public void dropIndex(ClientSession clientSession,Bson keys,MongoCollection<Document> collection){
        ifPresentOrElse(clientSession,session -> collection.dropIndex(session,keys),() -> collection.dropIndex(keys));
    }

    public void dropIndex(Bson keys,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection){
        dropIndex(MongoTransactionContext.getClientSessionContext(),keys,dropIndexOptions,collection);
    }

    public void dropIndex(ClientSession clientSession,Bson keys,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection){
        ifPresentOrElse(clientSession,session -> collection.dropIndex(session,keys,dropIndexOptions),() -> collection.dropIndex(keys,dropIndexOptions));
    }

    public void dropIndexes(MongoCollection<Document> collection){
        dropIndexes(MongoTransactionContext.getClientSessionContext(),collection);
    }

    public void dropIndexes(ClientSession clientSession,MongoCollection<Document> collection){
        ifPresentOrElse(clientSession, collection::dropIndexes, collection::dropIndexes);
    }

    public void dropIndexes(DropIndexOptions dropIndexOptions,MongoCollection<Document> collection){
        dropIndexes(MongoTransactionContext.getClientSessionContext(),dropIndexOptions,collection);
    }

    public void dropIndexes(ClientSession clientSession,DropIndexOptions dropIndexOptions,MongoCollection<Document> collection){
        ifPresentOrElse(clientSession,session -> collection.dropIndexes(session,dropIndexOptions),() -> collection.dropIndexes(dropIndexOptions));
    }

    public <T> List<T> doAggregateList(List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<T> clazz){
        return doAggregateList((ClientSession) null,aggregateList,basicDBObjectList,optionsBasicDBObject,clazz);
    }

    public <T> List<T> doAggregateList(ClientSession clientSession,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<T> clazz){
        MongoCollection<Document> collection = getCollection(clazz);
        List<BasicDBObject> aggregateConditionList = new ArrayList<BasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
            addAll(basicDBObjectList);
        }};
        AggregateIterable<Document> aggregateIterable = Optional.ofNullable(clientSession).map(session -> collection.aggregate(session,aggregateConditionList)).orElseGet(() -> collection.aggregate(aggregateConditionList));
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz);
    }

    public List<Map<String,Object>> doAggregateList(String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject){
        return doAggregateList(MongoTransactionContext.getClientSessionContext(),collectionName,aggregateList,basicDBObjectList,optionsBasicDBObject);
    }

    public List<Map<String,Object>> doAggregateList(ClientSession clientSession,String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject){
        MongoCollection<Document> collection = getCollection(collectionName);
        List<BasicDBObject> aggregateConditionList = new ArrayList<BasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
            addAll(basicDBObjectList);
        }};
        AggregateIterable<Map> aggregateIterable = Optional.ofNullable(clientSession).map(session -> collection.aggregate(session,aggregateConditionList,Map.class)).orElseGet(() -> collection.aggregate(aggregateConditionList,Map.class));
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return Converter.convertDocumentToMap(aggregateIterable.iterator());
    }

    public <E> List<E> doAggregateList(String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<E> clazz){
        return doAggregateList(MongoTransactionContext.getClientSessionContext(),collectionName,aggregateList,basicDBObjectList,optionsBasicDBObject,clazz);
    }

    public <E> List<E> doAggregateList(ClientSession clientSession,String collectionName,List<BaseAggregate> aggregateList,List<BasicDBObject> basicDBObjectList,BasicDBObject optionsBasicDBObject,Class<E> clazz){
        MongoCollection<Document> collection = getCollection(collectionName);
        List<BasicDBObject> aggregateConditionList = new ArrayList<BasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new BasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate())));
            addAll(basicDBObjectList);
        }};
        AggregateIterable<Document> aggregateIterable = Optional.ofNullable(clientSession).map(session -> collection.aggregate(session,aggregateConditionList)).orElseGet(() -> collection.aggregate(aggregateConditionList));
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz);
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
                    aggregateIterable.comment(String.valueOf(optionsBasicDBObject.get(key)));
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


    private <T> List<T> getLambdaQueryResult(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz) {
        return getLambdaQueryResult(MongoTransactionContext.getClientSessionContext(),compareConditionList,orderList,projectionList,basicDBObjectList,clazz);
    }

    private <T> List<T> getLambdaQueryResult(ClientSession clientSession,List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz) {
        return DocumentMapperConvert.mapDocumentList(baseLambdaQuery(clientSession,compareConditionList, orderList,projectionList,basicDBObjectList,clazz), clazz);
    }

    private List<Map<String, Object>> getLambdaQueryResult(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return getLambdaQueryResult(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList,orderList,projectionList,basicDBObjectList);
    }

    private List<Map<String, Object>> getLambdaQueryResult(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return Converter.convertDocumentToMap(baseLambdaQuery(clientSession,collectionName, compareConditionList, orderList,projectionList,basicDBObjectList), Math.toIntExact(doCount(collectionName, compareConditionList)));
    }

    /**
     * 查询执行
     *
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:51
     */
    private FindIterable<Document> baseLambdaQuery(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<?> clazz) {
        return baseLambdaQuery(MongoTransactionContext.getClientSessionContext(),compareConditionList,orderList,projectionList,basicDBObjectList,clazz);
    }

    private FindIterable<Document> baseLambdaQuery(ClientSession clientSession,List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<?> clazz) {
        BasicDBObject sortCond = new BasicDBObject();
        if (orderList != null && !orderList.isEmpty()) {
            orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        }
        MongoCollection<Document> collection = getCollection(clazz);
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(compareConditionList);
        if (null != basicDBObjectList && !basicDBObjectList.isEmpty()){
            basicDBObjectList.forEach(basic -> basicDBObject.putAll(basic.toMap()));
        }
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperatorEnum.TEXT.getValue()));
        }
        return Optional.ofNullable(clientSession).map(session -> collection.find(session,basicDBObject)).orElseGet(() -> collection.find(basicDBObject)).projection(BuildCondition.buildProjection(projectionList)).sort(sortCond);
    }

    private FindIterable<Map> baseLambdaQuery(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
        return baseLambdaQuery(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList,orderList,projectionList,basicDBObjectList);
    }

    private FindIterable<Map> baseLambdaQuery(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList) {
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
        return Optional.ofNullable(clientSession).map(session -> collection.find(session,basicDBObject,Map.class)).orElseGet(() -> collection.find(basicDBObject,Map.class)).projection(BuildCondition.buildProjection(projectionList)).sort(sortCond);
    }

    private <T> PageResult<T> getLambdaQueryResultPage(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams,Class<T> clazz) {
        return getLambdaQueryResultPage(MongoTransactionContext.getClientSessionContext(),compareConditionList,orderList,projectionList,basicDBObjectList,pageParams,clazz);
    }

    private <T> PageResult<T> getLambdaQueryResultPage(ClientSession clientSession,List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams,Class<T> clazz) {
        PageResult<T> pageResult = new PageResult<>();
        FindIterable<Document> documentFindIterable = baseLambdaQuery(clientSession,compareConditionList, orderList,projectionList,basicDBObjectList,clazz);
        long totalSize = doCount(compareConditionList,clazz);
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData(DocumentMapperConvert.mapDocumentList(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize()), clazz));
        return pageResult;
    }

    private PageResult<Map<String, Object>> getLambdaQueryResultPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams) {
        return getLambdaQueryResultPage(MongoTransactionContext.getClientSessionContext(),collectionName,compareConditionList,orderList,projectionList,basicDBObjectList,pageParams);
    }

    private PageResult<Map<String, Object>> getLambdaQueryResultPage(ClientSession clientSession,String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, PageParam pageParams) {
        PageResult<Map<String, Object>> pageResult = new PageResult<>();
        FindIterable<Map> documentFindIterable = baseLambdaQuery(clientSession,collectionName, compareConditionList, orderList,projectionList,basicDBObjectList);
        long totalSize = doCount(collectionName,compareConditionList);
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData(Converter.convertDocumentToMap(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize())));
        return pageResult;
    }

    private <T> MongoCollection<Document> getCollection(T entity) {
        return getCollection(ClassTypeUtil.getClass(entity)).withCodecRegistry(RegisterCodecUtil.registerCodec(entity));
    }

    private MongoCollection<Document> getCollection(String collectionName,Map<?,?> map){
        return getCollection(collectionName).withCodecRegistry(RegisterCodecUtil.registerCodec(map));
    }

    public MongoCollection<Document> getCollection(Class<?> clazz) {
        createIndex = null;
        String collectionName = this.collectionNameConvert.convert(clazz);
        return getCollection(collectionName);
    }

    public MongoCollection<Document> getCollection(String collectionName) {
        createIndex = null;
        MongoCollection<Document> mongoCollection;
        // 检查连接是否需要重新创建
        if (!this.collectionMap.containsKey(collectionName)) {
            if (connectMongoDB == null || !Objects.equals(connectMongoDB.getCollection(), collectionName)){
                connectMongoDB = new ConnectMongoDB(mongoClient, baseProperty.getDatabase(), collectionName);
            }
            mongoCollection = connectMongoDB.open();
            this.collectionMap.put(collectionName, mongoCollection);
        }else {
            mongoCollection = this.collectionMap.get(collectionName);
        }
        return mongoCollection.withCodecRegistry(RegisterCodecUtil.getCodecCacheAndDefault());
    }

    private <T> Document processIdField(T entity,Boolean skip){
        Document tableFieldMap = DocumentUtil.checkTableField(entity,true);
        fillId(entity, tableFieldMap);
        if (HandlerCache.documentHandler != null && !skip){
            //经过一下Document处理器
            tableFieldMap = HandlerCache.documentHandler.insertInvoke(Collections.singletonList(tableFieldMap)).get(0);
        }
        return tableFieldMap;
    }

    private synchronized Integer getAutoId(Class<?> clazz) {
        String collectionName = collectionNameConvert.convert(clazz);
        MongoCollection<Document> collection = getCollection("counters");
        Document query = new Document(SqlOperationConstant._ID, collectionName);
        Document update = new Document("$inc", new Document(SqlOperationConstant.AUTO_NUM, 1));
        // 只锁当前collection
        synchronized (collectionName.intern()) {
            Document document = Optional.ofNullable(MongoTransactionContext.getClientSessionContext())
                    .map(session -> collection.findOneAndUpdate(session, query, update, new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)))
                    .orElseGet(() -> collection.findOneAndUpdate(query, update, new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)));
            int finalNum = 1;
            if (document == null) {
                Map<String, Object> map = new HashMap<>();
                map.put(SqlOperationConstant._ID, collectionNameConvert.convert(clazz));
                map.put(SqlOperationConstant.AUTO_NUM, finalNum);
                collection.insertOne(new Document(map));
            } else {
                finalNum = Integer.parseInt(String.valueOf(document.get(SqlOperationConstant.AUTO_NUM)));
            }
            return finalNum;
        }
    }

    private <T> void fillId(T entity, Document document) {
        // 用户自行设置了id字段
        if (document.containsKey(SqlOperationConstant._ID)) {
            // 检查一边id的入库类型
            Object idObj = document.get(SqlOperationConstant._ID);
            if (ObjectId.isValid(String.valueOf(idObj)) && !idObj.getClass().equals(ObjectId.class)) {
                document.put(SqlOperationConstant._ID, new ObjectId(String.valueOf(idObj)));
            }
            return;
        }
        Field idField = getIdField(ClassTypeUtil.getClass(entity));
        // 没有指定id字段
        if (idField == null) {
            return;
        }
        ID annotation = idField.getAnnotation(ID.class);
        Object _idValue;
        if (annotation.type() == IdTypeEnum.AUTO) {
            _idValue = getAutoId(ClassTypeUtil.getClass(entity));
        } else {
            if (annotation.type() == IdTypeEnum.OBJECT_ID){
                return;
            }
            _idValue = Generate.generateId(annotation.type());
        }
        try {
            Object value = ConversionService.convertValue(idField, ClassTypeUtil.getClass(entity).getDeclaredConstructor().newInstance(), _idValue);
            document.put(SqlOperationConstant._ID, value);
            //为自行设置id，需要在这里判断一下重入，自行设置checkTableField方法会进行处理
            if (annotation.saveField()){
                document.put(idField.getName(),value);
            }
        } catch (IllegalAccessException | InstantiationException | InvocationTargetException |
                 NoSuchMethodException e) {
            logger.error("Failed to convert to entity class's' _id 'field type when filling in'_id',error message: {}",e.getMessage(),e);
            throw new RuntimeException(e);
        }
    }

    private <T> void setBackIdValue(Document document, T entity) {
        Object idValue = document.get(SqlOperationConstant._ID);
        if (idValue == null) {
            return;
        }
        Field idField = getIdField(ClassTypeUtil.getClass(entity));
        if (idField == null) {
            return;
        }
        if (ReflectionUtils.getFieldValue(entity, idField) != null) {
            return;
        }
        try {
            //使用策略转换器回写id
            ConversionService.setValue(idField,entity,idValue);
        } catch (Exception e) {
            logger.error("set back id field value error, error message: {}", e.getMessage());
        }
    }

    private <T> void ifPresentOrElse(T value,Consumer<? super T> action, Runnable emptyAction) {
        if (value != null) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    private <T> List<Document> processIdFieldList(Collection<T> entityList){
        List<Document> documentList = entityList.stream().map(document -> processIdField(document,true)).collect(Collectors.toList());
        return Optional.ofNullable(HandlerCache.documentHandler).map(documentHandler -> documentHandler.insertInvoke(documentList)).orElse(documentList);
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

    public CollectionNameConvert getCollectionNameConvert() {
        return collectionNameConvert;
    }

    public void setCollectionNameConvert(CollectionNameConvert collectionNameConvert) {
        this.collectionNameConvert = collectionNameConvert;
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

    public String getCreateIndex() {
        return createIndex;
    }

    public void setCreateIndex(String createIndex) {
        this.createIndex = createIndex;
    }
}
