package com.anwen.mongo.execute;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.*;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoException;
import com.mongodb.client.ClientSession;
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
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static com.anwen.mongo.toolkit.BeanMapUtilByReflect.getIdField;

/**
 * 执行器抽象类
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-28 11:01
 **/
public abstract class Execute {

    private final Logger logger = LoggerFactory.getLogger(Execute.class);

    /**
     * 计数id
     * @author JiaChaoYang
     * @date 2023/12/28 11:33
    */
    private int num = 1;

    private final MongoClient mongoClient;

    private final BaseProperty baseProperty;

    private final CollectionManager collectionManager;

    private final CollectionNameConvert collectionNameConvert;

    private final LambdaOperate lambdaOperate = new LambdaOperate();

    public Execute(MongoClient mongoClient, BaseProperty baseProperty,CollectionNameConvert collectionNameConvert,CollectionManager collectionManager) {
        this.mongoClient = mongoClient;
        this.baseProperty = baseProperty;
        this.collectionNameConvert = collectionNameConvert;
        this.collectionManager = collectionManager;
    }

    public <T> boolean save(T entity){
        try {
            Document document = processIdField(entity,false);
            InsertOneResult insertOneResult = doSave(document, collectionManager.getCollection(ClassTypeUtil.getClass(entity)));
            setBackIdValue(document, entity);
            return insertOneResult.wasAcknowledged();
        } catch (Exception e) {
            logger.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    protected abstract InsertOneResult doSave(Document document,MongoCollection<Document> collection);


    public <T> Boolean saveBatch(Collection<T> entityList) {
        try {
            List<Document> documentList = processIdFieldList(entityList);
            MongoCollection<Document> collection = collectionManager.getCollection(entityList.iterator().next().getClass());
            return doSaveBatch(documentList,collection).getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            logger.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    protected abstract InsertManyResult doSaveBatch(List<Document> documentList,MongoCollection<Document> collection);


    public <T> Boolean saveOrUpdate(T entity) {
        String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
        if (StringUtils.isBlank(idByEntity)){
            return save(entity);
        }
        return doIsExist(idByEntity,entity.getClass()) ? doUpdateById(entity) : doSave(entity);
    }

    public <T> Boolean saveOrUpdateBatch(Collection<T> entityList) {
        List<T> saveList = new ArrayList<>();
        List<T> updateList = new ArrayList<>();
        entityList.parallelStream().forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if ((StringUtils.isBlank(idByEntity) || !doIsExist(idByEntity, entity.getClass()))) {
                saveList.add(entity);
            } else {
                updateList.addAll(entityList);
            }
        });
        boolean save = false;
        boolean update = false;
        if (!saveList.isEmpty()){
            save = clientSession != null ? doSaveBatch(saveList) : doSaveBatch(saveList);
        }
        if (!updateList.isEmpty()){
            update = clientSession != null ? doUpdateBatchByIds(updateList) : doUpdateBatchByIds(updateList);
        }
        return save == update;
    }


    public <T> Boolean updateById(T entity) {
        Document document = DocumentUtil.checkUpdateField(entity,false);
        BasicDBObject filter = getFilter(document);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        MongoCollection<Document> collection = collectionManager.getCollection(ClassTypeUtil.getClass(entity));
        return doUpdateById(filter,update,collection).getModifiedCount() >= 1;
    }

    protected abstract UpdateResult doUpdateById(BasicDBObject filter,BasicDBObject update,MongoCollection<Document> collection);

    private BasicDBObject getFilter(Map<String, Object> entityMap) {
        if (!entityMap.containsKey(SqlOperationConstant._ID)) {
            throw new MongoException("_id undefined");
        }
        Object _idValue = entityMap.get(SqlOperationConstant._ID);
        BasicDBObject filter = new BasicDBObject(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(_idValue)) ? new ObjectId(String.valueOf(entityMap.get(SqlOperationConstant._ID))) : _idValue);
        entityMap.remove(SqlOperationConstant._ID);
        return filter;
    }

    public <T> Boolean updateBatchByIds(ClientSession clientSession,Collection<T> entityList) {
        int line = 0;
        for (T entity : entityList) {
            line += updateById(entity) ? 1 : 0;
        }
        return line == entityList.size();
    }

    public <T> Boolean updateByColumn(T entity, SFunction<T, Object> column) {
        return updateByColumn(entity,column.getFieldNameLine());
    }

    public <T> Boolean updateByColumn(T entity, String column) {
        Object filterValue = ClassTypeUtil.getClassFieldValue(entity,column);
        String valueOf = String.valueOf(filterValue);
        Bson filter = Filters.eq(column, ObjectId.isValid(valueOf) ? new ObjectId(valueOf) : filterValue);
        Document document = DocumentUtil.checkUpdateField(entity,false);
        MongoCollection<Document> collection = collectionManager.getCollection(ClassTypeUtil.getClass(entity));
        return doUpdateByColumn(filter,document,collection).getModifiedCount() >= 1;
    }

    protected abstract UpdateResult doUpdateByColumn(Bson filter, Document document, MongoCollection<Document> collection);

    public Boolean removeById(Serializable id, Class<?> clazz) {
        return executeRemove( id, collectionManager.getCollection(clazz));
    }

    private Boolean executeRemove(Serializable id, MongoCollection<Document> collection) {
        Bson filterId = Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id);
        return executeRemove(filterId,collection).getDeletedCount() >= 1;
    }

    protected abstract DeleteResult executeRemove(Bson filterId,MongoCollection<Document> collection);

    public <T> Boolean removeByColumn(SFunction<T, Object> column, Object value,Class<T> clazz) {
        return removeByColumn(column.getFieldNameLine(),value,clazz);
    }

    public Boolean removeByColumn(String column, Object value,Class<?> clazz) {
        return executeRemoveByColumn(column,value,collectionManager.getCollection(clazz));
    }

    public Boolean executeRemoveByColumn(String column,Object value,MongoCollection<Document> collection){
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return executeRemoveByColumn(filter,collection).getDeletedCount() >= 1;
    }

    protected abstract DeleteResult executeRemoveByColumn(Bson filter,MongoCollection<Document> collection);

    public Boolean removeBatchByIds(Collection<? extends Serializable> idList,Class<?> clazz) {
        return executeRemoveBatchByIds(idList,collectionManager.getCollection(clazz));
    }

    private Boolean executeRemoveBatchByIds(Collection<? extends Serializable> idList,MongoCollection<Document> collection){
        List<Serializable> convertedIds = idList.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        Bson objectIdBson = Filters.in(SqlOperationConstant._ID, convertedIds);
        return executeRemoveBatchByIds(objectIdBson,collection).getDeletedCount() >= 1;
    }

    protected abstract DeleteResult executeRemoveBatchByIds(Bson objectIdBson,MongoCollection<Document> collection);

    public <T> List<T> list(Class<T> clazz) {
        return DocumentMapperConvert.mapDocumentList(doList(collectionManager.getCollection(clazz)),clazz);
    }

    protected abstract FindIterable<Document> doList(MongoCollection<Document> collection);

    public <T> List<T> list(List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Class<T> clazz) {
        Map<String, BasicDBObject> baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        return lambdaOperate.getLambdaQueryResult(doList(baseLambdaQuery.get("basicDBObject"),baseLambdaQuery.get("projectionList"),baseLambdaQuery.get("sortCond"),collectionManager.getCollection(clazz)),clazz);
    }

    public <T> T one(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz) {
        Map<String, BasicDBObject> baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, null, projectionList, basicDBObjectList);
        List<T> result = lambdaOperate.getLambdaQueryResult(doList(baseLambdaQuery.get("basicDBObject"),baseLambdaQuery.get("projectionList"),baseLambdaQuery.get("sortCond"),collectionManager.getCollection(clazz)),clazz);
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : null;
    }

    public <T> T doLimitOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList,Class<T> clazz) {
        Map<String, BasicDBObject> baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        List<T> result = lambdaOperate.getLambdaQueryResult(doList(baseLambdaQuery.get("basicDBObject"),baseLambdaQuery.get("projectionList"),baseLambdaQuery.get("sortCond"),collectionManager.getCollection(clazz)),clazz);
        return !result.isEmpty() ? result.get(0) : null;
    }

    protected abstract FindIterable<Document> doList(BasicDBObject basicDBObject,BasicDBObject projectionList,BasicDBObject sortCond,MongoCollection<Document> collection);





































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
        MongoCollection<Document> collection = collectionManager.getCollection("counters");
        Document query = new Document(SqlOperationConstant._ID, collectionNameConvert.convert(clazz));
        Document update = new Document("$inc", new Document(SqlOperationConstant.AUTO_NUM, 1));
        Document document = Optional.ofNullable(MongoTransactionContext.getClientSessionContext()).map(session -> collection.findOneAndUpdate(session,query,update,new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER))).orElseGet(() -> collection.findOneAndUpdate(query,update,new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)));
        if (document == null){
            Integer finalNum = num;
            collection.insertOne(new Document(new HashMap<String,Object>(){{
                put(SqlOperationConstant._ID, collectionNameConvert.convert(clazz));
                put(SqlOperationConstant.AUTO_NUM, finalNum);
            }}));
        }else {
            num = Integer.parseInt(String.valueOf(document.get(SqlOperationConstant.AUTO_NUM)));
        }
        return num;
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

    private <T> void ifPresentOrElse(T value, Consumer<? super T> action, Runnable emptyAction) {
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

}
