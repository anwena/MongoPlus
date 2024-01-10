package com.anwen.mongo.execute;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.enums.AggregateOptionsEnum;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.model.*;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.*;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoException;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.Collation;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.FindOneAndUpdateOptions;
import com.mongodb.client.model.ReturnDocument;
import com.mongodb.client.result.InsertOneResult;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.TimeUnit;
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
public abstract class AbstractExecute implements Execute {

    private final Logger logger = LoggerFactory.getLogger(AbstractExecute.class);

    /**
     * 计数id
     * @author JiaChaoYang
     * @date 2023/12/28 11:33
    */
    private int num = 1;

    private final CollectionManager collectionManager;

    private final CollectionNameConvert collectionNameConvert;

    private final LambdaOperate lambdaOperate = new LambdaOperate();

    public AbstractExecute(CollectionNameConvert collectionNameConvert, CollectionManager collectionManager) {
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

    public <T> Boolean saveOrUpdate(T entity) {
        String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
        if (StringUtils.isBlank(idByEntity)){
            return save(entity);
        }
        return isExist(idByEntity,entity.getClass()) ? updateById(entity) : save(entity);
    }

    public <T> Boolean saveOrUpdateBatch(Collection<T> entityList) {
        List<T> saveList = new ArrayList<>();
        List<T> updateList = new ArrayList<>();
        entityList.parallelStream().forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if ((StringUtils.isBlank(idByEntity) || !isExist(idByEntity, entity.getClass()))) {
                saveList.add(entity);
            } else {
                updateList.addAll(entityList);
            }
        });
        boolean save = false;
        boolean update = false;
        if (!saveList.isEmpty()){
            save = saveBatch(saveList);
        }
        if (!updateList.isEmpty()){
            update = updateBatchByIds(updateList);
        }
        return save == update;
    }


    public <T> Boolean updateById(T entity) {
        Document document = DocumentUtil.checkUpdateField(entity,false);
        BasicDBObject filter = ExecuteUtil.getFilter(document);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        MongoCollection<Document> collection = collectionManager.getCollection(ClassTypeUtil.getClass(entity));
        return doUpdateById(filter,update,collection).getModifiedCount() >= 1;
    }

    public <T> Boolean updateBatchByIds(Collection<T> entityList) {
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

    public Boolean removeById(Serializable id, Class<?> clazz) {
        return executeRemove( id, collectionManager.getCollection(clazz));
    }

    private Boolean executeRemove(Serializable id, MongoCollection<Document> collection) {
        Bson filterId = Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id);
        return executeRemove(filterId,collection).getDeletedCount() >= 1;
    }

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

    public <T> List<T> list(Class<T> clazz) {
        return DocumentMapperConvert.mapDocumentList(doList(collectionManager.getCollection(clazz)),clazz);
    }

    public <T> List<T> list(List<CompareCondition> compareConditionList, List<Order> orderList, List<Projection> projectionList, List<BasicDBObject> basicDBObjectList, Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        return lambdaOperate.getLambdaQueryResult(doList(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),collectionManager.getCollection(clazz)),clazz);
    }

    public <T> List<T> aggregateList(List<BaseAggregate> aggregateList, List<AggregateBasicDBObject> basicDBObjectList, BasicDBObject optionsBasicDBObject, Class<T> clazz){
        List<AggregateBasicDBObject> aggregateConditionList = new ArrayList<AggregateBasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new AggregateBasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate(),aggregate.getOrder())));
            addAll(basicDBObjectList);
        }};
        aggregateConditionList.sort(Comparator.comparingInt(AggregateBasicDBObject::getOrder));
        AggregateIterable<Document> aggregateIterable = doAggregateList(aggregateConditionList, collectionManager.getCollection(clazz));
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz);
    }

    public <T> T one(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, null, projectionList, basicDBObjectList);
        List<T> result = lambdaOperate.getLambdaQueryResult(doList(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),collectionManager.getCollection(clazz)),clazz);
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : null;
    }

    public <T> T limitOne(List<CompareCondition> compareConditionList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList,List<Order> orderList,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        List<T> result = lambdaOperate.getLambdaQueryResult(doList(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),collectionManager.getCollection(clazz)),clazz);
        return !result.isEmpty() ? result.get(0) : null;
    }

    public <T> PageResult<T> page(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        FindIterable<Document> iterable = doList(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(clazz));
        return lambdaOperate.getLambdaQueryResultPage(iterable,count(compareConditionList,clazz),new PageParam(pageNum,pageSize),clazz);
    }

    public <T> T getById(Serializable id,Class<T> clazz) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return DocumentMapperConvert.mapDocument(doGetById(queryBasic,collectionManager.getCollection(clazz)).first(),clazz);
    }

    public boolean isExist(Serializable id,Class<?> clazz){
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return executeExist(queryBasic, collectionManager.getCollection(clazz)) >= 1;
    }

    public <T> List<T> getByIds(Collection<? extends Serializable> ids,Class<T> clazz) {
        BasicDBObject basicDBObject = checkIdType(ids);
        FindIterable<Document> iterable = doGetByIds(basicDBObject, collectionManager.getCollection(clazz));
        return DocumentMapperConvert.mapDocumentList(iterable, clazz);
    }

    public Boolean update(List<CompareCondition> compareConditionList,Class<?> clazz) {
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        BasicDBObject updateBasic = BuildCondition.buildUpdateValue(compareConditionList);
        BasicDBObject basicDBObject = new BasicDBObject() {{
            append(SpecialConditionEnum.SET.getCondition(), updateBasic);
        }};
        return executeUpdate(queryBasic,basicDBObject,collectionManager.getCollection(clazz)).getModifiedCount() >= 1;
    }

    public Boolean remove(List<CompareCondition> compareConditionList,Class<?> clazz) {
        return executeRemove(BuildCondition.buildQueryCondition(compareConditionList),collectionManager.getCollection(clazz)).getDeletedCount() >= 1;
    }

    public long count(List<CompareCondition> compareConditionList,Class<?> clazz){
        return executeCountByCondition(BuildCondition.buildQueryCondition(compareConditionList),collectionManager.getCollection(clazz));
    }

    public long count(Class<?> clazz){
        return doCount(collectionManager.getCollection(clazz));
    }

    public <T> List<T> queryCommand(String command,Class<T> clazz){
        FindIterable<Document> iterable = doQueryCommand(BasicDBObject.parse(command), collectionManager.getCollection(clazz));
        return lambdaOperate.getLambdaQueryResult(iterable,clazz);
    }

    public <T> List<T> getByColumn(String column,Object value,Class<T> clazz){
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return DocumentMapperConvert.mapDocumentList(doGetByColumn(filter,collectionManager.getCollection(clazz)),clazz);
    }

    protected BasicDBObject checkIdType(Collection<? extends Serializable> ids) {
        List<Serializable> convertedIds = ids.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        return new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), convertedIds));
    }

    protected <T> Document processIdField(T entity,Boolean skip){
        Document tableFieldMap = DocumentUtil.checkTableField(entity,true);
        fillId(entity, tableFieldMap);
        if (HandlerCache.documentHandler != null && !skip){
            //经过一下Document处理器
            tableFieldMap = HandlerCache.documentHandler.insertInvoke(Collections.singletonList(tableFieldMap)).get(0);
        }
        return tableFieldMap;
    }

    protected synchronized Integer getAutoId(Class<?> clazz) {
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

    protected <T> void fillId(T entity, Document document) {
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

    protected <T> void setBackIdValue(Document document, T entity) {
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

    protected <T> void ifPresentOrElse(T value, Consumer<? super T> action, Runnable emptyAction) {
        if (value != null) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    protected <T> List<Document> processIdFieldList(Collection<T> entityList){
        List<Document> documentList = entityList.stream().map(document -> processIdField(document,true)).collect(Collectors.toList());
        return Optional.ofNullable(HandlerCache.documentHandler).map(documentHandler -> documentHandler.insertInvoke(documentList)).orElse(documentList);
    }

    protected void aggregateOptions(AggregateIterable<?> aggregateIterable,BasicDBObject optionsBasicDBObject){
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

}
