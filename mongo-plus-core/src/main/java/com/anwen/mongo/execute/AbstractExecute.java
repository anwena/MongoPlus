package com.anwen.mongo.execute;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.enums.AggregateOptionsEnum;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.model.*;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.support.SFunction;
import com.anwen.mongo.toolkit.*;
import com.mongodb.BasicDBObject;
import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.AggregateIterable;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.*;
import com.mongodb.client.result.InsertManyResult;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;

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

    private final Log log = LogFactory.getLog(AbstractExecute.class);

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
            InsertManyResult insertManyResult = executeSave(Collections.singletonList(document), collectionManager.getCollection(ClassTypeUtil.getClass(entity)));
            setBackIdValue(document, entity);
            return insertManyResult.wasAcknowledged();
        } catch (Exception e) {
            log.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public <T> Boolean saveBatch(Collection<T> entityList) {
        try {
            List<Document> documentList = processIdFieldList(entityList);
            MongoCollection<Document> collection = collectionManager.getCollection(entityList.iterator().next().getClass());
            return executeSave(documentList,collection).getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            log.error("saveBatch fail , error info : {}", e.getMessage(), e);
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

    public <T> Boolean saveOrUpdateWrapper(T entity,QueryChainWrapper<T, ?> queryChainWrapper){
        long count = count(queryChainWrapper, entity.getClass());
        if (count > 0){
            MutablePair<BasicDBObject, BasicDBObject> updatePair = getUpdateCondition(queryChainWrapper.getCompareList(), entity);
            return executeUpdate(updatePair.getLeft(),updatePair.getRight(),collectionManager.getCollection(ClassTypeUtil.getClass(entity))).getModifiedCount() >= 1;
        }
        return save(entity);
    }

    public <T> Boolean saveOrUpdateBatch(Collection<T> entityList) {
        List<WriteModel<Document>> writeModelList = new ArrayList<>();
        entityList.forEach(entity -> {
            String idByEntity = ClassTypeUtil.getIdByEntity(entity, true);
            if (StringUtils.isBlank(idByEntity)){
                writeModelList.add(new InsertOneModel<>(processIdField(entity,false)));
            } else {
                MutablePair<BasicDBObject,BasicDBObject> basicDBObjectPair = getUpdate(entity);
                writeModelList.add(new UpdateManyModel<>(basicDBObjectPair.getLeft(),basicDBObjectPair.getRight()));
            }
        });
        BulkWriteResult bulkWriteResult = executeBulkWrite(writeModelList,collectionManager.getCollection(entityList.stream().findFirst().get().getClass()));
        return (bulkWriteResult.getModifiedCount() + bulkWriteResult.getInsertedCount()) == entityList.size();
    }

    public <T> Boolean saveOrUpdateBatchWrapper(Collection<T> entityList,QueryChainWrapper<T, ?> queryChainWrapper){
        Class<?> clazz = entityList.stream().findFirst().get().getClass();
        List<WriteModel<Document>> writeModelList = new ArrayList<>();
        entityList.forEach(entity -> {
            long count = count(queryChainWrapper, clazz);
            if (count > 0){
                MutablePair<BasicDBObject, BasicDBObject> updatePair = getUpdateCondition(queryChainWrapper.getCompareList(), entity);
                writeModelList.add(new UpdateManyModel<>(updatePair.getLeft(),updatePair.getRight()));
            } else {
                writeModelList.add(new InsertOneModel<>(processIdField(entity,false)));
            }
        });
        BulkWriteResult bulkWriteResult = executeBulkWrite(writeModelList, collectionManager.getCollection(clazz));
        return (bulkWriteResult.getModifiedCount() + bulkWriteResult.getInsertedCount()) == entityList.size();
    }

    public <T> Boolean update(T entity,QueryChainWrapper<T,?> queryChainWrapper){
        MutablePair<BasicDBObject, BasicDBObject> updatePair = getUpdateCondition(queryChainWrapper.getCompareList(), entity);
        return executeUpdate(updatePair.getLeft(),updatePair.getRight(),collectionManager.getCollection(entity.getClass())).getModifiedCount() > 0;
    }

    public <T> Boolean updateById(T entity) {
        MutablePair<BasicDBObject,BasicDBObject> basicDBObjectPair = getUpdate(entity);
        MongoCollection<Document> collection = collectionManager.getCollection(ClassTypeUtil.getClass(entity));
        return executeUpdate(basicDBObjectPair.getLeft(),basicDBObjectPair.getRight(),collection).getModifiedCount() >= 1;
    }

    public <T> Boolean updateBatchByIds(Collection<T> entityList) {
        List<WriteModel<Document>> writeModelList = new ArrayList<>();
        entityList.forEach(entity -> {
            MutablePair<BasicDBObject,BasicDBObject> basicDBObjectPair = getUpdate(entity);
            writeModelList.add(new UpdateManyModel<>(basicDBObjectPair.getLeft(),basicDBObjectPair.getRight()));
        });
        BulkWriteResult bulkWriteResult = executeBulkWrite(writeModelList,collectionManager.getCollection(entityList.stream().findFirst().get().getClass()));
        return bulkWriteResult.getModifiedCount() == entityList.size();
    }

    public <T> MutablePair<BasicDBObject,BasicDBObject> getUpdate(T entity){
        Document document = DocumentUtil.checkUpdateField(entity,false);
        BasicDBObject filter = ExecuteUtil.getFilter(document);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        return new MutablePair<>(filter,update);
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
        return executeUpdate(filter,document,collection).getModifiedCount() >= 1;
    }

    public Boolean removeById(Serializable id, Class<?> clazz) {
        return buildRemove( id, collectionManager.getCollection(clazz));
    }

    private Boolean buildRemove(Serializable id, MongoCollection<Document> collection) {
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
        return executeRemove(filter,collection).getDeletedCount() >= 1;
    }

    public Boolean removeBatchByIds(Collection<? extends Serializable> idList,Class<?> clazz) {
        return executeRemoveBatchByIds(idList,collectionManager.getCollection(clazz));
    }

    private Boolean executeRemoveBatchByIds(Collection<? extends Serializable> idList,MongoCollection<Document> collection){
        List<Serializable> convertedIds = idList.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        Bson objectIdBson = Filters.in(SqlOperationConstant._ID, convertedIds);
        return executeRemove(objectIdBson,collection).getDeletedCount() >= 1;
    }

    public <T> List<T> list(Class<T> clazz) {
        return DocumentMapperConvert.mapDocumentList(executeQuery(null,null,null,collectionManager.getCollection(clazz),Document.class),clazz);
    }

    public <T> List<T> list(QueryChainWrapper<T,?> queryChainWrapper, Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        return lambdaOperate.getLambdaQueryResult(executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),collectionManager.getCollection(clazz),Document.class),clazz);
    }

    public <T> List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz){
        List<BaseAggregate> aggregateList = queryChainWrapper.getBaseAggregateList();
        List<AggregateBasicDBObject> basicDBObjectList = queryChainWrapper.getBasicDBObjectList();
        BasicDBObject optionsBasicDBObject = queryChainWrapper.getOptionsBasicDBObject();
        List<AggregateBasicDBObject> aggregateConditionList = new ArrayList<AggregateBasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new AggregateBasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate(),aggregate.getOrder())));
            addAll(basicDBObjectList);
        }};
        aggregateConditionList.sort(Comparator.comparingInt(AggregateBasicDBObject::getOrder));
        AggregateIterable<Document> aggregateIterable = executeAggregate(aggregateConditionList, collectionManager.getCollection(clazz),Document.class);
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz);
    }

    public <T> T one(QueryChainWrapper<T,?> queryChainWrapper,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),null,queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        List<T> result = lambdaOperate.getLambdaQueryResult(executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),collectionManager.getCollection(clazz),Document.class),clazz);
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : null;
    }

    public <T> T limitOne(QueryChainWrapper<T, ?> queryChainWrapper,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        List<T> result = lambdaOperate.getLambdaQueryResult(executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),collectionManager.getCollection(clazz),Document.class),clazz);
        return !result.isEmpty() ? result.get(0) : null;
    }

    public <T> PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        FindIterable<Document> iterable = executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(clazz),Document.class);
        return lambdaOperate.getLambdaQueryResultPage(iterable,count(queryChainWrapper,clazz),new PageParam(pageNum,pageSize),clazz);
    }

    public <T> PageResult<T> page(List<CompareCondition> compareConditionList, List<Order> orderList,List<Projection> projectionList,List<BasicDBObject> basicDBObjectList, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(compareConditionList, orderList, projectionList, basicDBObjectList);
        FindIterable<Document> iterable = executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collectionManager.getCollection(clazz),Document.class);
        return lambdaOperate.getLambdaQueryResultPage(iterable, recentPageCount(compareConditionList,clazz, pageNum,  pageSize, recentPageNum),new PageParam(pageNum,pageSize),clazz);
    }

    public <T> T getById(Serializable id,Class<T> clazz) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return DocumentMapperConvert.mapDocument(executeQuery(queryBasic,null,null,collectionManager.getCollection(clazz),Document.class).first(),clazz);
    }

    public boolean isExist(Serializable id,Class<?> clazz){
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return executeCount(queryBasic,null, collectionManager.getCollection(clazz)) >= 1;
    }

    public boolean isExist(QueryChainWrapper<?,?> queryChainWrapper,Class<?> clazz){
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(queryChainWrapper.getCompareList());
        return executeCount(basicDBObject,null,collectionManager.getCollection(clazz)) >= 1;
    }

    public <T> List<T> getByIds(Collection<? extends Serializable> ids,Class<T> clazz) {
        BasicDBObject basicDBObject = checkIdType(ids);
        FindIterable<Document> iterable = executeQuery(basicDBObject,null,null, collectionManager.getCollection(clazz),Document.class);
        return DocumentMapperConvert.mapDocumentList(iterable, clazz);
    }

    public Boolean update(UpdateChainWrapper<?, ?> updateChainWrapper,Class<?> clazz) {
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(updateChainWrapper.getCompareList());
        compareConditionList.addAll(updateChainWrapper.getUpdateCompareList());
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
        return executeUpdate(queryBasic,DocumentUtil.handleBasicDBObject(basicDBObject),collectionManager.getCollection(clazz)).getModifiedCount() >= 1;
    }

    public Boolean remove(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz) {
        return executeRemove(BuildCondition.buildQueryCondition(updateChainWrapper.getCompareList()),collectionManager.getCollection(clazz)).getDeletedCount() >= 1;
    }

    public long count(QueryChainWrapper<?, ?> queryChainWrapper,Class<?> clazz){
        return executeCount(BuildCondition.buildQueryCondition(queryChainWrapper.getCompareList()),null,collectionManager.getCollection(clazz));
    }

    /**
     * 分页查询 查询总条数
     * @param compareConditionList 条件集合
     * @param clazz result class
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近n页的数据  {参数=null 表示仅查询当前页数据}  {参数取值[5-50] 表示查询最近[5-50]页的数据 建议recentPageNum等于10 参考 百度分页检索}
     * @return long
     */
    public long recentPageCount(List<CompareCondition> compareConditionList,Class<?> clazz, Integer pageNum, Integer pageSize, Integer recentPageNum){
        if (recentPageNum == null || !(recentPageNum <= 50 && recentPageNum >= 5)) {
            // 返回-1 表示不查询总条数
            return -1L;
        }
        //分页查询  不查询实际总条数  需要单独查询  是否有数据
        //如果recentPageNum = 10  第1-6页  总页数=10  从第7页开始 需要往后 + 4 页
        int limitParam = (pageNum < (recentPageNum / 2 + 1 + recentPageNum % 2) ? recentPageNum : (pageNum + (recentPageNum / 2 + recentPageNum % 2 - 1))) * pageSize;
        CountOptions countOptions = new CountOptions();
        countOptions.skip(limitParam).limit(1);
        long isExists = executeCount(BuildCondition.buildQueryCondition(compareConditionList),countOptions, collectionManager.getCollection(clazz));
        //如果查询结果为空 则查询总条数，如果不为空则 limitParam为总条数
        if (isExists == 0) {
            // 查询真实总条数
            CountOptions countOptionsReal = new CountOptions();
            countOptionsReal.limit(limitParam);
            return executeCount(BuildCondition.buildQueryCondition(compareConditionList),countOptions, collectionManager.getCollection(clazz));
        }
        return limitParam;
    }

    public long count(Class<?> clazz){
        return executeCount(null,null,collectionManager.getCollection(clazz));
    }

    public <T> List<T> queryCommand(String command,Class<T> clazz){
        FindIterable<Document> iterable = executeQuery(BasicDBObject.parse(command),null,null, collectionManager.getCollection(clazz),Document.class);
        return lambdaOperate.getLambdaQueryResult(iterable,clazz);
    }

    public <T> List<T> getByColumn(String column,Object value,Class<T> clazz){
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return DocumentMapperConvert.mapDocumentList(executeQuery(filter,null,null,collectionManager.getCollection(clazz),Document.class),clazz);
    }

    public String createIndex(Bson bson,Class<?> clazz){
        return doCreateIndex(bson,collectionManager.getCollection(clazz));
    }

    public String createIndex(Bson bson, IndexOptions indexOptions, Class<?> clazz){
        return doCreateIndex(bson,indexOptions,collectionManager.getCollection(clazz));
    }

    public List<String> createIndexes(List<IndexModel> indexes,Class<?> clazz){
        return doCreateIndexes(indexes,collectionManager.getCollection(clazz));
    }


    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions,Class<?> clazz){
        return doCreateIndexes(indexes,createIndexOptions,collectionManager.getCollection(clazz));
    }

    public List<Document> listIndexes(Class<?> clazz){
        return doListIndexes(collectionManager.getCollection(clazz));
    }

    public void dropIndex(String indexName,Class<?> clazz){
        doDropIndex(indexName,collectionManager.getCollection(clazz));
    }

    public void dropIndex(String indexName,DropIndexOptions dropIndexOptions,Class<?> clazz){
        doDropIndex(indexName,dropIndexOptions,collectionManager.getCollection(clazz));
    }

    public void dropIndex(Bson keys,Class<?> clazz){
        doDropIndex(keys,collectionManager.getCollection(clazz));
    }

    public void dropIndex(Bson keys,DropIndexOptions dropIndexOptions,Class<?> clazz){
        doDropIndex(keys,dropIndexOptions,collectionManager.getCollection(clazz));
    }

    public void dropIndexes(Class<?> clazz){
        doDropIndexes(collectionManager.getCollection(clazz));
    }

    public void dropIndexes(DropIndexOptions dropIndexOptions,Class<?> clazz){
        doDropIndexes(dropIndexOptions,collectionManager.getCollection(clazz));
    }

    protected BasicDBObject checkIdType(Collection<? extends Serializable> ids) {
        List<Serializable> convertedIds = ids.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        return new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), convertedIds));
    }

    protected <T> MutablePair<BasicDBObject,BasicDBObject> getUpdateCondition(List<CompareCondition> compareConditionList, T entity){
        BasicDBObject queryBasic = BuildCondition.buildQueryCondition(compareConditionList);
        Document document = DocumentUtil.checkUpdateField(entity,false);
        document.remove(SqlOperationConstant._ID);
        BasicDBObject updateField = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        return new MutablePair<>(queryBasic,updateField);
    }

    protected <T> Document processIdField(T entity,Boolean skip){
        Document tableFieldMap = DocumentUtil.checkTableField(entity);
        fillId(entity, tableFieldMap);
        if (HandlerCache.documentHandler != null && !skip){
            //经过一下Document处理器
            tableFieldMap = HandlerCache.documentHandler.insertInvoke(Collections.singletonList(tableFieldMap)).get(0);
        }
        return tableFieldMap;
    }

    protected Integer getAutoId(Class<?> clazz) {
        String collectionName = collectionNameConvert.convert(clazz);
        // 每个Collection单独加锁
        synchronized (collectionName.intern()) {
            MongoCollection<Document> collection = collectionManager.getCollection("counters");
            Document query = new Document(SqlOperationConstant._ID, collectionName);
            Document update = new Document("$inc", new Document(SqlOperationConstant.AUTO_NUM, 1));
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
            log.error("Failed to convert to entity class's' _id 'field type when filling in'_id',error message: {}",e.getMessage(),e);
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
            log.error("set back id field value error, error message: {}", e.getMessage());
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
        options(aggregateIterable, optionsBasicDBObject);
    }

    public static void options(AggregateIterable<?> aggregateIterable, BasicDBObject optionsBasicDBObject) {
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
