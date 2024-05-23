package com.anwen.mongo.mapper;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.enums.AggregateOptionsEnum;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.model.*;
import com.anwen.mongo.strategy.convert.ConversionService;
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
 * baseMapper默认实现
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-02-05 11:47
 **/
public class DefaultBaseMapperImpl implements BaseMapper {
    private final Logger logger = LoggerFactory.getLogger(DefaultBaseMapperImpl.class);

    private final CollectionNameConvert collectionNameConvert;

    private final MongoPlusClient mongoPlusClient;

    private final LambdaOperate lambdaOperate = new LambdaOperate();

    private final ExecutorFactory factory = new ExecutorFactory();

    public DefaultBaseMapperImpl(MongoPlusClient mongoPlusClient) {
        this.collectionNameConvert = mongoPlusClient.getCollectionNameConvert();
        this.mongoPlusClient = mongoPlusClient;
    }

    @Override
    public MongoPlusClient getMongoPlusClient() {
        return mongoPlusClient;
    }

    @Override
    public <T> boolean save(T entity){
        try {
            Document document = processIdField(entity,false);
            InsertManyResult insertManyResult = factory.getExecute().executeSave(Collections.singletonList(document), mongoPlusClient.getCollection(ClassTypeUtil.getClass(entity)));
            setBackIdValue(document, entity);
            return insertManyResult.wasAcknowledged();
        } catch (Exception e) {
            logger.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    @Override
    public <T> Boolean saveBatch(Collection<T> entityList) {
        try {
            List<Document> documentList = processIdFieldList(entityList);
            MongoCollection<Document> collection = mongoPlusClient.getCollection(entityList.iterator().next().getClass());
            InsertManyResult insertManyResult = factory.getExecute().executeSave(documentList, collection);
            return insertManyResult.getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            logger.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    @Override
    public Long update(Bson queryBasic, Bson updateBasic, Class<?> clazz) {
        return factory.getExecute().executeUpdate(
                queryBasic,
                updateBasic,
                mongoPlusClient.getCollection(clazz)
        ).getModifiedCount();
    }

    @Override
    public Integer bulkWrite(List<WriteModel<Document>> writeModelList, Class<?> clazz) {
        BulkWriteResult bulkWriteResult = factory.getExecute().executeBulkWrite(writeModelList,mongoPlusClient.getCollection(clazz));
        return bulkWriteResult.getModifiedCount() + bulkWriteResult.getInsertedCount();
    }

    @Override
    public <T> Boolean update(T entity,QueryChainWrapper<T,?> queryChainWrapper){
        MutablePair<BasicDBObject, BasicDBObject> updatePair = getUpdateCondition(queryChainWrapper.getCompareList(), entity);
        return factory.getExecute().executeUpdate(updatePair.getLeft(),updatePair.getRight(),mongoPlusClient.getCollection(ClassTypeUtil.getClass(entity))).getModifiedCount() > 0;
    }

    private Boolean buildRemove(Serializable id, MongoCollection<Document> collection) {
        Bson filterId = Filters.eq(SqlOperationConstant._ID, ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id);
        return factory.getExecute().executeRemove(filterId,collection).getDeletedCount() >= 1;
    }

    public Boolean executeRemoveByColumn(String column,Object value,MongoCollection<Document> collection){
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return factory.getExecute().executeRemove(filter,collection).getDeletedCount() >= 1;
    }

    private Boolean executeRemoveBatchByIds(Collection<? extends Serializable> idList,MongoCollection<Document> collection){
        List<Serializable> convertedIds = idList.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        Bson objectIdBson = Filters.in(SqlOperationConstant._ID, convertedIds);
        return factory.getExecute().executeRemove(objectIdBson,collection).getDeletedCount() >= 1;
    }

    @Override
    public <T> List<T> list(Class<T> clazz) {
        return DocumentMapperConvert.mapDocumentList(factory.getExecute().executeQuery(null,null,null,mongoPlusClient.getCollection(clazz),Document.class),clazz);
    }

    @Override
    public <T> List<T> list(QueryChainWrapper<T,?> queryChainWrapper, Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        return lambdaOperate.getLambdaQueryResult(factory.getExecute().executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),mongoPlusClient.getCollection(clazz),Document.class),clazz);
    }

    @Override
    public <T> List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper, Class<T> clazz){
        List<BaseAggregate> aggregateList = queryChainWrapper.getBaseAggregateList();
        List<AggregateBasicDBObject> basicDBObjectList = queryChainWrapper.getBasicDBObjectList();
        BasicDBObject optionsBasicDBObject = queryChainWrapper.getOptionsBasicDBObject();
        List<AggregateBasicDBObject> aggregateConditionList = new ArrayList<AggregateBasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new AggregateBasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate(),aggregate.getOrder())));
            addAll(basicDBObjectList);
        }};
        aggregateConditionList.sort(Comparator.comparingInt(AggregateBasicDBObject::getOrder));
        AggregateIterable<Document> aggregateIterable = factory.getExecute().executeAggregate(aggregateConditionList, mongoPlusClient.getCollection(clazz),Document.class);
        aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return DocumentMapperConvert.mapDocumentList(aggregateIterable.iterator(),clazz);
    }

    @Override
    public <T> T one(QueryChainWrapper<T,?> queryChainWrapper,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),null,queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        return lambdaOperate.getLambdaQueryResultOne(factory.getExecute().executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),mongoPlusClient.getCollection(clazz),Document.class).limit(1),clazz);
    }

    @Override
    public <T> T limitOne(QueryChainWrapper<T, ?> queryChainWrapper,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        return lambdaOperate.getLambdaQueryResultOne(factory.getExecute().executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(),mongoPlusClient.getCollection(clazz),Document.class).limit(1),clazz);
    }

    @Override
    public <T> PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize,Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        MongoCollection<Document> collection = mongoPlusClient.getCollection(clazz);
        long count;
        if (CollUtil.isEmpty(queryChainWrapper.getCompareList())){
            count = factory.getExecute().estimatedDocumentCount(collection);
        }else {
            count = count(queryChainWrapper,clazz);
        }
        FindIterable<Document> iterable = factory.getExecute().executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collection,Document.class);
        return lambdaOperate.getLambdaQueryResultPage(iterable,count,new PageParam(pageNum,pageSize),clazz);
    }

    @Override
    public <T> List<T> pageList(QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        FindIterable<Document> iterable = factory.getExecute().executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), mongoPlusClient.getCollection(clazz),Document.class);
        return DocumentMapperConvert.mapDocumentList(iterable.skip((pageNum - 1) * pageSize).limit(pageSize), clazz);
    }

    @Override
    public <T> PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<T> clazz) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        MongoCollection<Document> collection = mongoPlusClient.getCollection(clazz);
        long count;
        if (CollUtil.isEmpty(queryChainWrapper.getCompareList())){
            count = factory.getExecute().estimatedDocumentCount(collection);
        }else {
            count = recentPageCount(queryChainWrapper.getCompareList(),clazz, pageNum,  pageSize, recentPageNum);
        }
        FindIterable<Document> iterable = factory.getExecute().executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), collection,Document.class);
        return lambdaOperate.getLambdaQueryResultPage(iterable, count,new PageParam(pageNum,pageSize),clazz);
    }

    @Override
    public <T> T getById(Serializable id,Class<T> clazz) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return DocumentMapperConvert.mapDocument(factory.getExecute().executeQuery(queryBasic,null,null,mongoPlusClient.getCollection(clazz),Document.class).first(),clazz);
    }

    @Override
    public boolean isExist(Serializable id,Class<?> clazz){
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return factory.getExecute().executeCount(queryBasic,null, mongoPlusClient.getCollection(clazz)) >= 1;
    }

    @Override
    public boolean isExist(QueryChainWrapper<?,?> queryChainWrapper,Class<?> clazz){
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(queryChainWrapper.getCompareList());
        return factory.getExecute().executeCount(basicDBObject,null,mongoPlusClient.getCollection(clazz)) >= 1;
    }

    @Override
    public <T> List<T> getByIds(Collection<? extends Serializable> ids,Class<T> clazz) {
        BasicDBObject basicDBObject = checkIdType(ids);
        FindIterable<Document> iterable = factory.getExecute().executeQuery(basicDBObject,null,null, mongoPlusClient.getCollection(clazz),Document.class);
        return DocumentMapperConvert.mapDocumentList(iterable, clazz);
    }

    @Override
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
        return factory.getExecute().executeUpdate(queryBasic,DocumentUtil.handleBasicDBObject(basicDBObject),mongoPlusClient.getCollection(clazz)).getModifiedCount() >= 1;
    }

    @Override
    public Boolean remove(UpdateChainWrapper<?, ?> updateChainWrapper, Class<?> clazz) {
        return remove(BuildCondition.buildQueryCondition(updateChainWrapper.getCompareList()),clazz) >= 1;
    }

    @Override
    public Long remove(Bson filter, Class<?> clazz) {
        return factory.getExecute().executeRemove(filter,mongoPlusClient.getCollection(clazz)).getDeletedCount();
    }

    @Override
    public long count(QueryChainWrapper<?, ?> queryChainWrapper,Class<?> clazz){
        Execute execute = factory.getExecute();
        MongoCollection<Document> collection = mongoPlusClient.getCollection(clazz);
        return Optional.ofNullable(queryChainWrapper.getCompareList())
                .map(compare -> execute.executeCount(BuildCondition.buildQueryCondition(compare),null,collection))
                .orElseGet(() -> execute.estimatedDocumentCount(collection));
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
    @Override
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
        long isExists = factory.getExecute().executeCount(BuildCondition.buildQueryCondition(compareConditionList),countOptions, mongoPlusClient.getCollection(clazz));
        //如果查询结果为空 则查询总条数，如果不为空则 limitParam为总条数
        if (isExists == 0) {
            // 查询真实总条数
            CountOptions countOptionsReal = new CountOptions();
            countOptionsReal.limit(limitParam);
            return factory.getExecute().executeCount(BuildCondition.buildQueryCondition(compareConditionList),countOptions, mongoPlusClient.getCollection(clazz));
        }
        return limitParam;
    }

    @Override
    public long count(Class<?> clazz){
        return factory.getExecute().estimatedDocumentCount(mongoPlusClient.getCollection(clazz));
    }

    @Override
    public <T> List<T> queryCommand(String command,Class<T> clazz){
        FindIterable<Document> iterable = factory.getExecute().executeQuery(BasicDBObject.parse(command),null,null, mongoPlusClient.getCollection(clazz),Document.class);
        return lambdaOperate.getLambdaQueryResult(iterable,clazz);
    }

    @Override
    public <T> List<T> getByColumn(String column,Object value,Class<T> clazz){
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return DocumentMapperConvert.mapDocumentList(factory.getExecute().executeQuery(filter,null,null,mongoPlusClient.getCollection(clazz),Document.class),clazz);
    }

    @Override
    public String createIndex(Bson bson,Class<?> clazz){
        return factory.getExecute().doCreateIndex(bson,mongoPlusClient.getCollection(clazz));
    }

    @Override
    public String createIndex(Bson bson, IndexOptions indexOptions, Class<?> clazz){
        return factory.getExecute().doCreateIndex(bson,indexOptions,mongoPlusClient.getCollection(clazz));
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes,Class<?> clazz){
        return factory.getExecute().doCreateIndexes(indexes,mongoPlusClient.getCollection(clazz));
    }

    @Override
    public List<String> createIndexes(List<IndexModel> indexes, CreateIndexOptions createIndexOptions,Class<?> clazz){
        return factory.getExecute().doCreateIndexes(indexes,createIndexOptions,mongoPlusClient.getCollection(clazz));
    }

    @Override
    public List<Document> listIndexes(Class<?> clazz){
        return factory.getExecute().doListIndexes(mongoPlusClient.getCollection(clazz));
    }

    @Override
    public void dropIndex(String indexName,Class<?> clazz){
        factory.getExecute().doDropIndex(indexName,mongoPlusClient.getCollection(clazz));
    }

    @Override
    public void dropIndex(String indexName,DropIndexOptions dropIndexOptions,Class<?> clazz){
        factory.getExecute().doDropIndex(indexName,dropIndexOptions,mongoPlusClient.getCollection(clazz));
    }

    @Override
    public void dropIndex(Bson keys,Class<?> clazz){
        factory.getExecute().doDropIndex(keys,mongoPlusClient.getCollection(clazz));
    }

    @Override
    public void dropIndex(Bson keys,DropIndexOptions dropIndexOptions,Class<?> clazz){
        factory.getExecute().doDropIndex(keys,dropIndexOptions,mongoPlusClient.getCollection(clazz));
    }

    @Override
    public void dropIndexes(Class<?> clazz){
        factory.getExecute().doDropIndexes(mongoPlusClient.getCollection(clazz));
    }

    @Override
    public void dropIndexes(DropIndexOptions dropIndexOptions,Class<?> clazz){
        factory.getExecute().doDropIndexes(dropIndexOptions,mongoPlusClient.getCollection(clazz));
    }

    protected <T> MutablePair<BasicDBObject,BasicDBObject> getUpdate(T entity){
        Document document = DocumentUtil.checkUpdateField(entity,false);
        BasicDBObject filter = ExecuteUtil.getFilter(document);
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), document);
        return new MutablePair<>(filter,update);
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

    @Override
    public <T> Document processIdField(T entity,Boolean skip){
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
            MongoCollection<Document> collection = mongoPlusClient.getCollection(clazz,"counters");
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
        java.lang.reflect.Field idField = getIdField(ClassTypeUtil.getClass(entity));
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
