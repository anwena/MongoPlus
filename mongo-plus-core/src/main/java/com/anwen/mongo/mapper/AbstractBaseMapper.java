package com.anwen.mongo.mapper;

import com.anwen.mongo.aggregate.Aggregate;
import com.anwen.mongo.cache.global.CollectionLogicDeleteCache;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.cache.global.TenantCache;
import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.logic.LogicDeleteHandler;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.mapping.TypeReference;
import com.anwen.mongo.model.*;
import com.anwen.mongo.toolkit.Filters;
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
import java.util.*;
import java.util.stream.Collectors;

/**
 * 抽象的baseMapper
 *
 * @author anwen
 * @date 2024/6/26 下午2:01
 */
public abstract class AbstractBaseMapper implements BaseMapper {

    private final Log log = LogFactory.getLog(AbstractBaseMapper.class);

    private final MongoPlusClient mongoPlusClient;

    private final MongoConverter mongoConverter;

    private final ExecutorFactory factory;

    private final LambdaOperate lambdaOperate = new LambdaOperate();

    public AbstractBaseMapper(MongoPlusClient mongoPlusClient, MongoConverter mongoConverter,ExecutorFactory factory) {
        this.mongoPlusClient = mongoPlusClient;
        this.mongoConverter = mongoConverter;
        this.factory = factory;
    }

    @Override
    public MongoPlusClient getMongoPlusClient() {
        return mongoPlusClient;
    }

    @Override
    public MongoConverter getMongoConverter() {
        return this.mongoConverter;
    }

    @Override
    public Execute getExecute(){
        return factory.getExecute();
    }

    @Override
    public <T> boolean save(String database, String collectionName, T entity) {
        try {
            Document document = new Document();
            mongoConverter.writeBySave(entity, document);
            InsertManyResult insertManyResult = factory.getExecute().executeSave(Collections.singletonList(document), mongoPlusClient.getCollection(database,collectionName));
            mongoConverter.reSetIdValue(entity, document);
            return insertManyResult.wasAcknowledged();
        } catch (Exception e) {
            log.error("save fail , error info : {}", e.getMessage(), e);
            throw e;
        }
    }

    @Override
    public <T> Boolean saveBatch(String database, String collectionName, Collection<T> entityList) {
        try {
            List<Document> documentList = new ArrayList<>(entityList.size());
            mongoConverter.writeBySaveBatch(entityList, documentList);
            MongoCollection<Document> collection = mongoPlusClient.getCollection(database,collectionName);
            InsertManyResult insertManyResult = factory.getExecute().executeSave(documentList, collection);
            mongoConverter.batchReSetIdValue(entityList, documentList);
            return insertManyResult.getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            log.error("saveBatch fail , error info : {}", e.getMessage(), e);
            throw e;
        }
    }

    @Override
    public Long update(String database, String collectionName, Bson queryBasic, Bson updateBasic) {
        return factory.getExecute().executeUpdate(
                Collections.singletonList(new MutablePair<>(queryBasic, updateBasic)),
                mongoPlusClient.getCollection(database,collectionName)
        ).getModifiedCount();
    }

    @Override
    public Integer bulkWrite(String database, String collectionName, List<WriteModel<Document>> writeModelList) {
        BulkWriteResult bulkWriteResult = factory.getExecute().executeBulkWrite(writeModelList,mongoPlusClient.getCollection(database,collectionName));
        return bulkWriteResult.getModifiedCount() + bulkWriteResult.getInsertedCount();
    }

    @Override
    public <T> Boolean update(String database, String collectionName, T entity, QueryChainWrapper<T, ?> queryChainWrapper) {
        MutablePair<BasicDBObject, BasicDBObject> updatePair = ConditionUtil.getUpdateCondition(queryChainWrapper.getCompareList(), entity,mongoConverter);
        return update(database,collectionName,updatePair.getLeft(),updatePair.getRight()) > 0;
    }

    @Override
    public boolean isExist(String database, String collectionName, Serializable id) {
        QueryWrapper<Object> wrapper = Wrappers.lambdaQuery().eq(SqlOperationConstant._ID, id);
        return isExist(database,collectionName,wrapper);
    }

    @Override
    public boolean isExist(String database, String collectionName, QueryChainWrapper<?, ?> queryChainWrapper) {
        BasicDBObject basicDBObject = BuildCondition.buildQueryCondition(queryChainWrapper.getCompareList());
        return factory.getExecute().executeCount(basicDBObject,null,mongoPlusClient.getCollection(database,collectionName)) >= 1;
    }

    @Override
    public Boolean update(String database, String collectionName, UpdateChainWrapper<?, ?> updateChainWrapper) {
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
        BasicDBObject targetBasicDBObject = new BasicDBObject();
        mongoConverter.write(basicDBObject,targetBasicDBObject);
        return update(database,collectionName,queryBasic,targetBasicDBObject) >= 1;
    }

    @Override
    public Boolean remove(String database, String collectionName, UpdateChainWrapper<?, ?> updateChainWrapper) {
        return remove(database,collectionName,BuildCondition.buildQueryCondition(updateChainWrapper.getCompareList())) >= 1;
    }

    @Override
    public Long remove(String database, String collectionName, Bson filter) {
        return factory.getExecute().executeRemove(filter,mongoPlusClient.getCollection(database, collectionName)).getDeletedCount();
    }

    @Override
    public long count(String database, String collectionName, QueryChainWrapper<?, ?> queryChainWrapper) {
        Execute execute = factory.getExecute();
        MongoCollection<Document> collection = mongoPlusClient.getCollection(database, collectionName);
        long line;
        if (!(CollectionLogicDeleteCache.open &&
                Objects.nonNull(LogicDeleteHandler.mapper().get(LogicDeleteHandler.getBeanClass(collection)))) &&
                CollUtil.isEmpty(queryChainWrapper.getCompareList()) &&
                (TenantCache.getIgnoreTenant() != null ||
                InterceptorCache.getTenant() == null)){
            line = execute.estimatedDocumentCount(collection);
        } else {
            line = execute.executeCount(BuildCondition.buildQueryCondition(queryChainWrapper.getCompareList()),null,collection);
        }
        return line;
    }

    @Override
    public long recentPageCount(String database, String collectionName, List<CompareCondition> compareConditionList, Integer pageNum, Integer pageSize, Integer recentPageNum) {
        if (recentPageNum == null || !(recentPageNum <= 50 && recentPageNum >= 5)) {
            // 返回-1 表示不查询总条数
            return -1L;
        }
        //分页查询  不查询实际总条数  需要单独查询  是否有数据
        //如果recentPageNum = 10  第1-6页  总页数=10  从第7页开始 需要往后 + 4 页
        int limitParam = (pageNum < (recentPageNum / 2 + 1 + recentPageNum % 2) ? recentPageNum : (pageNum + (recentPageNum / 2 + recentPageNum % 2 - 1))) * pageSize;
        CountOptions countOptions = new CountOptions();
        countOptions.skip(limitParam).limit(1);
        MongoCollection<Document> collection = mongoPlusClient.getCollection(database, collectionName);
        long isExists = factory.getExecute().executeCount(BuildCondition.buildQueryCondition(compareConditionList),countOptions, collection);
        //如果查询结果为空 则查询总条数，如果不为空则 limitParam为总条数
        if (isExists == 0) {
            // 查询真实总条数
            CountOptions countOptionsReal = new CountOptions();
            countOptionsReal.limit(limitParam);
            return factory.getExecute().executeCount(BuildCondition.buildQueryCondition(compareConditionList),countOptions, collection);
        }
        return limitParam;
    }

    @Override
    public <R> List<R> list(String database, String collectionName, Class<R> rClazz) {
        return list(database,collectionName,new TypeReference<R>(rClazz){});
    }

    @Override
    public <R> List<R> list(String database, String collectionName, TypeReference<R> typeReference) {
        FindIterable<Document> findIterable = factory.getExecute().executeQuery(null, null, null, Document.class, mongoPlusClient.getCollection(database, collectionName));
        return mongoConverter.read(findIterable, typeReference);
    }

    @Override
    public <T, R> List<R> list(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, Class<R> rClazz) {
        return list(database,collectionName,queryChainWrapper,new TypeReference<R>(rClazz){});
    }

    @Override
    public <T, R> List<R> list(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, TypeReference<R> typeReference) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        FindIterable<Document> documentFindIterable = factory.getExecute().executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), Document.class, mongoPlusClient.getCollection(database, collectionName));
        return mongoConverter.read(documentFindIterable, typeReference);
    }

    @Override
    public <T, R> List<R> aggregateList(String database, String collectionName, AggregateChainWrapper<T, ?> queryChainWrapper, Class<R> rClazz) {
        return aggregateList(database,collectionName,queryChainWrapper,new TypeReference<R>(rClazz){});
    }

    @Override
    public <T, R> List<R> aggregateList(String database, String collectionName, AggregateChainWrapper<T, ?> queryChainWrapper, TypeReference<R> typeReference) {
        List<BaseAggregate> aggregateList = queryChainWrapper.getBaseAggregateList();
        List<AggregateBasicDBObject> basicDBObjectList = queryChainWrapper.getBasicDBObjectList();
        BasicDBObject optionsBasicDBObject = queryChainWrapper.getOptionsBasicDBObject();
        List<AggregateBasicDBObject> aggregateConditionList = new ArrayList<AggregateBasicDBObject>() {{
            aggregateList.forEach(aggregate -> add(new AggregateBasicDBObject("$" + aggregate.getType(), aggregate.getPipelineStrategy().buildAggregate(),aggregate.getOrder())));
            addAll(basicDBObjectList);
        }};
        aggregateConditionList.sort(Comparator.comparingInt(AggregateBasicDBObject::getOrder));
        AggregateIterable<Document> aggregateIterable = factory.getExecute().executeAggregateOld(aggregateConditionList, Document.class, mongoPlusClient.getCollection(database, collectionName));
        AggregateUtil.aggregateOptions(aggregateIterable,optionsBasicDBObject);
        return mongoConverter.read(aggregateIterable,typeReference);
    }

    @Override
    public <R> List<R> aggregateList(String database, String collectionName, Aggregate<?> aggregate, Class<R> rClazz) {
        return aggregateList(database,collectionName,aggregate,new TypeReference<R>(rClazz){});
    }

    @Override
    public <R> List<R> aggregateList(String database, String collectionName, Aggregate<?> aggregate, TypeReference<R> typeReference) {
        List<Bson> aggregateConditionList = aggregate.getAggregateConditionList();
        AggregateIterable<Document> aggregateIterable = factory.getExecute().executeAggregate(aggregateConditionList, Document.class, mongoPlusClient.getCollection(database, collectionName));
        AggregateUtil.aggregateOptions(aggregateIterable,aggregate.getAggregateOptions());
        return mongoConverter.read(aggregateIterable,typeReference);
    }

    @Override
    public <T, R> R one(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, Class<R> rClazz) {
        return one(database,collectionName,queryChainWrapper,new TypeReference<R>(rClazz){});
    }

    @Override
    public <T, R> R one(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, TypeReference<R> typeReference) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        return mongoConverter.readDocument(factory.getExecute().executeQuery(baseLambdaQuery.getCondition(),baseLambdaQuery.getProjection(),baseLambdaQuery.getSort(), Document.class, mongoPlusClient.getCollection(database, collectionName)).limit(1),typeReference);
    }

    @Override
    public <T, R> PageResult<R> page(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<R> rClazz) {
        return page(database,collectionName,queryChainWrapper,pageNum,pageSize,new TypeReference<R>(rClazz){});
    }

    @Override
    public <T, R> PageResult<R> page(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, TypeReference<R> typeReference) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        MongoCollection<Document> collection = mongoPlusClient.getCollection(database, collectionName);
        long count;
        if (!(CollectionLogicDeleteCache.open &&
                Objects.nonNull(LogicDeleteHandler.mapper().get(LogicDeleteHandler.getBeanClass(collection)))) &&
                CollUtil.isEmpty(queryChainWrapper.getCompareList()) &&
                (TenantCache.getIgnoreTenant() != null ||
                        InterceptorCache.getTenant() == null)){
            count = factory.getExecute().estimatedDocumentCount(collection);
        }else {
            count = count(database,collectionName,queryChainWrapper);
        }
        FindIterable<Document> iterable = factory.getExecute().executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), Document.class, collection);
        return lambdaOperate.getLambdaQueryResultPage(iterable, count, new PageParam(pageNum, pageSize), typeReference, mongoConverter);
    }

    @Override
    public <T, R> List<R> pageList(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Class<R> rClazz) {
        return pageList(database,collectionName,queryChainWrapper,pageNum,pageSize,new TypeReference<R>(rClazz){});
    }

    @Override
    public <T, R> List<R> pageList(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, TypeReference<R> typeReference) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        FindIterable<Document> iterable = factory.getExecute().executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), Document.class, mongoPlusClient.getCollection(database, collectionName));
        return mongoConverter.read(iterable.skip((pageNum - 1) * pageSize).limit(pageSize), typeReference);
    }

    @Override
    public <T, R> PageResult<R> page(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, Class<R> rClazz) {
        return page(database,collectionName,queryChainWrapper,pageNum,pageSize,recentPageNum,new TypeReference<R>(rClazz){});
    }

    @Override
    public <T, R> PageResult<R> page(String database, String collectionName, QueryChainWrapper<T, ?> queryChainWrapper, Integer pageNum, Integer pageSize, Integer recentPageNum, TypeReference<R> typeReference) {
        BaseLambdaQueryResult baseLambdaQuery = lambdaOperate.baseLambdaQuery(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
        MongoCollection<Document> collection = mongoPlusClient.getCollection(database, collectionName);
        long count;
        if (!(CollectionLogicDeleteCache.open &&
                Objects.nonNull(LogicDeleteHandler.mapper().get(LogicDeleteHandler.getBeanClass(collection)))) &&
                CollUtil.isEmpty(queryChainWrapper.getCompareList()) &&
                (TenantCache.getIgnoreTenant() != null ||
                        InterceptorCache.getTenant() == null)){
            count = factory.getExecute().estimatedDocumentCount(collection);
        }else {
            count = recentPageCount(database,collectionName,queryChainWrapper.getCompareList(), pageNum,  pageSize, recentPageNum);
        }
        FindIterable<Document> iterable = factory.getExecute().executeQuery(baseLambdaQuery.getCondition(), baseLambdaQuery.getProjection(), baseLambdaQuery.getSort(), Document.class, collection);
        return lambdaOperate.getLambdaQueryResultPage(iterable, count,new PageParam(pageNum,pageSize),typeReference,mongoConverter);
    }

    @Override
    public <R> List<R> getByIds(String database, String collectionName, Collection<? extends Serializable> ids, Class<R> rClazz) {
        return getByIds(database,collectionName,ids,new TypeReference<R>(rClazz){});
    }

    @Override
    public <R> List<R> getByIds(String database, String collectionName, Collection<? extends Serializable> ids, TypeReference<R> typeReference) {
        BasicDBObject basicDBObject = checkIdType(ids);
        FindIterable<Document> iterable = factory.getExecute().executeQuery(basicDBObject,null,null, Document.class, mongoPlusClient.getCollection(database, collectionName));
        return mongoConverter.read(iterable, typeReference);
    }

    @Override
    public <R> R getById(String database, String collectionName, Serializable id, Class<R> rClazz) {
        return getById(database,collectionName,id,new TypeReference<R>(rClazz){});
    }

    @Override
    public <R> R getById(String database, String collectionName, Serializable id, TypeReference<R> typeReference) {
        BasicDBObject queryBasic = new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id));
        return mongoConverter.read(factory.getExecute().executeQuery(queryBasic,null,null, Document.class, mongoPlusClient.getCollection(database, collectionName)).first(),typeReference);
    }


    @Override
    public <R> List<R> queryCommand(String database, String collectionName, String command, Class<R> rClazz) {
        return queryCommand(database,collectionName,command,new TypeReference<R>(rClazz){});
    }

    @Override
    public <R> List<R> queryCommand(String database, String collectionName, String command, TypeReference<R> typeReference) {
        FindIterable<Document> iterable = factory.getExecute().executeQuery(BasicDBObject.parse(command),null,null, Document.class, mongoPlusClient.getCollection(database, collectionName));
        return mongoConverter.read(iterable,typeReference);
    }

    @Override
    public <R> List<R> getByColumn(String database, String collectionName, String column, Object value, Class<R> rClazz) {
        return getByColumn(database,collectionName,column,value,new TypeReference<R>(rClazz){});
    }

    @Override
    public <R> List<R> getByColumn(String database, String collectionName, String column, Object value, TypeReference<R> typeReference) {
        Bson filter = Filters.eq(column, ObjectId.isValid(String.valueOf(value)) ? new ObjectId(String.valueOf(value)) : value);
        return mongoConverter.read(factory.getExecute().executeQuery(filter,null,null, Document.class, mongoPlusClient.getCollection(database, collectionName)),typeReference);
    }

    @Override
    public long count(String database, String collectionName) {
        MongoCollection<Document> collection = mongoPlusClient.getCollection(database, collectionName);
        Execute execute = factory.getExecute();
        long line;
        if (!(CollectionLogicDeleteCache.open &&
                Objects.nonNull(LogicDeleteHandler.mapper().get(LogicDeleteHandler.getBeanClass(collection)))) &&
                (TenantCache.getIgnoreTenant() != null ||
                        InterceptorCache.getTenant() == null)){
            line = execute.estimatedDocumentCount(collection);
        } else {
            line = execute.executeCount(null,null,collection);
        }
        return line;
    }

    @Override
    public String createIndex(String database, String collectionName, Bson bson) {
        return factory.getExecute().doCreateIndex(bson,mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public String createIndex(String database, String collectionName, Bson bson, IndexOptions indexOptions) {
        return factory.getExecute().doCreateIndex(bson,indexOptions,mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public List<String> createIndexes(String database, String collectionName, List<IndexModel> indexes) {
        return factory.getExecute().doCreateIndexes(indexes,mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public List<String> createIndexes(String database, String collectionName, List<IndexModel> indexes, CreateIndexOptions createIndexOptions) {
        return factory.getExecute().doCreateIndexes(indexes,createIndexOptions,mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public List<Document> listIndexes(String database, String collectionName) {
        return factory.getExecute().doListIndexes(mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public void dropIndex(String database, String collectionName, String indexName) {
        factory.getExecute().doDropIndex(indexName,mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public void dropIndex(String database, String collectionName, String indexName, DropIndexOptions dropIndexOptions) {
        factory.getExecute().doDropIndex(indexName,dropIndexOptions,mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public void dropIndex(String database, String collectionName, Bson keys) {
        factory.getExecute().doDropIndex(keys,mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public void dropIndex(String database, String collectionName, Bson keys, DropIndexOptions dropIndexOptions) {
        factory.getExecute().doDropIndex(keys,dropIndexOptions,mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public void dropIndexes(String database, String collectionName) {
        factory.getExecute().doDropIndexes(mongoPlusClient.getCollection(database, collectionName));
    }

    @Override
    public void dropIndexes(String database, String collectionName, DropIndexOptions dropIndexOptions) {
        factory.getExecute().doDropIndexes(dropIndexOptions,mongoPlusClient.getCollection(database, collectionName));
    }

    protected BasicDBObject checkIdType(Collection<? extends Serializable> ids) {
        List<Serializable> convertedIds = ids.stream()
                .map(id -> ObjectId.isValid(String.valueOf(id)) ? new ObjectId(String.valueOf(id)) : id)
                .collect(Collectors.toList());
        return new BasicDBObject(SqlOperationConstant._ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), convertedIds));
    }

}
