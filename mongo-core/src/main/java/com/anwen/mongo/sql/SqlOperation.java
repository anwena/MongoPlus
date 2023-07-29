package com.anwen.mongo.sql;

import cn.hutool.core.collection.CollUtil;
import com.anwen.mongo.annotation.CutInID;
import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.enums.CompareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.enums.QueryOperator;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.sql.comm.ConnectMongoDB;
import com.anwen.mongo.sql.interfaces.CompareCondition;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.model.BaseProperty;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;
import com.anwen.mongo.sql.model.SlaveDataSource;
import com.anwen.mongo.sql.support.SFunction;
import com.anwen.mongo.utils.BeanMapUtilByReflect;
import com.anwen.mongo.utils.Converter;
import com.anwen.mongo.utils.StringUtils;
import com.anwen.mongo.utils.codec.RegisterCodecUtil;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoException;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.Filters;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.InsertManyResult;
import com.mongodb.client.result.InsertOneResult;
import com.mongodb.client.result.UpdateResult;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import org.bson.Document;
import org.bson.codecs.configuration.CodecRegistries;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.stream.Collectors;

import static com.anwen.mongo.utils.BeanMapUtilByReflect.checkTableField;

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
    
    private final String _ID = "_id";

    private String createIndex = null;

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
            collectionMap.put(tableName, connectMongoDB.open());
        } catch (MongoException e) {
            log.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    public void init(String collectionName) {
        try {
            connectMongoDB = new ConnectMongoDB(mongoClient, baseProperty.getDatabase(), collectionName);
            collectionMap.put(collectionName, connectMongoDB.open());
        } catch (MongoException e) {
            log.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    @CutInID
    public Boolean doSave(T entity) {
        try {
            InsertOneResult insertOneResult = getCollection(entity).insertOne(new Document(checkTableField(entity)));
            return insertOneResult.wasAcknowledged();
        } catch (Exception e) {
            log.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    @CutInID
    public Boolean doSave(String collectionName, Map<String, Object> entityMap) {
        try {
            InsertOneResult insertOneResult = getCollection(entityMap, collectionName).insertOne(new Document(checkTableField(entityMap)));
            return insertOneResult.wasAcknowledged();
        } catch (Exception e) {
            log.error("save fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSaveBatch(Collection<T> entityList) {
        try {
            InsertManyResult insertManyResult = getCollection(entityList.iterator().next()).insertMany(BeanMapUtilByReflect.listToDocumentList(entityList));
            return insertManyResult.getInsertedIds().size() == entityList.size();
        } catch (Exception e) {
            log.error("saveBatch fail , error info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doSaveBatch(String collectionName, Collection<Map<String, Object>> entityList) {
        try {
            InsertManyResult insertManyResult = getCollection(entityList.iterator().next(), collectionName).insertMany(BeanMapUtilByReflect.listToDocumentList(entityList));
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
        //            Class<?> entityClass = entity.getClass().getSuperclass();
//            Field field = entityClass.getFields()[0];
//            String id = String.valueOf(field.get(entity));
        if (entityMap.containsKey(_ID)) {
            return doUpdateById(collectionName, entityMap);
        }
        return doSave(collectionName, entityMap);
    }


    public Boolean doSaveOrUpdateBatch(Collection<T> entityList) {
        List<T> insertList = new ArrayList<>();
        for (Document document : getCollection(entityList.iterator().next()).find(
                Filters.in(_ID, entityList.stream().map(entity -> {
                    try {
                        return (String) entity.getClass().getSuperclass().getMethod("getId").invoke(entity);
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
        for (Document document : getCollection(entityList.iterator().next(), collectionName).find(
                Filters.in(_ID, entityList.stream().map(entity -> entity.get(_ID)).collect(Collectors.toList()))
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
        try {
            BasicDBObject filter = new BasicDBObject(_ID, entity.getClass().getSuperclass().getMethod("getId").invoke(entity).toString());
            BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), new Document(checkTableField(entity)));
            updateResult = getCollection(entity).updateOne(filter, update);
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            log.error("update fail , fail info : {}", e.getMessage(), e);
            return false;
        }
        return updateResult.getModifiedCount() != 0;
    }

    public Boolean doUpdateById(String collectionName, Map<String, Object> entityMap) {
        if (entityMap.containsKey(_ID)) {
            throw new MongoException("_id undefined");
        }
        UpdateResult updateResult;
        BasicDBObject filter = new BasicDBObject(_ID, entityMap.get(_ID));
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), new Document(entityMap));
        updateResult = getCollection(entityMap, collectionName).updateOne(filter, update);
        return updateResult.getModifiedCount() != 0;
    }


    public Boolean doUpdateBatchByIds(Collection<T> entityList) {
        for (T entity : entityList) {
            UpdateResult updateResult;
            try {
                updateResult = getCollection(entity).updateMany(Filters.eq(_ID, entity.getClass().getSuperclass().getMethod("getId").invoke(entity)), new Document(checkTableField(entity)));
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
            return updateResult.getModifiedCount() == entityList.size();
        }
        return false;
    }

    public Boolean doUpdateBatchByIds(String collectionName, Collection<Map<String, Object>> entityList) {
        for (Map<String, Object> entity : entityList) {
            UpdateResult updateResult = getCollection(entity, collectionName).updateOne(Filters.eq(_ID, entity.get(_ID)), new Document(entity));
            return updateResult.getModifiedCount() == entityList.size();
        }
        return false;
    }


    public Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        try {
            UpdateResult updateResult = getCollection(entity).updateOne(Filters.eq(column.getFieldName(), entity.getClass().getMethod(column.getFieldName()).invoke(entity)), new Document(checkTableField(entity)));
            return updateResult.getModifiedCount() > 0;
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            log.error("update fail , fail info : {}", e.getMessage(), e);
            return false;
        }
    }


    public Boolean doUpdateByColumn(T entity, String column) {
        try {
            UpdateResult updateResult = getCollection(entity).updateOne(Filters.eq(column, entity.getClass().getMethod("get"+Character.toUpperCase(column.charAt(0))+column.substring(1)).invoke(entity)), new Document(checkTableField(entity)));
            return updateResult.getModifiedCount() > 0;
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            log.error("update fail , fail info : {}", e.getMessage(), e);
            return false;
        }
    }

    public Boolean doUpdateByColumn(String collectionName,Map<String,Object> entityMap, String column) {
        UpdateResult updateResult = getCollection(entityMap,collectionName).updateOne(Filters.eq(column, entityMap.get(column)), new Document(entityMap));
        return updateResult.getModifiedCount() > 0;
    }


    public Boolean doRemoveById(Serializable id) {
        return getCollection().deleteOne(Filters.eq(_ID, id)).getDeletedCount() != 0;
    }

    public Boolean doRemoveById(String collectionName,Serializable id) {
        return getCollection(collectionName).deleteOne(Filters.eq(_ID, id)).getDeletedCount() != 0;
    }


    public Boolean doRemoveByColumn(SFunction<T, Object> column, String value) {
        return getCollection().deleteOne(Filters.eq(column.getFieldNameLine(), value)).getDeletedCount() != 0;
    }


    public Boolean doRemoveByColumn(String column, String value) {
        return getCollection().deleteOne(Filters.eq(column, value)).getDeletedCount() != 0;
    }

    public Boolean doRemoveByColumn(String collectionName,String column, String value) {
        return getCollection(collectionName).deleteOne(Filters.eq(column, value)).getDeletedCount() != 0;
    }


    public Boolean doRemoveBatchByIds(Collection<Serializable> idList) {
        return getCollection().deleteMany(Filters.in(_ID, idList)).getDeletedCount() != 0;
    }

    public Boolean doRemoveBatchByIds(String collectionName,Collection<Serializable> idList) {
        return getCollection(collectionName).deleteMany(Filters.in(_ID, idList)).getDeletedCount() != 0;
    }

    public List<T> doList() {
        return DocumentMapperConvert.mapDocumentList(getCollection().find(),mongoEntity);
    }

    public List<Map<String, Object>> doList(String collectionName) {
        return Converter.convertDocumentToMap(getCollection(collectionName).find());
    }

    public List<Map<String, Object>> doList(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList) {
        return getLambdaQueryResult(collectionName, compareConditionList, orderList);
    }

    public PageResult<Map<String, Object>> doPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, Integer pageNum, Integer pageSize) {
        return getLambdaQueryResultPage(collectionName, compareConditionList, orderList, new PageParam(pageNum, pageSize));
    }

    public PageResult<Map<String,Object>> doPage(String collectionName,Integer pageNum,Integer pageSize){
        return getLambdaQueryResultPage(collectionName,new ArrayList<>(),new ArrayList<>(),new PageParam(pageNum,pageSize));
    }

    public Map<String, Object> doOne(String collectionName, List<CompareCondition> compareConditionList) {
        List<Map<String, Object>> result = getLambdaQueryResult(collectionName, compareConditionList, new ArrayList<>());
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : new HashMap<>();
    }

    public Map<String, Object> doLimitOne(String collectionName, List<CompareCondition> compareConditionList) {
        List<Map<String, Object>> result = getLambdaQueryResult(collectionName, compareConditionList, new ArrayList<>());
        return !result.isEmpty() ? result.get(0) : new HashMap<>();
    }


    public T doGetById(String collectionName, Serializable id) {
        BasicDBObject byId = new BasicDBObject(_ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), id));
        FindIterable<Document> iterable = getCollection(collectionName).find(byId);
        return (T) iterable.first();
    }

    public List<T> doGetByIds(String collectionName, Collection<Serializable> ids) {
        BasicDBObject query = new BasicDBObject(_ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), ids));
        FindIterable<Document> iterable = getCollection(collectionName).find(query);
        return DocumentMapperConvert.mapDocumentList(iterable, mongoEntity);
    }


    public List<T> doList(List<CompareCondition> compareConditionList, List<Order> orderList) {
        return getLambdaQueryResult(compareConditionList, orderList);
    }

    public T doOne(List<CompareCondition> compareConditionList) {
        List<T> result = getLambdaQueryResult(compareConditionList, new ArrayList<>());
        if (result.size() > 1) {
            throw new MongoQueryException("query result greater than one line");
        }
        return !result.isEmpty() ? result.get(0) : null;
    }

    public T doLimitOne(List<CompareCondition> compareConditionList) {
        List<T> result = getLambdaQueryResult(compareConditionList, new ArrayList<>());
        return !result.isEmpty() ? result.get(0) : null;
    }

    public PageResult<T> doPage(List<CompareCondition> compareConditionList, List<Order> orderList, Integer pageNum, Integer pageSize) {
        return getLambdaQueryResultPage(compareConditionList, orderList, new PageParam(pageNum, pageSize));
    }

    public T doGetById(Serializable id) {
        BasicDBObject byId = new BasicDBObject(_ID, new BasicDBObject(SpecialConditionEnum.EQ.getCondition(), id));
        FindIterable<Document> iterable = getCollection().find(byId);
        return (T) iterable.first();
    }

    public List<T> doGetByIds(Collection<Serializable> ids) {
        BasicDBObject query = new BasicDBObject(_ID, new BasicDBObject(SpecialConditionEnum.IN.getCondition(), ids));
        FindIterable<Document> iterable = getCollection().find(query);
        return DocumentMapperConvert.mapDocumentList(iterable, mongoEntity);
    }

    public Boolean doUpdate(List<CompareCondition> compareConditionList) {
        BasicDBObject queryBasic = buildQueryCondition(compareConditionList);
        BasicDBObject updateBasic = buildUpdateValue(compareConditionList);
        UpdateResult updateResult = getCollection().updateMany(queryBasic, new BasicDBObject() {{
            append(SpecialConditionEnum.SET.getCondition(), updateBasic);
        }});
        return updateResult.getModifiedCount() > 0;
    }

    public Boolean doUpdate(String collectionName,List<CompareCondition> compareConditionList){
        BasicDBObject queryBasic = buildQueryCondition(compareConditionList);
        BasicDBObject updateBasic = buildUpdateValue(compareConditionList);
        UpdateResult updateResult = getCollection(collectionName).updateMany(queryBasic, new BasicDBObject() {{
            append(SpecialConditionEnum.SET.getCondition(), updateBasic);
        }});
        return updateResult.getModifiedCount() > 0;
    }

    public Boolean doRemove(List<CompareCondition> compareConditionList) {
        BasicDBObject deleteBasic = buildQueryCondition(compareConditionList);
        DeleteResult deleteResult = getCollection().deleteMany(deleteBasic);
        return deleteResult.getDeletedCount() > 0;
    }

    public Boolean doRemove(String collectionName,List<CompareCondition> compareConditionList){
        BasicDBObject deleteBasic = buildQueryCondition(compareConditionList);
        DeleteResult deleteResult = getCollection(collectionName).deleteMany(deleteBasic);
        return deleteResult.getDeletedCount() > 0;
    }

    public long doCount(String collectionName,List<CompareCondition> compareConditionList){
        return getCollection(collectionName).countDocuments(buildQueryCondition(compareConditionList));
    }

    public long doCount(String collectionName){
        return getCollection(collectionName).countDocuments();
    }

    public long doCount(){
        return getCollection().countDocuments();
    }

    public long doCount(List<CompareCondition> compareConditionList){
        return getCollection().countDocuments(buildQueryCondition(compareConditionList));
    }

    private List<T> getLambdaQueryResult(List<CompareCondition> compareConditionList, List<Order> orderList) {
        return DocumentMapperConvert.mapDocumentList(baseLambdaQuery(compareConditionList, orderList), mongoEntity);
    }

    private List<Map<String, Object>> getLambdaQueryResult(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList) {
        return Converter.convertDocumentToMap(baseLambdaQuery(collectionName, compareConditionList, orderList));
    }

    /**
     * 查询执行
     *
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:51
     */
    private FindIterable<Document> baseLambdaQuery(List<CompareCondition> compareConditionList, List<Order> orderList) {
        BasicDBObject sortCond = new BasicDBObject();
        orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        MongoCollection<Document> collection = getCollection();
        if (StringUtils.isNotBlank(createIndex)) {
            collection.createIndex(new Document(createIndex, QueryOperator.TEXT.getValue()));
        }
        return collection.find(buildQueryCondition(compareConditionList)).sort(sortCond);
    }

    private FindIterable<Document> baseLambdaQuery(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList) {
        BasicDBObject sortCond = new BasicDBObject();
        orderList.forEach(order -> sortCond.put(order.getColumn(), order.getType()));
        return getCollection(collectionName).find(buildQueryCondition(compareConditionList)).sort(sortCond);
    }

    private PageResult<T> getLambdaQueryResultPage(List<CompareCondition> compareConditionList, List<Order> orderList, PageParam pageParams) {
        PageResult<T> pageResult = new PageResult<>();
        FindIterable<Document> documentFindIterable = baseLambdaQuery(compareConditionList, orderList);
        long totalSize = doCount();
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData(DocumentMapperConvert.mapDocumentList(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize()), mongoEntity));
        return pageResult;
    }

    private PageResult<Map<String, Object>> getLambdaQueryResultPage(String collectionName, List<CompareCondition> compareConditionList, List<Order> orderList, PageParam pageParams) {
        PageResult<Map<String, Object>> pageResult = new PageResult<>();
        FindIterable<Document> documentFindIterable = baseLambdaQuery(collectionName, compareConditionList, orderList);
        long totalSize = doCount(collectionName);
        pageResult.setPageNum(pageParams.getPageNum());
        pageResult.setPageSize(pageParams.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData(Converter.convertDocumentToMap(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize())));
        return pageResult;
    }

    /**
     * 构建查询条件
     *
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:48
     */
    private BasicDBObject buildQueryCondition(List<CompareCondition> compareConditionList) {
        return new BasicDBObject() {{
            compareConditionList.stream().filter(compareCondition -> compareCondition.getType() == CompareEnum.QUERY.getKey()).collect(Collectors.toList()).forEach(compare -> {
                if (Objects.equals(compare.getCondition(), QueryOperator.LIKE.getValue()) && StringUtils.isNotBlank((String) compare.getValue())) {
                    put(compare.getColumn(), new BasicDBObject(SpecialConditionEnum.REGEX.getCondition(), compare.getValue()));
                } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.OR.getKey())) {
                    if (CollUtil.isEmpty(compare.getChildCondition())) {
                        compare.setChildCondition(Collections.singletonList(compare));
                    }
                    put(SpecialConditionEnum.OR.getCondition(), buildOrQueryCondition(compare.getChildCondition()));
                } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.NOR.getKey())) {
                    put(SpecialConditionEnum.NOR.getCondition(), buildQueryCondition(compare.getChildCondition()));
                } else if (Objects.equals(compare.getLogicType(), LogicTypeEnum.ELEMMATCH.getKey())) {
                    put(SpecialConditionEnum.ELEM_MATCH.getCondition(), buildOrQueryCondition(compare.getChildCondition()));
                } else if (Objects.equals(compare.getCondition(),QueryOperator.TEXT.getValue())) {
                    put(SpecialConditionEnum.TEXT.getCondition(), new BasicDBObject(SpecialConditionEnum.SEARCH.getCondition(), compare.getValue()));
                    createIndex = compare.getColumn();
                } else {
                    put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
                }
            });
        }};
    }

    /**
     * 构建子条件
     *
     * @author JiaChaoYang
     * @date 2023/7/16 19:59
     */
    private List<BasicDBObject> buildOrQueryCondition(List<CompareCondition> compareConditionList) {
        List<BasicDBObject> basicDBObjectList = new ArrayList<>();
        compareConditionList.forEach(compare -> {
            BasicDBObject basicDBObject = new BasicDBObject();
            if (Objects.equals(compare.getCondition(), QueryOperator.LIKE.getValue()) && StringUtils.isNotBlank((String) compare.getValue())) {
                basicDBObject.put(compare.getColumn(), new BasicDBObject(SpecialConditionEnum.REGEX.getCondition(), compare.getValue()));
            } else if (Objects.equals(compare.getCondition(), QueryOperator.AND.getValue())) {
                basicDBObjectList.add(buildQueryCondition(compare.getChildCondition()));
            } else if (Objects.equals(compare.getCondition(),QueryOperator.TEXT.getValue())) {
                basicDBObject.put(SpecialConditionEnum.TEXT.getCondition(), new BasicDBObject(SpecialConditionEnum.SEARCH.getCondition(), compare.getValue()));
                createIndex = compare.getColumn();
            }else {
                basicDBObject.put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
            }
            basicDBObjectList.add(basicDBObject);
        });
        return basicDBObjectList;
    }

    /**
     * 构建更新值
     *
     * @author JiaChaoYang
     * @date 2023/7/9 22:16
     */
    private BasicDBObject buildUpdateValue(List<CompareCondition> compareConditionList) {
        return new BasicDBObject() {{
            compareConditionList.stream().filter(compareCondition -> compareCondition.getType() == CompareEnum.UPDATE.getKey()).collect(Collectors.toList()).forEach(compare -> {
                put(compare.getColumn(), compare.getValue());
            });
        }};
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
        String tableName = clazz.getSimpleName().toLowerCase();
        if (clazz.isAnnotationPresent(CollectionName.class)) {
            tableName = clazz.getAnnotation(CollectionName.class).value();
        }
        return getCollection(tableName);
    }

    private MongoCollection<Document> getCollection(String tableName) {
        createIndex = null;
        // 检查连接是否需要重新创建
        if (!this.collectionMap.containsKey(tableName)) {
            if (connectMongoDB == null){
                connectMongoDB = new ConnectMongoDB(mongoClient, baseProperty.getDatabase(), tableName);
            }
            MongoCollection<Document> mongoCollection = connectMongoDB.open();
            this.collectionMap.put(tableName, mongoCollection);
            return mongoCollection;
        }
        return this.collectionMap.get(tableName);
    }

}
