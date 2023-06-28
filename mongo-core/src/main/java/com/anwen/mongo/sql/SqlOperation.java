package com.anwen.mongo.sql;

import cn.hutool.db.Page;
import cn.hutool.json.JSONUtil;
import com.anwen.mongo.annotation.CutInID;
import com.anwen.mongo.annotation.table.TableName;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.sql.comm.ConnectMongoDB;
import com.anwen.mongo.sql.interfaces.CompareCondition;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.model.BaseProperty;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;
import com.anwen.mongo.sql.model.SlaveDataSource;
import com.anwen.mongo.sql.support.SFunction;
import com.anwen.mongo.utils.BeanMapUtilByReflect;
import com.anwen.mongo.utils.StringUtils;
import com.anwen.mongo.utils.codec.RegisterCodecUtil;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoClientSettings;
import com.mongodb.MongoException;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.model.Filters;
import com.mongodb.client.result.InsertOneResult;
import com.mongodb.client.result.UpdateResult;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.bson.Document;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.anwen.mongo.utils.BeanMapUtilByReflect.checkTableField;

/**
 * @Description: sql执行
 * @BelongsProject:  mongo
 * @BelongsPackage: com.anwen.mongo.sql
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-16 20:35
 * @Version: 1.0
 */
@Data
@Slf4j
public class SqlOperation<T> {

    private Map<String,MongoCollection<Document>> collectionMap = new HashMap<>();

    private List<SlaveDataSource> slaveDataSources;

    private BaseProperty baseProperty;

    private MongoClient mongoClient;

    // 实例化 ConnectMongoDB 对象，用于保存连接
    private ConnectMongoDB connectMongoDB;

    private Class<T> mongoEntity;

    public void setMongoEntity(Class<T> mongoEntity) {
        this.mongoEntity = mongoEntity;
    }
/*public Class<T> getEClass() {
        Type genericSuperclass = this.getClass().getGenericSuperclass();
        if (genericSuperclass instanceof ParameterizedType) {
            Type[] actualTypeArguments = ((ParameterizedType) genericSuperclass)
                    .getActualTypeArguments();
            if (actualTypeArguments != null && actualTypeArguments.length > 0) {
                mongoEntity = (Class<T>) actualTypeArguments[0];
            }
        }
        return mongoEntity;
        *//*Type superClass = getClass().getGenericSuperclass();
        if (!(superClass instanceof ParameterizedType)) {
            throw new IllegalArgumentException("无泛型类型信息");
        }
        mongoEntity = (T) Object.class;
        ParameterizedType parameterizedType = (ParameterizedType)SqlOperation.class.getGenericSuperclass();
        System.out.println(parameterizedType.getTypeName() + "--------->" + parameterizedType.getActualTypeArguments()[0].getTypeName());

        Type[] types = SqlOperation.class.getGenericInterfaces();
        for (Type type : types) {
            ParameterizedType typ = (ParameterizedType)type;

        }
        //get the Class object of this own class
        Class<? extends SqlOperation> thisClass = this.getClass();

        //get the Type Object of supper class
        Type superClassType = thisClass.getGenericSuperclass();
        ParameterizedType pt = (ParameterizedType) superClassType;

        //get the Generic Type array
        Type[] genTypeArr = pt.getActualTypeArguments();
        Type genType = genTypeArr[0];
        if (!(genType instanceof Class)) {
            return (Class<T>) Object.class;
        }

        return (Class<T>) genType;*//*
    }*/

    public void init(Class<?> clazz) {
        String tableName = clazz.getSimpleName().toLowerCase();
        if (clazz.isAnnotationPresent(TableName.class)) {
            TableName annotation = clazz.getAnnotation(TableName.class);
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
            collectionMap.put(tableName,connectMongoDB.open());
            /*if (connectMongoDB == null || !connectMongoDB.isSame(baseProperty.getDatabase(), tableName)) {
                connectMongoDB = new ConnectMongoDB(mongoClient, baseProperty.getDatabase(), tableName);
                this.collection = connectMongoDB.open(t);
            }*/
        } catch (MongoException e) {
            log.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
    }

    @CutInID
    public Boolean doSave(T entity) {
        try {
            InsertOneResult insertOneResult = getCollection().withCodecRegistry(CodecRegistries.fromRegistries(RegisterCodecUtil.registerCodec(entity))).insertOne(new Document(checkTableField(entity)));
            return insertOneResult.wasAcknowledged();
        }catch (Exception e){
            log.error("save fail , error info : {}",e.getMessage(),e);
            return false;
        }
    }

    public Boolean doSaveBatch(Collection<T> entityList) {
        try {
            getCollection().insertMany(BeanMapUtilByReflect.listToDocumentList(entityList));
        }catch (Exception e){
            log.error("saveBatch fail , error info : {}",e.getMessage(),e);
            return false;
        }
        return true;
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


    public Boolean doSaveOrUpdateBatch(Collection<T> entityList) {
        List<T> insertList = new ArrayList<>();
        for (Document document : getCollection().find(
                Filters.in("_id", entityList.stream().map(entity -> {
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
        if (insertList.size() != 0) doSaveBatch(insertList);
        doUpdateBatchByIds(entityList);
        return true;
    }


    public Boolean doUpdateById(T entity) {
        UpdateResult updateResult;
        try {
            BasicDBObject filter = new BasicDBObject("_id",entity.getClass().getSuperclass().getMethod("getId").invoke(entity).toString());
            BasicDBObject update = new BasicDBObject("$set",new Document(checkTableField(entity)));
            updateResult = getCollection().updateOne(filter,update);
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    public Boolean doUpdateBatchByIds(Collection<T> entityList) {
        AtomicReference<UpdateResult> updateResult = new AtomicReference<>();
        entityList.forEach(entity -> {
            try {
                updateResult.set(getCollection().updateMany(Filters.eq("_id", entity.getClass().getSuperclass().getMethod("getId").invoke(entity)), new Document(checkTableField(entity))));
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                log.error("update fail , fail info : {}",e.getMessage(),e);
            }
        });
        return updateResult.get().getMatchedCount() != 0;
    }


    public Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        UpdateResult updateResult;
        try {
            updateResult = getCollection().updateOne(Filters.eq(column.getFieldName(), entity.getClass().getMethod("getId").invoke(entity)), new Document(checkTableField(entity)));
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    public Boolean doUpdateByColumn(T entity, String column) {
        UpdateResult updateResult;
        try {
            updateResult = getCollection().updateOne(Filters.eq(column, entity.getClass().getMethod("getId").invoke(entity)), new Document(checkTableField(entity)));
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    public Boolean doRemoveById(Serializable id) {
        T t = doGetById(id);
        Document document = getCollection().findOneAndDelete(Filters.eq("_id", id));
        return getCollection().deleteOne(Filters.eq("_id",id)).getDeletedCount() != 0;
    }


    public Boolean doRemoveByColumn(SFunction<T, Object> column, String value) {
        return getCollection().deleteOne(Filters.eq(column.getFieldNameLine(),value)).getDeletedCount() != 0;
    }


    public Boolean doRemoveByColumn(String column, String value) {
        return getCollection().deleteOne(Filters.eq(column,value)).getDeletedCount() != 0;
    }


    public Boolean doRemoveBatchByIds(Collection<Object> idList) {
        return getCollection().deleteMany(Filters.in("_id",idList)).getDeletedCount() != 0;
    }


    public List<T> doList() {
        FindIterable<Document> documents = getCollection().find();
        MongoCursor<T> iterator = (MongoCursor<T>) documents.iterator();
        List<T> list = new ArrayList<>();
        while (iterator.hasNext()){
            list.add(iterator.next());
        }
        return list;
    }


    public List<T> doList(List<CompareCondition> compareConditionList, List<Order> orderList) {
        return getLambdaQueryResult(compareConditionList,orderList);
    }


    public T doOne(List<CompareCondition> compareConditionList) {
        List<T> result = getLambdaQueryResult(compareConditionList,new ArrayList<>());
        if (result.size() > 1){
            throw new MongoQueryException("query result greater than one line");
        }
        return result.size() > 0 ? result.get(0) : null;
    }

    public PageResult<T> doPage(List<CompareCondition> compareConditionList, List<Order> orderList , Integer pageNum, Integer pageSize){
        return getLambdaQueryResultPage(compareConditionList, orderList, new PageParam(pageNum, pageSize));
    }

    public T doGetById(Serializable id) {
        BasicDBObject byId = new BasicDBObject();
        byId.put("_id",new BasicDBObject("$eq",id));
        FindIterable<Document> iterable = getCollection().find(byId);
        return (T) iterable.first();
    }

    public Boolean doUpdate(List<CompareCondition> compareConditionList){
        //TODO 待实现
        return true;
    }

    public Boolean doRemove(List<CompareCondition> compareConditionList){
        //TODO 待实现
        return true;
    }

    /**
     * 查询执行
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:51
    */
    private FindIterable<Document> baseLambdaQuery(List<CompareCondition> compareConditionList, List<Order> orderList){
        BasicDBObject sortCond = new BasicDBObject();
        orderList.forEach(order -> sortCond.put(order.getColumn(),order.getType()));
        return getCollection().find(buildQueryCondition(compareConditionList)).sort(sortCond);
    }

    private <T> List<T> getLambdaQueryResult(List<CompareCondition> compareConditionList, List<Order> orderList){
        return (List<T>) DocumentMapperConvert.mapDocumentList(baseLambdaQuery(compareConditionList,orderList),mongoEntity);
    }

    private PageResult<T> getLambdaQueryResultPage(List<CompareCondition> compareConditionList, List<Order> orderList, PageParam pageParams){
        PageResult<T> pageResult = new PageResult<>();
        FindIterable<Document> documentFindIterable = baseLambdaQuery(compareConditionList, orderList);
        Integer totalSize = documentFindIterable.iterator().available();
        pageResult.setPageNum(pageResult.getPageNum());
        pageResult.setPageSize(pageResult.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData(DocumentMapperConvert.mapDocumentList(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize()),mongoEntity));
        return pageResult;
    }

    /**
     * 构建查询条件
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:48
    */
    private BasicDBObject buildQueryCondition(List<CompareCondition> compareConditionList){
        return new BasicDBObject(){{
            compareConditionList.stream().filter(compareCondition -> compareCondition.getType() == 0).collect(Collectors.toList()).forEach(compare -> {
                if (Objects.equals(compare.getCondition(),"like") && StringUtils.isNotBlank((String) compare.getValue())){
                    put(compare.getColumn(),new BasicDBObject("$regex",compare.getValue()));
                }else {
                    put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
                }
            });
        }};
    }

    private MongoCollection<Document> getCollection(){
        Class<?> clazz = mongoEntity;
        String tableName = clazz.getSimpleName().toLowerCase();
        if (clazz.isAnnotationPresent(TableName.class)){
            tableName = clazz.getAnnotation(TableName.class).value();
        }
        // 检查连接是否需要重新创建
        if (!this.collectionMap.containsKey(tableName)){
            MongoCollection<Document> mongoCollection = connectMongoDB.open();
            this.collectionMap.put(tableName,mongoCollection);
            return mongoCollection;
        }
        return this.collectionMap.get(tableName);
    }

}
