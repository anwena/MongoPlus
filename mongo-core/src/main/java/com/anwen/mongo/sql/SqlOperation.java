package com.anwen.mongo.sql;

import cn.hutool.db.Page;
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
import com.mongodb.BasicDBObject;
import com.mongodb.MongoException;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.model.Filters;
import com.mongodb.client.result.UpdateResult;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.bson.Document;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
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

    private MongoCollection<Document> collection;

    private List<SlaveDataSource> slaveDataSources;

    private BaseProperty baseProperty;

    private MongoClient mongoClient;

    private T t;

    public void init(Class<?> aClass){
        try {
            this.t = (T)aClass.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        String tableName = aClass.getSimpleName().toLowerCase();
        if (aClass.isAnnotationPresent(TableName.class)){
            TableName annotation = aClass.getAnnotation(TableName.class);
            tableName = annotation.value();
            if (StringUtils.isNotBlank(annotation.dataSource())){
                if (slaveDataSources == null || slaveDataSources.size() == 0){
                    throw new InitMongoCollectionException("No slave data source configured");
                }
                slaveDataSources.forEach(slave -> {
                    if (Objects.equals(annotation.dataSource(), slave.getSlaveName())) {
                        baseProperty.setHost(slave.getHost());
                        baseProperty.setPort(slave.getPort());
                        baseProperty.setDatabase(slave.getDatabase());
                        baseProperty.setUsername(slave.getUsername());
                        baseProperty.setPassword(slave.getPassword());
                    }
                });
            }
        }
        try {
            this.collection = new ConnectMongoDB(mongoClient,baseProperty.getDatabase(),tableName).open(t);
        } catch (MongoException e) {
            log.error("Failed to connect to MongoDB: {}" + e.getMessage(),e);
        }
    }

    @CutInID
    public Boolean doSave(T entity) {
        try {
            //连接到数据库
            collection.insertOne(new Document(checkTableField(entity)));
        }catch (Exception e){
            log.error("save fail , error info : {}",e.getMessage(),e);
            return false;
        }
        return true;
    }

    public Boolean doSaveBatch(Collection<T> entityList) {
        try {
            collection.insertMany(BeanMapUtilByReflect.listToDocumentList(entityList));
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
        for (Document document : collection.find(
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
            updateResult = collection.updateOne(filter,update);
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
                updateResult.set(collection.updateMany(Filters.eq("_id", entity.getClass().getSuperclass().getMethod("getId").invoke(entity)), new Document(checkTableField(entity))));
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                log.error("update fail , fail info : {}",e.getMessage(),e);
            }
        });
        return updateResult.get().getMatchedCount() != 0;
    }


    public Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        UpdateResult updateResult;
        try {
            updateResult = collection.updateOne(Filters.eq(column.getFieldName(), entity.getClass().getMethod("getId").invoke(entity)), new Document(checkTableField(entity)));
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    public Boolean doUpdateByColumn(T entity, String column) {
        UpdateResult updateResult;
        try {
            updateResult = collection.updateOne(Filters.eq(column, entity.getClass().getMethod("getId").invoke(entity)), new Document(checkTableField(entity)));
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    public Boolean doRemoveById(Serializable id) {
        T t = doGetById(id);
        Document document = collection.findOneAndDelete(Filters.eq("_id", id));
        return collection.deleteOne(Filters.eq("_id",id)).getDeletedCount() != 0;
    }


    public Boolean doRemoveByColumn(SFunction<T, Object> column, String value) {
        return collection.deleteOne(Filters.eq(column.getFieldNameLine(),value)).getDeletedCount() != 0;
    }


    public Boolean doRemoveByColumn(String column, String value) {
        return collection.deleteOne(Filters.eq(column,value)).getDeletedCount() != 0;
    }


    public Boolean doRemoveBatchByIds(Collection<Object> idList) {
        return collection.deleteMany(Filters.in("_id",idList)).getDeletedCount() != 0;
    }


    public List<T> doList() {
        FindIterable<Document> documents = collection.find();
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
        FindIterable<Document> iterable = collection.find(byId);
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
        return collection.find(buildQueryCondition(compareConditionList)).sort(sortCond);
    }

    private <T> List<T> getLambdaQueryResult(List<CompareCondition> compareConditionList, List<Order> orderList){
        return (List<T>) DocumentMapperConvert.mapDocumentList(baseLambdaQuery(compareConditionList,orderList),t.getClass());
    }

    private PageResult<T> getLambdaQueryResultPage(List<CompareCondition> compareConditionList, List<Order> orderList, PageParam pageParams){
        PageResult<T> pageResult = new PageResult<>();
        FindIterable<Document> documentFindIterable = baseLambdaQuery(compareConditionList, orderList);
        Integer totalSize = documentFindIterable.iterator().available();
        pageResult.setPageNum(pageResult.getPageNum());
        pageResult.setPageSize(pageResult.getPageSize());
        pageResult.setTotalSize(totalSize);
        pageResult.setTotalPages((totalSize + pageParams.getPageSize() - 1) / pageParams.getPageSize());
        pageResult.setContentData((List<T>) DocumentMapperConvert.mapDocumentList(documentFindIterable.skip((pageParams.getPageNum() - 1) * pageParams.getPageSize()).limit(pageParams.getPageSize()),t.getClass()));
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

}
