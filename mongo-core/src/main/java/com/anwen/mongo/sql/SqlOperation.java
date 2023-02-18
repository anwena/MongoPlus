package com.anwen.mongo.sql;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.annotation.table.TableName;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.sql.comm.ConnectMongoDB;
import com.anwen.mongo.sql.interfaces.Compare;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.support.SFunction;
import com.anwen.mongo.utils.BeanMapUtilByReflect;
import com.anwen.mongo.utils.GenericSuperclassUtil;
import com.anwen.mongo.utils.StringUtils;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoClient;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.model.Filters;
import com.mongodb.client.result.UpdateResult;
import lombok.Data;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * &#064;Description: sql执行
 * &#064;BelongsProject:  mongo
 * &#064;@BelongsPackage: com.anwen.mongo.sql
 * &#064;@Author: JiaChaoYang
 * &#064;@CreateTime: 2023-02-16 20:35
 * &#064;@Version: 1.0
 */
@Data
public class SqlOperation {

    private static final Logger log = LoggerFactory.getLogger(SqlOperation.class);

    private MongoCollection<Document> collection;

    private MongoClient mongoClient;

    private String database;

    public void init(Class<?> aClass){
        String tableName = aClass.getSimpleName().toLowerCase();
        if (aClass.isAnnotationPresent(TableName.class)){
            tableName = aClass.getAnnotation(TableName.class).value();
        }
        this.collection = new ConnectMongoDB(mongoClient, database, tableName).open();
    }

    public <T> Boolean doSave(T entity) {
        try {
            collection.insertOne(new Document(BeanMapUtilByReflect.checkTableField(entity)));
        }catch (Exception e){
            log.error("save fail , error info : {}",e.getMessage(),e);
            return false;
        }
        return true;
    }

    protected <T> Boolean doSaveBatch(Collection<T> entityList) {
        try {
            collection.insertMany(BeanMapUtilByReflect.listToDocumentList(entityList));
        }catch (Exception e){
            log.error("saveBatch fail , error info : {}",e.getMessage(),e);
            return false;
        }
        return true;
    }


    protected <T> Boolean doSaveOrUpdate(T entity) {
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


    protected <T> Boolean doSaveOrUpdateBatch(Collection<T> entityList) {
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


    protected <T> Boolean doUpdateById(T entity) {
        UpdateResult updateResult;
        try {
            BasicDBObject filter = new BasicDBObject("_id",entity.getClass().getSuperclass().getMethod("getId").invoke(entity).toString());
            BasicDBObject update = new BasicDBObject("$set",new Document(BeanMapUtilByReflect.checkTableField(entity)));
            updateResult = collection.updateOne(filter,update);
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    protected <T> Boolean doUpdateBatchByIds(Collection<T> entityList) {
        AtomicReference<UpdateResult> updateResult = new AtomicReference<>();
        entityList.forEach(entity -> {
            try {
                updateResult.set(collection.updateMany(Filters.eq("_id", entity.getClass().getSuperclass().getMethod("getId").invoke(entity)), new Document(BeanMapUtilByReflect.checkTableField(entity))));
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                log.error("update fail , fail info : {}",e.getMessage(),e);
            }
        });
        return updateResult.get().getMatchedCount() != 0;
    }


    protected <T> Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        UpdateResult updateResult;
        try {
            updateResult = collection.updateOne(Filters.eq(column.getFieldName(), entity.getClass().getMethod("getId").invoke(entity)), new Document(BeanMapUtilByReflect.checkTableField(entity)));
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    protected <T> Boolean doUpdateByColumn(T entity, String column) {
        UpdateResult updateResult;
        try {
            updateResult = collection.updateOne(Filters.eq(column, entity.getClass().getMethod("getId").invoke(entity)), new Document(BeanMapUtilByReflect.checkTableField(entity)));
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    protected <T> Boolean doRemoveById(Serializable id) {
        T t = doGetById(id);
        Document document = collection.findOneAndDelete(Filters.eq("_id", id));
        return collection.deleteOne(Filters.eq("_id",id)).getDeletedCount() != 0;
    }


    protected <T> Boolean doRemoveByColumn(SFunction<T, Object> column, String value) {
        return collection.deleteOne(Filters.eq(column.getFieldNameLine(),value)).getDeletedCount() != 0;
    }


    protected <T> Boolean doRemoveByColumn(String column, String value) {
        return collection.deleteOne(Filters.eq(column,value)).getDeletedCount() != 0;
    }


    protected <T> Boolean doRemoveBatchByIds(Collection<Object> idList) {
        return collection.deleteMany(Filters.in("_id",idList)).getDeletedCount() != 0;
    }


    protected <T> List<T> doList() {
        FindIterable<Document> documents = collection.find();
        MongoCursor<T> iterator = (MongoCursor<T>) documents.iterator();
        List<T> list = new ArrayList<>();
        while (iterator.hasNext()){
            list.add(iterator.next());
        }
        return list;
    }


    protected <T> List<T> doList(List<Compare> compareList, List<Order> orderList) {
        return baseLambdaQuery(compareList,orderList);
    }


    protected <T> T doOne(List<Compare> compareList, List<Order> orderList) {
        List<T> result = baseLambdaQuery(compareList, orderList);
        if (result.size() > 1){
            throw new MongoQueryException("query result greater than 1 line");
        }
        return result.size() > 0 ? result.get(0) : null;
    }


    protected <T> T doGetById(Serializable id) {
        BasicDBObject byId = new BasicDBObject();
        byId.put("_id",new BasicDBObject("$eq",id));
        FindIterable<Document> iterable = collection.find(byId);
        return (T) iterable.first();
    }

    private <T> List<T> baseLambdaQuery(List<Compare> compareList, List<Order> orderList){
        List<T> resultList = new ArrayList<>();
        BasicDBObject queryCond = new BasicDBObject();
        compareList.forEach(compare -> {
            if (Objects.equals(compare.getCondition(),"like") && StringUtils.isBlank((String) compare.getValue())){
                Pattern pattern = Pattern.compile("^.*"+compare.getValue()+".*$", Pattern.CASE_INSENSITIVE);
                queryCond.put(compare.getColumn(),pattern);
            }else {
                queryCond.put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
            }
        });
        BasicDBObject sortCond = new BasicDBObject();
        orderList.forEach(order -> {
            sortCond.put(order.getColumn(),order.getType());
        });
        for (Document document : collection.find(queryCond).sort(sortCond)) {
            resultList.add((T) document);
        }
        return resultList;
    }
}
