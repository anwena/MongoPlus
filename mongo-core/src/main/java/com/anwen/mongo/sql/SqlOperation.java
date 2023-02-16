package com.anwen.mongo.sql;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.annotation.ID;
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
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @Description:
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.sql
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-16 20:35
 * @Version: 1.0
 */
@Data
public class SqlOperation {

    private static final Logger log = LoggerFactory.getLogger(SqlOperation.class);

    private MongoCollection<Document> collection;

    private MongoClient mongoClient;

    private String database;

    public void init(Class<?> aClass){
        this.collection = new ConnectMongoDB(mongoClient, database, aClass.getSimpleName().toLowerCase()).open();
    }

    public <T> Boolean doSave(T entity) {
        try {
            Map<String, Object> beanToMap = BeanMapUtilByReflect.beanToMap(entity);
            collection.insertOne(new Document(beanToMap));
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
            Method method = entity.getClass().getMethod("getId");
            String id = (String) method.invoke(entity);
            if (doGetById(id) == null) return doSave(entity);
            return doUpdateById(entity);
        } catch (IllegalAccessException | NoSuchMethodException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }


    protected <T> Boolean doSaveOrUpdateBatch(Collection<T> entityList) {
        collection.find(new BasicDBObject());
        List<T> insertList = new ArrayList<>();
        for (Document document : collection.find(
                Filters.in("_id", entityList.stream().map(entity -> {
                    try {
                        return (String) entity.getClass().getMethod("getId").invoke(entity);
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
        Class<?> entityClass = entity.getClass();
        System.out.println(Arrays.toString(entityClass.getAnnotations()));
        System.out.println(entityClass.getAnnotation(ID.class));
        System.out.println(Arrays.toString(entityClass.getDeclaredAnnotations()));
        UpdateResult updateResult;
        try {
            String id = entity.getClass().getMethod("get_id").invoke(entity).toString();
            Document document = new Document();
            document.putAll(BeanMapUtilByReflect.beanToMap(entity));
            BasicDBObject filter = new BasicDBObject("_id",new ObjectId(id));
            BasicDBObject update = new BasicDBObject("$set",new Document(BeanMapUtilByReflect.beanToMap(entity)));
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
                updateResult.set(collection.updateMany(Filters.eq("_id", entity.getClass().getMethod("getId").invoke(entity)), new Document(BeanMapUtilByReflect.beanToMap(entity))));
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                log.error("update fail , fail info : {}",e.getMessage(),e);
            }
        });
        return updateResult.get().getMatchedCount() != 0;
    }


    protected <T> Boolean doUpdateByColumn(T entity, SFunction<T, Object> column) {
        UpdateResult updateResult;
        try {
            updateResult = collection.updateOne(Filters.eq(column.getFieldName(), entity.getClass().getMethod("getId").invoke(entity)), new Document(BeanMapUtilByReflect.beanToMap(entity)));
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    protected <T> Boolean doUpdateByColumn(T entity, String column) {
        UpdateResult updateResult;
        try {
            updateResult = collection.updateOne(Filters.eq(column, entity.getClass().getMethod("getId").invoke(entity)), new Document(BeanMapUtilByReflect.beanToMap(entity)));
        }catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e){
            log.error("update fail , fail info : {}",e.getMessage(),e);
            return false;
        }
        return updateResult.getMatchedCount() != 0;
    }


    protected <T> Boolean doRemoveById(Serializable id) {
        T t = doGetById(id);
        System.out.println(t);
        Document document = collection.findOneAndDelete(Filters.eq("_id", id));
        log.info(JSON.toJSONString(document));
        return collection.deleteOne(Filters.eq("_id",id)).getDeletedCount() != 0;
    }


    protected <T> Boolean doRemoveByColumn(Function<T, Object> column, String value) {
        return collection.deleteOne(Filters.eq(GenericSuperclassUtil.getSerializedLambda(column),value)).getDeletedCount() != 0;
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
