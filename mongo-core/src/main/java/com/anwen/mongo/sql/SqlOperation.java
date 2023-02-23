package com.anwen.mongo.sql;

import com.anwen.mongo.annotation.CutInID;
import com.anwen.mongo.annotation.table.TableName;
import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.domain.MongoQueryException;
import com.anwen.mongo.sql.comm.ConnectMongoDB;
import com.anwen.mongo.sql.interfaces.Compare;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;
import com.anwen.mongo.sql.model.SlaveDataSource;
import com.anwen.mongo.sql.support.SFunction;
import com.anwen.mongo.utils.BeanMapUtilByReflect;
import com.anwen.mongo.utils.StringUtils;
import com.mongodb.*;
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
import java.util.stream.Collectors;

/**
 * @Description: sql执行
 * @BelongsProject:  mongo
 * @BelongsPackage: com.anwen.mongo.sql
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-16 20:35
 * @Version: 1.0
 */
@Data
public class SqlOperation<T> {

    private static final Logger log = LoggerFactory.getLogger(SqlOperation.class);

    private MongoCollection<Document> collection;

    private String host;

    private String port;

    private String database;

    private String username;

    private String password;

    private List<SlaveDataSource> slaveDataSources;

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
                        this.host = slave.getHost();
                        this.port = slave.getPort();
                        this.database = slave.getDatabase();
                        this.username = slave.getUsername();
                        this.password = slave.getPassword();
                    }
                });
            }
        }
        //ServerAddress()两个参数分别为 服务器地址 和 端口
        ServerAddress serverAddress = new ServerAddress(host, Integer.parseInt(port));
        MongoCredential mongoCredential = MongoCredential.createScramSha1Credential(username, database, password.toCharArray());
        this.collection = new ConnectMongoDB(new MongoClient(serverAddress,mongoCredential,MongoClientOptions.builder().build()), database, tableName).open();
    }

    @CutInID
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

    protected <T> PageResult<T> doPage(Integer pageNum,Integer pageSize){
        baseLambdaQuery(new ArrayList<>(),new ArrayList<>(),new PageParam(pageNum,pageSize));
        return new PageResult<>();
    }


    protected <T> T doGetById(Serializable id) {
        BasicDBObject byId = new BasicDBObject();
        byId.put("_id",new BasicDBObject("$eq",id));
        FindIterable<Document> iterable = collection.find(byId);
        return (T) iterable.first();
    }

    private <T> List<T> baseLambdaQuery(List<Compare> compareList, List<Order> orderList, PageParam ... pageParams){
        List<T> resultList = new ArrayList<>();
        BasicDBObject queryCond = new BasicDBObject();
        compareList.forEach(compare -> {
            if (Objects.equals(compare.getCondition(),"like") && StringUtils.isNotBlank((String) compare.getValue())){
                queryCond.put(compare.getColumn(),new BasicDBObject("$regex",compare.getValue()));
            }else {
                queryCond.put(compare.getColumn(), new BasicDBObject("$" + compare.getCondition(), compare.getValue()));
            }
        });
        BasicDBObject sortCond = new BasicDBObject();
        orderList.forEach(order -> {
            sortCond.put(order.getColumn(),order.getType());
        });
        FindIterable<Document> iterable = collection.find(queryCond).sort(sortCond);
        if (pageParams != null){
            iterable = iterable.skip((pageParams[0].getPageNum() - 1)*pageParams[0].getPageSize()).limit(pageParams[0].getPageSize());
        }
        for (Object document : iterable) {
            Map<String, Object> map = BeanMapUtilByReflect.beanToMap(document);
            try {
                resultList.add((T) BeanMapUtilByReflect.mapToBean(map, t.getClass()));
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return resultList;
    }
}
