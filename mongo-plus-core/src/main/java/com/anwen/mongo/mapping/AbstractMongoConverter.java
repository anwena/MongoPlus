package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.bson.MongoPlusDocument;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.config.Configuration;
import com.anwen.mongo.constant.DataSourceConstant;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.incrementer.id.IdWorker;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.model.AutoFillMetaObject;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.toolkit.*;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.model.FindOneAndUpdateOptions;
import com.mongodb.client.model.ReturnDocument;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.*;

/**
 * 抽象的映射处理器
 * @author JiaChaoYang
 * @date 2024/5/1 下午6:22
 */
public abstract class AbstractMongoConverter implements MongoConverter {

    private final MongoPlusClient mongoPlusClient;

    public AbstractMongoConverter(MongoPlusClient mongoPlusClient){
        this.mongoPlusClient = mongoPlusClient;
    }

    private final Logger logger = LoggerFactory.getLogger(AbstractMongoConverter.class);

    //定义添加自动填充字段
    private final AutoFillMetaObject insertFillAutoFillMetaObject = new AutoFillMetaObject(new MongoPlusDocument());
    private final AutoFillMetaObject updateFillAutoFillMetaObject = new AutoFillMetaObject(new MongoPlusDocument());;

    @Override
    public void writeBySave(Object sourceObj, Document document) {
        //封装class信息
        ClassInformation classInformation = SimpleClassInformation.of(sourceObj);
        //如果存在元对象处理器，且插入或更新字段为空，则获取自动填充字段
        if (HandlerCache.metaObjectHandler != null && insertFillAutoFillMetaObject.isEmpty()){
            //获取所有自动填充数据
            BeanMapUtilByReflect.getFillInsertAndUpdateField(classInformation,insertFillAutoFillMetaObject,updateFillAutoFillMetaObject);
        }
        //拿到类中的@ID字段
        FieldInformation idFieldInformation = classInformation.getAnnotationField(ID.class, "@ID field not found");
        //如果没有设置
        Object idValue = idFieldInformation.getValue();
        if (idValue != null){
            if (ObjectId.isValid(String.valueOf(idValue)) && !idValue.getClass().equals(ObjectId.class)) {
                document.put(SqlOperationConstant._ID, new ObjectId(String.valueOf(idValue)));
            }
        } else {
            idValue = generateId(idFieldInformation.getId().type(),classInformation);
        }
        if (idValue != null){
            try {
                Object value = ConversionService.convertValue(idFieldInformation.getField(), sourceObj, idValue);
                document.put(SqlOperationConstant._ID, value);
                //为自行设置id，需要在这里判断一下重入，自行设置checkTableField方法会进行处理
                if (idFieldInformation.getId().saveField()){
                    document.put(idFieldInformation.getName(),value);
                }
            } catch (IllegalAccessException e) {
                logger.error("Failed to convert to entity class's' _id 'field type when filling in'_id',error message: {}",e.getMessage(),e);
                throw new RuntimeException(e);
            }
        }
        //如果存在元对象处理器，且插入或更新字段为空，则获取自动填充字段
        if (HandlerCache.metaObjectHandler != null && !insertFillAutoFillMetaObject.isEmpty()){
            HandlerCache.metaObjectHandler.insertFill(insertFillAutoFillMetaObject);
        }
        //添加自动填充字段
        document.putAll(insertFillAutoFillMetaObject.getAllFillField());
        //映射到Document
        write(sourceObj,document);
        //经过一下Document处理器
        if (HandlerCache.documentHandler != null){
            HandlerCache.documentHandler.insertInvoke(Collections.singletonList(document));
        }
    }

    @Override
    public void write(Object sourceObj, Bson bson) {
        if (null == sourceObj) {
            return;
        }
        //如果为空，则创建一个
        bson = bson != null ? bson : new Document();
        write(sourceObj,bson,SimpleClassInformation.of(sourceObj));
    }

    /**
     * 抽象的映射方法
     * @param sourceObj        映射源对象
     * @param bson             映射对象
     * @param classInformation 类信息
     * @author anwen
     * @date 2024/5/1 下午6:40
     */
    public abstract void write(Object sourceObj, Bson bson, ClassInformation classInformation);

    /**
     * 生成id，写在这里，方便自己自定义
     * @param idTypeEnum id枚举类型
     * @param classInformation 类信息
     * @return {@link Serializable}
     * @author anwen
     * @date 2024/5/1 下午9:26
     */
    public Serializable generateId(IdTypeEnum idTypeEnum,ClassInformation classInformation){
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_UUID.getKey()){
            return IdWorker.get32UUID();
        }
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_ULID.getKey()){
            return IdWorker.get26ULID();
        }
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_ID.getKey()){
            return IdWorker.getId();
        }
        if (idTypeEnum.getKey() == IdTypeEnum.AUTO.getKey()){
            return generateAutoId(classInformation);
        }
        return null;
    }

    /**
     * 生成自增id，写在这里，方便自定义
     * @param classInformation 类信息
     * @return {@link Integer}
     * @author anwen
     * @date 2024/5/1 下午9:26
     */
    public Integer generateAutoId(ClassInformation classInformation){
        CollectionNameConvert collectionNameConvert = mongoPlusClient.getCollectionNameConvert();
        String collectionName = collectionNameConvert.convert(classInformation.getClazz());
        // 每个Collection单独加锁
        synchronized (collectionName.intern()) {
            MongoCollection<Document> collection = mongoPlusClient.getCollection(classInformation.getClazz(), PropertyCache.autoIdCollectionName);
            Document query = new Document(SqlOperationConstant._ID, collectionName);
            Document update = new Document("$inc", new Document(SqlOperationConstant.AUTO_NUM, 1));
            Document document = Optional.ofNullable(MongoTransactionContext.getClientSessionContext())
                    .map(session -> collection.findOneAndUpdate(session, query, update, new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)))
                    .orElseGet(() -> collection.findOneAndUpdate(query, update, new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)));
            int finalNum = 1;
            if (document == null) {
                Map<String, Object> map = new HashMap<>();
                map.put(SqlOperationConstant._ID, collectionName);
                map.put(SqlOperationConstant.AUTO_NUM, finalNum);
                collection.insertOne(new Document(map));
            } else {
                finalNum = Integer.parseInt(String.valueOf(document.get(SqlOperationConstant.AUTO_NUM)));
            }
            return finalNum;
        }
    }

    /**
     * 将简单类型进行转换
     * @param value 值
     * @return {@link Object}
     * @author anwen
     * @date 2024/5/1 下午9:28
     */
    protected Object getPotentiallyConvertedSimpleWrite(Object value) {

        if (value == null) {
            return null;
        }

        if (CollUtil.isArray(value)) {

            if (value instanceof byte[]) {
                return value;
            }
            return BsonUtil.asCollection(value);
        }

        return Enum.class.isAssignableFrom(value.getClass()) ? ((Enum<?>) value).name() : value;
    }

}
