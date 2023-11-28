package com.anwen.mongo.toolkit;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.PropertyFilter;
import com.alibaba.fastjson.serializer.SerializeConfig;
import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.handlers.DocumentHandler;
import org.bson.Document;
import org.bson.types.Binary;

import java.io.File;
import java.lang.reflect.Field;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;
import java.util.function.Function;
import java.util.function.UnaryOperator;

import static com.anwen.mongo.toolkit.BeanMapUtilByReflect.getFieldName;
import static com.anwen.mongo.toolkit.BeanMapUtilByReflect.getFillInsertAndUpdateField;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-11-23 13:27
 **/
public class DocumentUtil {

    public static List<Document> handleMapList(List<Map<String,Object>> mapList,Boolean isSave){
        return new ArrayList<Document>(){{
            mapList.forEach(map -> {
                add(handleMap(map,isSave));
            });
        }};
    }

    public static Document handleMap(Map<String,Object> map,Boolean isSave){
        Document document = handleDocument(new Document(map));
        return Optional.ofNullable(HandlerCache.documentHandler).map(checkSaveOrUpdate(isSave,Collections.singletonList(document)).andThen(documentList -> documentList.get(0))).orElse(document);
    }

    public static List<Document> handleDocumentList(List<Document> documentList,Boolean isSave){
        List<Document> documentArrayList = new ArrayList<Document>() {{
            documentList.forEach(document -> add(handleDocument(document)));
        }};
        return Optional.ofNullable(HandlerCache.documentHandler).map(checkSaveOrUpdate(isSave,documentList)).orElse(documentArrayList);
    }

    public static <T> Document checkUpdateField(T entity,boolean isSave){
        Document document = checkTableField(entity, isSave);
        return Optional.ofNullable(HandlerCache.documentHandler).map(checkSaveOrUpdate(isSave,Collections.singletonList(document)).andThen(documentList -> documentList.get(0))).orElse(document);
    }

    private static boolean skipCheckField(Field field) {
        CollectionField collectionField = field.getAnnotation(CollectionField.class);
        return collectionField != null && !collectionField.exist();
    }

    public static UnaryOperator<List<Document>> unaryOperator(Boolean isSave,DocumentHandler documentHandler){
        if (isSave){
            return documentHandler::insertInvoke;
        }
        return documentHandler::updateInvoke;
    }

    public static Function<DocumentHandler,List<Document>> checkSaveOrUpdate(Boolean isSave,List<Document> handleDocument){
        return documentHandler->unaryOperator(isSave, documentHandler).apply(handleDocument);
    }

    public static Document handleDocument(Document document){
        Document result = new Document();
        PropertyFilter propertyFilter = (object, name, value) -> {
            if (value instanceof LocalDate
                    || value instanceof LocalDateTime
                    || value instanceof LocalTime
                    || value instanceof Date
                    || value instanceof Binary) {
                result.put(name,value);
                return false;
            }
            return true;
        };
        String jsonString = JSON.toJSONString(document, new SerializeConfig() {{
            addFilter(Document.class,propertyFilter);
        }});
        document = Document.parse(jsonString);
        document.putAll(result);
        return document;
    }

    /**
     * 检查对象属性并返回属性值Map。
     * @param entity 对象实例
     * @return 属性值Map
     */
    public static <T> Document checkTableField(T entity,boolean isSave) {
        //定义添加自动填充字段
        Map<String,Object> insertFillMap = new HashMap<>();
        //定义添加自动填充字段
        Map<String,Object> updateFillMap = new HashMap<>();
        //定义返回结果Map
        Map<String, Object> resultMap = new HashMap<>();
        //获取实体class
        Class<?> entityClass = ClassTypeUtil.getClass(entity);
        //获取所有字段
        List<Field> fieldList = ClassTypeUtil.getFields(entityClass);
        getFillInsertAndUpdateField(fieldList,insertFillMap,updateFillMap);
        //设置所有属性可访问
        for (Field field : fieldList) {
            field.setAccessible(true);
            // 是否跳过解析
            if (skipCheckField(field)) {
                continue;
            }
            // 属性名
            String fieldName = getFieldName(field);
            // 属性值
            Object fieldValue = ReflectionUtils.getFieldValue(entity, field);
            ID idAnnotation = field.getAnnotation(ID.class);
            if (idAnnotation != null) {
                //手动设置id值，永远优先
                if (fieldValue != null){
                    resultMap.put(SqlOperationConstant._ID,fieldValue);
                    continue;
                }
                if (isSave && !idAnnotation.saveField()) {
                    continue;
                }
            }
            // 不为null再进行映射
            if (fieldValue != null){
                resultMap.put(fieldName, fieldValue);
            }
        }
        Document document = handleDocument(new Document(resultMap));
        if (HandlerCache.metaObjectHandler != null){
            if (isSave) {
                HandlerCache.metaObjectHandler.insertFill(insertFillMap,document);
            } else {
                HandlerCache.metaObjectHandler.updateFill(updateFillMap,document);
            }
        }
        return document;
    }

}
