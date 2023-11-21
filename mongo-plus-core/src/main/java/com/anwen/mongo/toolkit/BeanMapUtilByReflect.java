package com.anwen.mongo.toolkit;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.PropertyFilter;
import com.alibaba.fastjson.serializer.SerializeConfig;
import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.cache.global.AutoFillCache;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.FieldFill;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.handlers.MetaObjectHandler;
import org.bson.Document;

import java.lang.reflect.Field;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;


/**
 * @author JiaChaoYang
 * bean、map操作
 * @since 2023-02-09 15:08
 **/
public class BeanMapUtilByReflect {

    public static List<Document> mapListToDocumentList(Collection<Map<String,Object>> mapCollection){
        return mapCollection.stream().map(map -> Document.parse(JSON.toJSONString(map))).collect(Collectors.toList());
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
        Document document = handleMap(resultMap);
        if (AutoFillCache.metaObjectHandler != null){
            if (isSave) {
                AutoFillCache.metaObjectHandler.insertFill(insertFillMap,document);
            } else {
                AutoFillCache.metaObjectHandler.updateFill(updateFillMap,document);
            }
        }
        return document;
    }

    public static Document handleDocument(Document document){
        Document result = new Document();
        PropertyFilter propertyFilter = (object, name, value) -> {
            if (value instanceof LocalDate
                    || value instanceof LocalDateTime
                    || value instanceof LocalTime
                    || value instanceof Date) {
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

    public static List<Document> handleDocumentList(List<Document> documentList){
        return new ArrayList<Document>(){{
            documentList.forEach(document -> {
                add(handleDocument(document));
            });
        }};
    }

    public static void getFillInsertAndUpdateField(List<Field> fieldList,Map<String,Object> insertFill,Map<String,Object> updateFill){
        fieldList.forEach(field -> {
            field.setAccessible(true);
            CollectionField collectionField = field.getAnnotation(CollectionField.class);
            if (collectionField != null && collectionField.fill() != FieldFill.DEFAULT){
                if (collectionField.fill() == FieldFill.INSERT){
                    insertFill.put(getFieldName(field),null);
                }
                if (collectionField.fill() == FieldFill.UPDATE){
                    updateFill.put(getFieldName(field),null);
                }
                if (collectionField.fill() == FieldFill.INSERT_UPDATE){
                    insertFill.put(getFieldName(field),null);
                    updateFill.put(getFieldName(field),null);
                }
            }
        });
    }

    public static List<Document> handleMapList(List<Map<String,Object>> mapList){
        return new ArrayList<Document>(){{
            mapList.forEach(map -> {
                add(handleMap(map));
            });
        }};
    }

    public static Document handleMap(Map<String,Object> map){
        return handleDocument(new Document(map));
    }

    public static Field getIdField(Class<?> clazz) {
        for (Field field : ClassTypeUtil.getFields(clazz)) {
            if (field.isAnnotationPresent(ID.class)) {
                return field;
            }
        }
        return null;
    }

    private static boolean skipCheckField(Field field) {
        CollectionField collectionField = field.getAnnotation(CollectionField.class);
        return collectionField != null && !collectionField.exist();
    }

    private static String getFieldName(Field field) {
        CollectionField collectionField = field.getAnnotation(CollectionField.class);
        if (collectionField != null) {
            return collectionField.value();
        }
        return field.getName();
    }

    private static void setChildFieldValue(String fieldName , Map<String,Object> childMap,Map<String,Object> resultMap){
        childMap.values().removeIf(Objects::isNull);
        childMap.keySet().forEach(map -> resultMap.put(fieldName + "." + map, childMap.get(map)));
    }

    /**
     * 处理异常并将其抛出。
     * @param e 异常对象
     */
    private static void handleException(Exception e) {
        throw new RuntimeException(e);
    }
}
