package com.anwen.mongo.toolkit;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.constant.SqlOperationConstant;
import org.bson.Document;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;


/**
 * @author JiaChaoYang
 * bean、map操作
 * @since 2023-02-09 15:08
 **/
public class BeanMapUtilByReflect {

    public static List<Document> mapListToDocumentList(Collection<Map<String,Object>> mapCollection){
        return mapCollection.stream().map(Document::new).collect(Collectors.toList());
    }

    /**
     * 检查对象属性并返回属性值Map。
     * @param entity 对象实例
     * @return 属性值Map
     */
    public static <T> Map<String, Object> checkTableField(T entity) {
        //定义返回结果Map
        Map<String, Object> resultMap = new HashMap<>();
        //获取实体class
        Class<?> entityClass = entity.getClass();
        //获取所有字段
        List<Field> fieldList = ClassTypeUtil.getFields(entityClass);
        //设置所有属性可访问
        AccessibleObject.setAccessible(fieldList.toArray(new Field[0]),true);
        for (Field field : fieldList) {
            // 是否跳过解析
            if (skipCheckField(field)) {
                continue;
            }
            // 属性名
            String fieldName = getFieldName(field);
            // 属性值
            Object fieldValue = ReflectionUtils.getFieldValue(entity, field);
            // 不为null再进行映射
            if (fieldValue != null){
                resultMap.put(fieldName, fieldValue);
            }
        }
        return resultMap;
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
        ID idAnnotation = field.getAnnotation(ID.class);
        if (idAnnotation != null) {
            return SqlOperationConstant._ID;
        }
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
