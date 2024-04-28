package com.anwen.mongo.toolkit;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.enums.FieldFill;
import com.anwen.mongo.mapping.ClassInformation;
import com.anwen.mongo.mapping.FieldInformation;
import org.bson.Document;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
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

    public static void getFillInsertAndUpdateField(ClassInformation classInformation, Map<String,Object> insertFill, Map<String,Object> updateFill){
        classInformation.getFields().forEach(field -> {
            CollectionField collectionField = field.getCollectionField();
            if (collectionField != null && collectionField.fill() != FieldFill.DEFAULT){
                if (collectionField.fill() == FieldFill.INSERT){
                    insertFill.put(field.getName(),null);
                }
                if (collectionField.fill() == FieldFill.UPDATE){
                    updateFill.put(field.getName(),null);
                }
                if (collectionField.fill() == FieldFill.INSERT_UPDATE){
                    insertFill.put(field.getName(),null);
                    updateFill.put(field.getName(),null);
                }
            }
        });
    }

    public static Field getIdField(Class<?> clazz) {
        for (Field field : ClassTypeUtil.getFields(clazz)) {
            if (field.isAnnotationPresent(ID.class)) {
                return field;
            }
        }
        return null;
    }

    public static String getFieldName(Field field) {
        CollectionField collectionField = field.getAnnotation(CollectionField.class);
        if (collectionField != null && StringUtils.isNotBlank(collectionField.value())) {
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
