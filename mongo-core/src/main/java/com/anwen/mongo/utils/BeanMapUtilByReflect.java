package com.anwen.mongo.utils;

import com.anwen.mongo.annotation.table.TableField;
import org.bson.Document;

import java.lang.reflect.Field;
import java.util.*;

/**
 * @author JiaChaoYang
 * bean、map操作
 * @since 2023-02-09 15:08
 **/
public class BeanMapUtilByReflect {

    public static <T> List<Document> listToDocumentList(Collection<T> collection){
        List<Document> documentList = new ArrayList<>();
        collection.forEach(c -> {
            documentList.add(new Document(checkTableField(c)));
        });
        return documentList;
    }

    public static <T> Map<String,Object> checkTableField(T entity){
        Map<String,Object> resultMap = new HashMap<>();
        Class<?> entityClass = entity.getClass();
        for (Field field : entityClass.getFields()) {
            field.setAccessible(true);
            if (field.isAnnotationPresent(TableField.class)){
                TableField annotation = field.getAnnotation(TableField.class);
                if (!annotation.exist()){
                    String fieldName = annotation.value() != null ? annotation.value() : field.getName();
                    try {
                        resultMap.put(fieldName,field.get(entity));
                    } catch (IllegalAccessException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
        return resultMap;
    }

}
