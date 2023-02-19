package com.anwen.mongo.utils;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.enums.IdType;
import com.anwen.mongo.sql.model.BaseModelID;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

/**
 * @author JiaChaoYang
 * 注解操作
 * @since 2023-02-13 13:59
 **/
public class AnnotationUtil {

    public static Map<String,Object> getFieldAnnotation(Object object) {
        Field[] fields = object.getClass().getDeclaredFields();
        Map<String,Object> resultMap = new HashMap<>();
        Class<?> superclass = object.getClass().getSuperclass();
        IdType idType = null;
        String fieldName = "id";
        Class<?> fieldType = String.class;
        for (Field field : fields) {
            if (field.isAnnotationPresent(ID.class)){
                idType = field.getAnnotation(ID.class).type();
                fieldName = field.getName();
                fieldType = field.getType();
            }
        }
        if (superclass == BaseModelID.class){
            try {
                idType = superclass.getField("id").getAnnotation(ID.class).type();
            } catch (NoSuchFieldException e) {
                throw new RuntimeException(e);
            }
        }
        if (idType != null){
            resultMap.put("fieldName",fieldName);
            resultMap.put("fieldType",fieldType);
            resultMap.put("generateType",idType);
        }
        return resultMap;
    }
}
