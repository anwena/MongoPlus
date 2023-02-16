package com.anwen.mongo.utils;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.enums.IdType;

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
        for (Field field : fields) {
            IdType idType = field.getAnnotation(ID.class).type();
            // 是否引用ApiModelProperty注解
            boolean bool = field.isAnnotationPresent(ID.class);
            if (bool) {
                resultMap.put("fieldName",field.getName());
                resultMap.put("fieldType",field.getType());
                resultMap.put("generateType",idType);
            }
        }
        return resultMap;
    }
}
