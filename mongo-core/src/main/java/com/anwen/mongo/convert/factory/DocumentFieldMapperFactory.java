package com.anwen.mongo.convert.factory;

import com.anwen.mongo.convert.DocumentFieldMapper;
import com.anwen.mongo.convert.mapper.*;
import org.bson.types.ObjectId;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.util.Date;

/**
 * 映射工厂
 *
 * @author JiaChaoYang
 **/
public class DocumentFieldMapperFactory {

    public static <T> DocumentFieldMapper<T> getMapper(Field field, Object fieldValue) {
        if (field.getType().equals(Date.class)) {
            return new DateFieldMapper<>(fieldValue);
        } else if (field.getType().equals(LocalDateTime.class)) {
            return new LocalDateTimeFieldMapper<>(fieldValue);
        } else if (!isPrimitive(field.getType())) {
            return new ObjectFieldMapper<>(fieldValue);
        } else if (field.getType().equals(ObjectId.class)){
            return new ObjectIdFieldMapper<>(fieldValue);
        }else {
            return new DefaultFieldMapper<>(fieldValue);
        }
    }

    // 判断是否为基本类型或基本类型的包装类型
    private static boolean isPrimitive(Class<?> type) {
        return type.isPrimitive() || Number.class.isAssignableFrom(type) ||
                Boolean.class.isAssignableFrom(type) || Character.class.isAssignableFrom(type) ||
                String.class.isAssignableFrom(type);
    }

}
