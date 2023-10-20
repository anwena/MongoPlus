package com.anwen.mongo.convert.factory;

import com.anwen.mongo.convert.DocumentFieldMapper;
import com.anwen.mongo.convert.mapper.CollectionFieldMapper;
import com.anwen.mongo.convert.mapper.DefaultFieldMapper;
import com.anwen.mongo.convert.mapper.DocumentTypeFieldMapper;
import com.anwen.mongo.convert.mapper.ObjectIdFieldMapper;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import org.bson.Document;
import org.bson.types.ObjectId;

import java.lang.reflect.Field;
import java.util.Collection;

/**
 * 映射工厂
 *
 * @author JiaChaoYang
 **/
public class DocumentFieldMapperFactory {

    public static <T> DocumentFieldMapper<T> getMapper(Field field, Object fieldValue) {
        Class<?> fieldType = field.getType();
        if (fieldType.isArray() || Collection.class.isAssignableFrom(fieldType)) {
            return new CollectionFieldMapper<>(fieldValue);
        } else if (ConversionService.isExist(fieldType)) {
            return new DefaultFieldMapper<>(fieldValue);
        } else if (ClassTypeUtil.isItCustomType(field) || fieldType.equals(Document.class)){
            return new DocumentTypeFieldMapper<>(fieldValue);
        } else if (fieldType.equals(ObjectId.class)) {
            return new ObjectIdFieldMapper<>(fieldValue);
        } else {
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
