package com.anwen.mongo.convert.mapper;

import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.convert.DocumentFieldMapper;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import org.bson.Document;

import java.lang.reflect.Field;

public class ObjectFieldMapper<T> implements DocumentFieldMapper<T> {

    private final Object fieldValue;

    public ObjectFieldMapper(Object fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public void mapField(Document doc, Field field, T obj) throws IllegalAccessException {
        // TODO: 处理嵌套对象字段的转换逻辑
        if (fieldValue instanceof Document) {
            Object nestedObj = DocumentMapperConvert.mapDocument((Document) fieldValue, ClassTypeUtil.getClassByFieldType(field));
            field.set(obj, nestedObj);
        } else {
            field.set(obj, fieldValue);
        }
    }
}
