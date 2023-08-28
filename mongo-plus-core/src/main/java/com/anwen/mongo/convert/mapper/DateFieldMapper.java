package com.anwen.mongo.convert.mapper;

import com.anwen.mongo.convert.DocumentFieldMapper;
import org.bson.Document;

import java.lang.reflect.Field;
import java.util.Date;

/**
 * Date类型转换器
 *
 * @author JiaChaoYang
 **/
public class DateFieldMapper<T> implements DocumentFieldMapper<T> {

    private final Object fieldValue;

    public DateFieldMapper(Object fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public void mapField(Document doc, Field field, T obj) throws IllegalAccessException {
        // TODO: 处理Date类型字段的转换逻辑
        if (fieldValue instanceof Date) {
            field.set(obj, fieldValue);
        } else if (fieldValue instanceof Long) {
            field.set(obj, new Date((Long) fieldValue));
        }
    }
}

