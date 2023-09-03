package com.anwen.mongo.convert.mapper;

import com.anwen.mongo.convert.DocumentFieldMapper;
import com.mongodb.MongoException;
import org.bson.Document;

import java.lang.reflect.Field;

/**
 * 默认字段映射器
 *
 * @author JiaChaoYang
 **/
public class DefaultFieldMapper<T> implements DocumentFieldMapper<T> {

    private final Object fieldValue;

    public DefaultFieldMapper(Object fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public void mapField(Document doc, Field field, T obj) throws IllegalAccessException {
        if (field.getType().isAssignableFrom(fieldValue.getClass())){
            field.set(obj, fieldValue);
        }else {
            throw new MongoException("Database field and entity class field types do not match : "+field.getName());
        }
    }
}
