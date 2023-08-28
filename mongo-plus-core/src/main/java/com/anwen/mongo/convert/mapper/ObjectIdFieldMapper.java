package com.anwen.mongo.convert.mapper;

import com.anwen.mongo.convert.DocumentFieldMapper;
import org.bson.Document;

import java.lang.reflect.Field;

/**
 * @author JiaChaoYang
 **/
public class ObjectIdFieldMapper<T> implements DocumentFieldMapper<T> {

    private final Object fieldValue;

    public ObjectIdFieldMapper(Object fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public void mapField(Document doc, Field field, T obj) throws IllegalAccessException {
        field.set(obj,String.valueOf(fieldValue));
    }
}
