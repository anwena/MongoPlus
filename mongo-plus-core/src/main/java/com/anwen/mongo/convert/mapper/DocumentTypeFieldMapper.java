package com.anwen.mongo.convert.mapper;

import com.anwen.mongo.convert.DocumentFieldMapper;
import com.anwen.mongo.convert.DocumentMapperConvert;
import org.bson.Document;

import java.lang.reflect.Field;

/**
 * Document类型处理
 *
 * @author JiaChaoYang
 **/
public class DocumentTypeFieldMapper<T> implements DocumentFieldMapper<T> {

    private final Object fieldValue;

    public DocumentTypeFieldMapper(Object fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public void mapField(Document doc, Field field, T obj) throws IllegalAccessException {
        field.set(obj,field.getType().equals(Document.class) ? fieldValue : DocumentMapperConvert.mapDocument((Document) fieldValue,field.getType()));
    }
}
