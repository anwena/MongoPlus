package com.anwen.mongo.convert.mapper;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.convert.DocumentFieldMapper;
import com.mongodb.MongoException;
import org.bson.Document;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * 集合字段
 *
 * @author JiaChaoYang
 **/
public class CollectionFieldMapper<T> implements DocumentFieldMapper<T> {

    private Object fieldValue;

    public CollectionFieldMapper(Object fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public void mapField(Document doc, Field field, T obj) throws IllegalAccessException {
        if (!(fieldValue instanceof Collection<?>)){
            CollectionField collectionField = field.getAnnotation(CollectionField.class);
            if (collectionField != null && collectionField.convertCollect()) {
                fieldValue = new ArrayList<Object>() {{
                    add(fieldValue);
                }};
            }else {
                return;
            }
        }
        field.set(obj, JSON.parseArray(JSON.toJSONString(fieldValue),getListGenericType(field)));
    }

    public Class<?> getListGenericType(Field field) {
        Type genericType = field.getGenericType();
        if (genericType instanceof ParameterizedType) {
            ParameterizedType parameterizedType = (ParameterizedType) genericType;
            Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
            if (actualTypeArguments.length > 0 && actualTypeArguments[0] instanceof Class) {
                return (Class<?>) actualTypeArguments[0];
            }
        }
        return Object.class;
    }

}
