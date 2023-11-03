package com.anwen.mongo.strategy.convert.impl;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.strategy.convert.ConversionStrategy;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description Collection策略实现类
 * @date 2023-11-02 15:55
 **/
public class CollectionConversionStrategy implements ConversionStrategy<Collection<?>> {

    @Override
    public Collection<?> convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        if (!(fieldValue instanceof Collection<?>)){
            CollectionField collectionField = field.getAnnotation(CollectionField.class);
            if (collectionField != null && collectionField.convertCollect()) {
                Object finalFieldValue = fieldValue;
                fieldValue = new ArrayList<Object>() {{
                    add(finalFieldValue);
                }};
            }
        }
        return JSON.parseArray(JSON.toJSONString(fieldValue),getListGenericType(field));
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
