package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.mongodb.MongoException;
import org.bson.Document;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description Map类型策略实现类
 * @date 2023-11-02 16:11
 **/
public class MapConversionStrategy implements ConversionStrategy<Map<?,?>> {

    @Override
    @SuppressWarnings("unchecked")
    public Map<?,?> convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        if (!fieldValue.getClass().equals(Document.class)){
            throw new MongoException("FieldValue Type Not Is Document");
        }
        Type[] typeArguments = ((ParameterizedType) field.getGenericType()).getActualTypeArguments();
        Class<?> valueClazz = (Class<?>) typeArguments[1];
        if (valueClazz.equals(Object.class)){
            return (Document) fieldValue;
        }else {
            Document document = (Document) fieldValue;
            Map map;
            if (field.getType().equals(Map.class)){
                map = new HashMap();
            }else {
                try {
                    map = (Map) field.getType().getDeclaredConstructor().newInstance();
                } catch (InstantiationException | InvocationTargetException | NoSuchMethodException e) {
                    throw new RuntimeException(e);
                }
            }
            for (String key : document.keySet()) {
                Object value = document.get(key);
                if (value.getClass().equals(Document.class)){
                    map.putAll(convertDocumentToMap(field,obj,value));
                }else {
                    map.put(key,ConversionService.convertValue(field, obj, value, valueClazz));
                }
            }
            return map;
        }
    }

    private Map<?,?> convertDocumentToMap(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        return convertValue(field, obj, fieldValue);
    }
}
