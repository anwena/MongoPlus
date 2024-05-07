package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import org.bson.Document;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description Collection策略实现类
 * @date 2023-11-02 15:55
 **/
public class CollectionConversionStrategy implements ConversionStrategy<Collection<?>> {

    @Override
    @SuppressWarnings({"unchecked","rawtypes"})
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
        Class<?> fieldType = field.getType();
        Collection<Object> arrayList;
        try {
            arrayList = fieldType.isAssignableFrom(ArrayList.class) ? new ArrayList<>() : (Collection<Object>) fieldType.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | InvocationTargetException | NoSuchMethodException e) {
            throw new MongoPlusConvertException("Failed to create Collection instance",e);
        }
        if (fieldValue instanceof Collection) {
            ((ArrayList) fieldValue).forEach(value -> {
                arrayList.add(DocumentMapperConvert.mapDocument((Document) value,ClassTypeUtil.getListGenericType(field),false));
            });
        } else {
            arrayList.add(DocumentMapperConvert.mapDocument((Document) fieldValue,ClassTypeUtil.getListGenericType(field),false));
        }
        return arrayList/*JSON.parseArray(JSON.toJSONString(fieldValue), ClassTypeUtil.getListGenericType(field))*/;
    }
}
