package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.convert.DocumentMapperConvert;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.CustomClassUtil;
import org.bson.Document;

import java.lang.reflect.Field;

/**
 * 无策略格式数据
 *
 * @author JiaChaoYang
 **/
public class DefaultConversionStrategy implements ConversionStrategy<Object> {
    @Override
    public Object convertValue(Field field, Object obj, Object fieldValue) throws IllegalAccessException {
        if (CustomClassUtil.isCustomObject(field.getType()) && fieldValue.getClass().equals(Document.class)){
            return DocumentMapperConvert.mapDocument((Document) fieldValue,field.getType(),false);
        }
        return fieldValue;
    }
}
