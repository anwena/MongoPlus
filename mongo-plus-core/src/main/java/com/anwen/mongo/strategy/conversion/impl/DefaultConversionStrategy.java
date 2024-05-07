package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.mapping.SimpleTypeHolder;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import org.bson.Document;

/**
 * 无策略格式数据
 *
 * @author JiaChaoYang
 **/
public class DefaultConversionStrategy implements ConversionStrategy<Object> {

    @Override
    public Object convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        if (!new SimpleTypeHolder().isSimpleType(fieldType) && fieldValue.getClass().equals(Document.class)){
            return mongoConverter.readInternal((Document) fieldValue,fieldType);
        }

        return fieldValue;
    }
}
