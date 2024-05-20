package com.anwen.mongo.strategy.conversion.impl;

import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import org.bson.Document;

/**
 * Document转换策略
 * @author anwen
 * @date 2024/5/20 下午10:17
 */
public class DocumentConversionStrategy implements ConversionStrategy<Document> {
    @Override
    public Document convertValue(Object fieldValue, Class<?> fieldType, MongoConverter mongoConverter) throws IllegalAccessException {
        return (Document) fieldValue;
    }
}
