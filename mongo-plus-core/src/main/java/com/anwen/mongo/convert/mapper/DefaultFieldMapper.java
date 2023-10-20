package com.anwen.mongo.convert.mapper;

import com.anwen.mongo.convert.DocumentFieldMapper;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.mongodb.MongoException;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

/**
 * 默认字段映射器
 *
 * @author JiaChaoYang
 **/
public class DefaultFieldMapper<T> implements DocumentFieldMapper<T> {

    Logger logger = LoggerFactory.getLogger(DefaultFieldMapper.class);

    private final Object fieldValue;

    public DefaultFieldMapper(Object fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public void mapField(Document doc, Field field, T obj) {
        try {
            ConversionService.convertValue(field,obj,fieldValue);
        }catch (Exception e){
            logger.error("convert error,message: {}",e.getMessage(),e);
            throw new MongoException("Database field and entity class field types do not match : "+field.getName());
        }
    }
}
