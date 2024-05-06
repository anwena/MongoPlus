package com.anwen.mongo.strategy.convert.impl;

import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import org.bson.Document;

import java.lang.reflect.Field;
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
            throw new MongoPlusConvertException("FieldValue Type Not Is Document");
        }
        Document document = (Document) fieldValue;
        Map map = new HashMap();
        document.forEach((k,v) -> {
            try {
                map.put(k,ConversionService.convertValue(field, obj, v, v.getClass()));
            } catch (IllegalAccessException e) {
                throw new MongoPlusConvertException("Exception occurred in converting internal elements");
            }
        });
        return map;
    }
}
