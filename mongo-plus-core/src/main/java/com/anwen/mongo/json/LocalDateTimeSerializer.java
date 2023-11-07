package com.anwen.mongo.json;

import com.alibaba.fastjson.serializer.JSONSerializer;
import com.alibaba.fastjson.serializer.ObjectSerializer;
import com.alibaba.fastjson.serializer.PropertyFilter;
import com.anwen.mongo.strategy.convert.impl.DateConversionStrategy;
import org.bson.BsonDateTime;

import java.io.IOException;
import java.lang.reflect.Type;

/**
 * @author JiaChaoYang
 **/
public class LocalDateTimeSerializer implements ObjectSerializer {

    @Override
    public void write(JSONSerializer jsonSerializer, Object o, Object o1, Type type, int i) throws IOException {
        System.out.println("进来了进来了");

    }
}
