package com.anwen.mongo.json;

import com.alibaba.fastjson.serializer.JSONSerializer;
import com.alibaba.fastjson.serializer.ObjectSerializer;
import com.anwen.mongo.strategy.convert.impl.DateConversionStrategy;
import org.bson.BsonDateTime;

import java.io.IOException;
import java.lang.reflect.Type;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

/**
 * @author JiaChaoYang
 **/
public class LocalDateTimeSerializer implements ObjectSerializer {

    @Override
    public void write(JSONSerializer jsonSerializer, Object o, Object o1, Type type, int i) throws IOException {
        System.out.println("进来了进来了");
        DateConversionStrategy dateConversionStrategy = new DateConversionStrategy();
        jsonSerializer.write(new BsonDateTime(((LocalDateTime) o).toEpochSecond(ZoneOffset.UTC) * 1000L));
    }
}
