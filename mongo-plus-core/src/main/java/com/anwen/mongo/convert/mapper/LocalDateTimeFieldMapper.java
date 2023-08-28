package com.anwen.mongo.convert.mapper;

import com.anwen.mongo.convert.DocumentFieldMapper;
import org.bson.Document;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Date;

public class LocalDateTimeFieldMapper<T> implements DocumentFieldMapper<T> {

    private final Object fieldValue;

    public LocalDateTimeFieldMapper(Object fieldValue) {
        this.fieldValue = fieldValue;
    }

    @Override
    public void mapField(Document doc, Field field, T obj) throws IllegalAccessException {
        // TODO: 处理LocalDateTime类型字段的转换逻辑
        if (fieldValue instanceof Date) {
            field.set(obj, LocalDateTime.ofInstant(((Date)fieldValue).toInstant(), ZoneOffset.UTC).atZone(ZoneId.of("Asia/Shanghai")).toLocalDateTime());
        }
    }
}

