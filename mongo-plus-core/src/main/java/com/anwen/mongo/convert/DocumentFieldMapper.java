package com.anwen.mongo.convert;

import org.bson.Document;

import java.lang.reflect.Field;

public interface DocumentFieldMapper<T> {
    void mapField(Document doc, Field field, T obj) throws IllegalAccessException;
}
