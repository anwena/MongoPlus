package com.anwen.mongo.convert;

public interface CollectionNameConvert {

    <T> String convert(Class<T> entityClass);

}
