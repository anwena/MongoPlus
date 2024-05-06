package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * @author JiaChaoYang
 **/
public interface FieldInformation {

    Object getValue();

    String getName();

    boolean isMap();

    Field getField();

    Class<?> getType();

    Class<?> mapValueType();

    Class<?> collectionValueType();

    boolean isCollection();

    boolean isSimpleType();

    boolean isSkipCheckField();

    boolean isSkipCheckFieldAndId();

    boolean isId();

    ID getId();

    Method getMethod();

    Method setMethod();

    CollectionField getCollectionField();

}
