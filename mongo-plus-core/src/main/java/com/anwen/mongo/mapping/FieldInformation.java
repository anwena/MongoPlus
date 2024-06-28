package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

/**
 * @author JiaChaoYang
 **/
public interface FieldInformation {

    Object getValue();

    String getName();

    String getCamelCaseName();

    String getIdOrCamelCaseName();

    boolean isMap();

    Field getField();

    Class<?> getTypeClass();

    Type[] getType();

    TypeInformation getTypeInformation();

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

    void setValue(Object value);

    CollectionField getCollectionField();

    Annotation getAnnotation(Class<? extends Annotation> annotationClass);

    Type getGenericType();

    void setGenericType(Type type);

}
