package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.toolkit.StringUtils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class SimpleFieldInformation<T> implements FieldInformation {

    private final Object value;

    private String name;

    private final Boolean isMap;

    private Class<?> mapValueType;

    private Class<?> collectionValueType = Object.class;

    private final Boolean isCollection;

    private final Field field;

    private final Boolean isSkipCheckField;

    private Boolean isId = false;

    private ID id;

    private CollectionField collectionField;

    @Override
    public Field getField() {
        return field;
    }

    @Override
    public Class<?> getType() {
        return type;
    }

    private final Class<?> type;

    public SimpleFieldInformation(T instance, Field field) {
        field.setAccessible(true);
        this.field = field;
        this.type = field.getType();
        try {
            this.value = field.get(instance);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        this.name = field.getName();
        CollectionField collectionField = field.getAnnotation(CollectionField.class);
        if (collectionField != null && StringUtils.isNotBlank(collectionField.value())) {
            this.name = collectionField.value();
            this.collectionField = collectionField;
        }
        this.isMap = Map.class.isAssignableFrom(type);
        this.isCollection = type.isArray() //
                || Iterable.class.equals(type) //
                || Collection.class.isAssignableFrom(type);
        this.isSkipCheckField = collectionField != null && !collectionField.exist();
        ID IDAnnotation = field.getAnnotation(ID.class);
        if (IDAnnotation != null){
            this.isId = true;
            this.id = IDAnnotation;
        }
    }

    @Override
    public Object getValue() {
        return this.value;
    }

    @Override
    public String getName() {
        return this.name;
    }

    @Override
    public boolean isMap(){
        return this.isMap;
    }

    @Override
    public Class<?> mapValueType(){
        if (this.isMap && this.mapValueType == null) {
            Type[] typeArguments = ((ParameterizedType) field.getGenericType()).getActualTypeArguments();
            this.mapValueType = (Class<?>) typeArguments[1];
        }
        return this.mapValueType;
    }

    @Override
    public Class<?> collectionValueType() {
        if (this.isCollection && this.collectionValueType == null){
            Type genericType = field.getGenericType();
            if (genericType instanceof ParameterizedType) {
                ParameterizedType parameterizedType = (ParameterizedType) genericType;
                Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
                if (actualTypeArguments.length > 0 && actualTypeArguments[0] instanceof Class) {
                    this.collectionValueType = (Class<?>) actualTypeArguments[0];
                }
            }
        }
        return this.collectionValueType;
    }

    @Override
    public boolean isCollection(){
        return this.isCollection;
    }

    @Override
    public boolean isSimpleType(){
        return new SimpleTypeHolder().isSimpleType(type);
    }

    @Override
    public boolean isSkipCheckField() {
        return isSkipCheckField;
    }

    @Override
    public boolean isId() {
        return isId;
    }

    @Override
    public ID getId() {
        return id;
    }

    @Override
    public CollectionField getCollectionField() {
        return this.collectionField;
    }


}
