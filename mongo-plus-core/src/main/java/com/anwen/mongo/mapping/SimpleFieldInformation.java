package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.model.BaseModelID;
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

    private Object value;

    private String name;

    private Class<?> mapValueType;

    private Class<?> collectionValueType = Object.class;

    private final Field field;

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

    private final T instance;

    public SimpleFieldInformation(T instance, Field field) {
        this.instance = instance;
        field.setAccessible(true);
        this.field = field;
        this.type = field.getType();
    }

    @Override
    public Object getValue() {
        if (this.value == null){
            try {
                this.value = field.get(instance);
            } catch (IllegalAccessException e) {
                throw new RuntimeException(e);
            }
        }
        return this.value;
    }

    @Override
    public String getName() {
        if (this.name == null){
            this.name = field.getName();
            if (getCollectionField() != null && StringUtils.isNotBlank(getCollectionField().value())){
                this.name = getCollectionField().value();
            }
        }
        return this.name;
    }

    @Override
    public boolean isMap(){
        return Map.class.isAssignableFrom(type);
    }

    @Override
    public Class<?> mapValueType(){
        if (isMap() && this.mapValueType == null) {
            Type[] typeArguments = ((ParameterizedType) field.getGenericType()).getActualTypeArguments();
            this.mapValueType = (Class<?>) typeArguments[1];
        }
        return this.mapValueType;
    }

    @Override
    public Class<?> collectionValueType() {
        if (isCollection() && this.collectionValueType == null){
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
        return type.isArray() //
                || Iterable.class.equals(type) //
                || Collection.class.isAssignableFrom(type);
    }

    @Override
    public boolean isSimpleType(){
        return new SimpleTypeHolder().isSimpleType(type);
    }

    @Override
    public boolean isSkipCheckField() {
        return getCollectionField() != null && !getCollectionField().exist();
    }

    @Override
    public boolean isSkipCheckFieldAndId() {
        return isSkipCheckField() || isId();
    }

    @Override
    public boolean isId() {
        return getId() != null;
    }

    @Override
    public ID getId() {
        if (this.id == null){
            this.id = field.getAnnotation(ID.class);
        }
        return this.id;
    }

    @Override
    public CollectionField getCollectionField() {
        if (this.collectionField == null){
            CollectionField collectionField = field.getAnnotation(CollectionField.class);
            if (collectionField != null) {
                this.collectionField = collectionField;
            }
        }
        return this.collectionField;
    }


}
