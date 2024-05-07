package com.anwen.mongo.mapping;

import com.anwen.mongo.cache.global.ClassInformationCache;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Optional;

public interface TypeInformation {

    static TypeInformation of(Class<?> clazz){
        try {
            return of(clazz.getDeclaredConstructor().newInstance());
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    static TypeInformation of(Object instance){
        Class<?> clazz = instance.getClass();
        return Optional.ofNullable(ClassInformationCache.classInformationMap.get(clazz)).orElseGet(() -> {
            SimpleTypeInformation<?> simpleClassInformation = new SimpleTypeInformation<>(instance);
            ClassInformationCache.classInformationMap.put(clazz, simpleClassInformation);
            return simpleClassInformation;
        });
    }

    <T> T getInstance();

    List<FieldInformation> getFields();

    List<FieldInformation> getAnnotationFields(Class<? extends Annotation> annotationClass);

    FieldInformation getAnnotationField(Class<? extends Annotation> annotationClass,String nullMessage);

    Object getAnnotationFieldValue(Class<? extends Annotation> annotationClass,String nullMessage);

    Class<?> getClazz();

    Boolean isMap();

    Boolean isCollection();

    Boolean isSimpleType();

    Type[] getType();

}
