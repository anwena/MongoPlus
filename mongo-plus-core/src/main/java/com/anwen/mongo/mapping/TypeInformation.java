package com.anwen.mongo.mapping;

import com.anwen.mongo.toolkit.ClassTypeUtil;

import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.List;

public interface TypeInformation {

    static TypeInformation of(Class<?> clazz){
        return of(ClassTypeUtil.getInstanceByClass(clazz));
    }

    static TypeInformation of(Object instance){
        return SimpleTypeInformation.of(instance);
    }

    <T> T getInstance();

    void setInstance(Object instance);

    List<FieldInformation> getFields();

    FieldInformation getField(String fieldName);

    List<FieldInformation> getAnnotationFields(Class<? extends Annotation> annotationClass);

    FieldInformation getAnnotationField(Class<? extends Annotation> annotationClass,String nullMessage);

    FieldInformation getAnnotationField(Class<? extends Annotation> annotationClass);

    Object getAnnotationFieldValue(Class<? extends Annotation> annotationClass,String nullMessage);

    Class<?> getClazz();

    Boolean isMap();

    Boolean isCollection();

    Boolean isSimpleType();

    Type[] getType();

}
