package com.anwen.mongo.mapping;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.List;

public interface ClassInformation {

    List<FieldInformation> getFields();

    List<FieldInformation> getAnnotationFields(Class<? extends Annotation> annotationClass);

    FieldInformation getAnnotationField(Class<? extends Annotation> annotationClass,String nullMessage);

    Object getAnnotationFieldValue(Class<? extends Annotation> annotationClass,String nullMessage);

    Class<?> getClazz();

    Boolean isMap();

    Boolean isCollection();

}
