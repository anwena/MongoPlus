package com.anwen.mongo.toolkit;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.model.BaseModelID;
import com.anwen.mongo.model.ClassAnnotationFiled;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.*;

/**
 * @author JiaChaoYang
 * 注解操作
 * @since 2023-02-13 13:59
 **/
public class AnnotationUtil {

    /**
     * 从类的所有字段上获取注解
     *
     * @param beanClass       类的class
     * @param annotationClass 要获取的注解class
     * @return 逻辑删除的字段信息
     */
    public static <T extends Annotation> ClassAnnotationFiled<T> getAnnotationOnFiled(Class<?> beanClass, Class<T> annotationClass) {
        for (Field field : beanClass.getDeclaredFields()) {
            T annotation = getAnnotation(field, annotationClass);
            if (Objects.nonNull(annotation)) {
                return new ClassAnnotationFiled<>(beanClass, field, annotation);
            }
        }
        Class<?> superclass = beanClass.getSuperclass();
        if (!Objects.equals(superclass, Object.class)) {
            return getAnnotationOnFiled(superclass, annotationClass);
        }
        return null;
    }

    /**
     * 从字段上获取注解
     *
     * @param field           字段
     * @param annotationClass 要获取的注解class
     * @param <T>             具体注解
     * @return 注解
     */
    public static <T extends Annotation> T getAnnotation(Field field, Class<T> annotationClass) {
        return findFirstAnnotation(annotationClass, field);
    }

    public static Map<String, Object> getFieldAnnotation(Object object) {
        Field[] fields = object.getClass().getDeclaredFields();
        Map<String, Object> resultMap = new HashMap<>();
        Class<?> superclass = object.getClass().getSuperclass();
        IdTypeEnum idTypeEnum = null;
        String fieldName = "id";
        Class<?> fieldType = String.class;
        for (Field field : fields) {
            if (field.isAnnotationPresent(ID.class)) {
                idTypeEnum = field.getAnnotation(ID.class).type();
                fieldName = field.getName();
                fieldType = field.getType();
            }
        }
        if (superclass == BaseModelID.class) {
            try {
                idTypeEnum = superclass.getField("id").getAnnotation(ID.class).type();
            } catch (NoSuchFieldException e) {
                throw new RuntimeException(e);
            }
        }
        if (idTypeEnum != null) {
            resultMap.put("fieldName", fieldName);
            resultMap.put("fieldType", fieldType);
            resultMap.put("generateType", idTypeEnum);
        }
        return resultMap;
    }

    public static <T extends Annotation> T findFirstAnnotation(Class<T> annotationClazz, Field field) {
        return getAnnotation(annotationClazz, new HashSet<>(), field.getDeclaredAnnotations());
    }

    @SuppressWarnings("unchecked")
    private static <T extends Annotation> T getAnnotation(Class<T> annotationClazz, Set<Class<? extends Annotation>> annotationSet, Annotation... annotations) {
        for (Annotation annotation : annotations) {
            if (annotationSet.add(annotation.annotationType())) {
                if (annotationClazz.isAssignableFrom(annotation.annotationType())) {
                    return (T) annotation;
                }
                annotation = getAnnotation(annotationClazz, annotationSet, annotation.annotationType().getDeclaredAnnotations());
                if (annotation != null) {
                    return (T) annotation;
                }
            }
        }
        return null;
    }

    public static String collectionName(Class<?> clazz){
        String collectionName = clazz.getName();
        CollectionName annotation = clazz.getAnnotation(CollectionName.class);
        if (annotation != null && StringUtils.isNotBlank(annotation.value())){
            collectionName = annotation.value();
        }
        return collectionName;
    }

}
