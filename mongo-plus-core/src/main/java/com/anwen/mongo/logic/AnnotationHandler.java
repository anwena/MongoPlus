package com.anwen.mongo.logic;


import com.anwen.mongo.model.ClassAnnotationFiled;
import com.anwen.mongo.toolkit.AnnotationUtil;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.Objects;

/**
 * 注解处理类
 *
 * @author loser
 * @date 2024/4/29
 */
public interface AnnotationHandler {

    /**
     * 从类的所有字段上获取注解
     *
     * @param beanClass       类的class
     * @param annotationClass 要获取的注解class
     * @return 逻辑删除的字段信息
     */
    static <T extends Annotation> ClassAnnotationFiled<T> getAnnotationOnFiled(Class<?> beanClass, Class<T> annotationClass) {
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
    static <T extends Annotation> T getAnnotation(Field field, Class<T> annotationClass) {
        return AnnotationUtil.findFirstAnnotation(annotationClass, field);
    }

}
