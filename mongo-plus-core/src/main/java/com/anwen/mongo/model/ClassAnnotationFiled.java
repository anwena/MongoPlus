package com.anwen.mongo.model;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;

/**
 * 目标类首个指定注解结果包裹对象
 *
 * @author loser
 * @date 2024/4/28
 */
public class ClassAnnotationFiled<T extends Annotation> {

    private Class<?> target;

    private Field field;

    private T targetAnnotation;

    public ClassAnnotationFiled(Class<?> target, Field field, T tableLogic) {
        this.target = target;
        this.field = field;
        this.targetAnnotation = tableLogic;
    }

    public Class<?> getTarget() {
        return target;
    }

    public void setTarget(Class<?> target) {
        this.target = target;
    }

    public Field getField() {
        return field;
    }

    public void setField(Field field) {
        this.field = field;
    }

    public T getTargetAnnotation() {
        return targetAnnotation;
    }

    public void setTargetAnnotation(T targetAnnotation) {
        this.targetAnnotation = targetAnnotation;
    }
}
