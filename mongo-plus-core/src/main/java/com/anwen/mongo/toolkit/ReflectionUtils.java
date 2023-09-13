package com.anwen.mongo.toolkit;

import java.lang.reflect.Field;

public class ReflectionUtils {

    private ReflectionUtils() {
    }


    public static Object getFieldValue(Object target, Field field) {
        try {
            return field.get(target);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    public static void setFieldValue(Object target, Field field, Object fieldValue) {
        try {
            field.set(target, fieldValue);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }
}
