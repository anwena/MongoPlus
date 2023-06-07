package com.anwen.mongo.convert;

import java.lang.reflect.Field;

/**
 * @Description: 属性设置器，用于将值转换为指定类型并设置到目标对象的属性中
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.convert
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-07 01:06
 * @Version: 1.0
 */
public interface FieldSetter {
    /**
     * 设置属性值。
     *
     * @param target 目标对象
     * @param field  属性字段
     * @param value  源值
     * @throws IllegalAccessException 如果无法访问属性或属性类型不支持赋值操作
     */
    void setFieldValue(Object target, Field field, Object value) throws IllegalAccessException;
}
