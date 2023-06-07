package com.anwen.mongo.convert;

import com.anwen.mongo.annotation.table.TableField;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @Description: 将 Map 转换为 Java 实体类的工具类
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.convert
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-07 01:01
 * @Version: 1.0
 */
public class MapToObjectConverter<T> {
    private final Class<T> clazz;
    private final ObjectMappingStrategy strategy;
    private final Map<Class<?>, FieldSetter> fieldSetterCache = new HashMap<>();

    /**
     * 构造函数
     *
     * @param clazz 目标实体类的 Class 对象
     * @param strategy 映射策略
     */
    public MapToObjectConverter(Class<T> clazz, ObjectMappingStrategy strategy) {
        this.clazz = clazz;
        this.strategy = strategy;
    }

    /**
     * 将给定的 Map 转换为指定类型的实体类对象。
     *
     * @param map 要转换的 Map
     * @return 指定类型的实体类对象
     * @throws IllegalAccessException 如果无法访问某个属性
     */
    public T convert(Map<String, Object> map) throws IllegalAccessException {
        if (map == null) {
            return null;
        }
        T obj;
        try {
            obj = clazz.newInstance();
        } catch (InstantiationException e) {
            throw new IllegalArgumentException("Failed to instantiate class: " + clazz.getName(), e);
        }
        for (Field field : clazz.getDeclaredFields()) {
            TableField tableField = field.getAnnotation(TableField.class);
            String fieldName = tableField != null ? tableField.value() : field.getName();
            if (map.containsKey(fieldName)) {
                Object value = map.get(fieldName);
                if (value != null) {
                    field.setAccessible(true);
                    if (field.getType().isArray()) {
                        setArrayField(obj, field, value);
                    } else if (List.class.isAssignableFrom(field.getType())) {
                        setListField(obj, field, value);
                    } else {
                        FieldSetter fieldSetter = getFieldSetter(field);
                        Object mappedValue = strategy.map(value, field.getType());
                        fieldSetter.setFieldValue(obj, field, mappedValue);
                    }
                }
            }
        }
        return obj;
    }

    /**
     * 获取指定字段的 FieldSetter 对象。
     *
     * @param field 字段对象
     * @return FieldSetter 对象
     */
    private FieldSetter getFieldSetter(Field field) {
        Class<?> fieldType = field.getType();
        FieldSetter fieldSetter = fieldSetterCache.get(fieldType);
        if (fieldSetter == null) {
            String simpleName = fieldType.getSimpleName();
            fieldSetter = FieldSetterFactory.createSetter(simpleName);
            fieldSetterCache.put(fieldType, fieldSetter);
        }
        return fieldSetter;
    }

    /**
     * 设置目标对象中数组类型的属性值
     *
     * @param instance 目标对象
     * @param field 数组类型的属性
     * @param sourceValue 源值对象
     * @throws IllegalAccessException
     */
    private void setArrayField(Object instance, Field field, Object sourceValue) throws IllegalAccessException {
        Class<?> componentType = field.getType().getComponentType();
        int length = Array.getLength(sourceValue);
        Object array = Array.newInstance(componentType, length);
        for (int i = 0; i < length; i++) {
            Object item = MapToObjectConverter.this.convert((Map<String, Object>) Array.get(sourceValue, i));
            Array.set(array, i, item);
        }
        field.set(instance, array);
    }

    /**
     * 设置目标对象中 List 类型的属性值
     *
     * @param instance 目标对象
     * @param field List 类型的属性
     * @param sourceValue 源值对象
     * @throws IllegalAccessException
     */
    private void setListField(Object instance, Field field, Object sourceValue) throws IllegalAccessException {
        List<Object> list = new ArrayList<>();
        ParameterizedType genericType = (ParameterizedType) field.getGenericType();
        Class<?> componentType = (Class<?>) genericType.getActualTypeArguments()[0];
        for (Object item : (List<?>) sourceValue) {
            if (item != null) {
                Object convertedItem = MapToObjectConverter.this.convert((Map<String, Object>) item);
                list.add(convertedItem);
            }
        }
        field.set(instance, list);
    }
}
