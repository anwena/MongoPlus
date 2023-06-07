package com.anwen.mongo.utils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @Description: 获取对象中的所有字段类型
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.utils
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-06 23:34
 * @Version: 1.0
 */
public class ClassTypeUtil {

    // 内部缓存，存储已经处理过的对象类型及其字段的类型
    private static Map<Class<?>, List<Class<?>>> cacheMap = new ConcurrentHashMap<>();

    private static volatile ClassTypeUtil instance;

    private ClassTypeUtil() {
    }

    public static ClassTypeUtil getInstance() {
        if (instance == null) {
            synchronized (ClassTypeUtil.class) {
                if (instance == null) {
                    instance = new ClassTypeUtil();
                }
            }
        }
        return instance;
    }

    /**
     * 获取对象的所有字段类型
     * @param clazz 待获取类型字段的class
     * @return 对象的所有字段类型列表
     */
    public static synchronized List<Class<?>> getAllFieldClasses(Class<?> clazz) {
        // 获取对象类型
        // 查找缓存中是否已有该类型对象的记录
        if (cacheMap.containsKey(clazz)) {
            // 如果已有记录，直接返回缓存中存储的结果
            return cacheMap.get(clazz);
        }
        // 如果缓存中没有记录，则使用反射获取对象类型及其所有字段的类型
        List<Class<?>> classList = new ArrayList<>();
        Field[] declaredFields = clazz.getDeclaredFields();
        for (Field field : declaredFields) {
            Class<?> fieldType = field.getType();
            if (fieldType.isArray()) {
                // 如果字段类型为数组，获取数组元素类型并添加到列表中
                classList.add(fieldType.getComponentType());
            } else if (Collection.class.isAssignableFrom(fieldType)) {
                // 如果字段类型为集合，则获取集合元素的类型并添加到列表中
                Type genericType = field.getGenericType();
                if (genericType instanceof ParameterizedType) {
                    ParameterizedType parameterizedType = (ParameterizedType) genericType;
                    classList.add((Class<?>) parameterizedType.getActualTypeArguments()[0]);
                }
            } else {
                // 如果字段类型为普通类型，则直接添加到列表中
                classList.add(fieldType);
            }
        }
        // 将结果存储到缓存中
        cacheMap.put(clazz, classList);
        // 返回结果
        return classList;
    }

    /**
     * 获取对象的所有自定义类字段类型
     * @param clazz 待获取类型字段的class
     * @return 对象的所有字段类型列表
     */
    public static synchronized List<Class<?>> getAllCustomFieldClasses(Class<?> clazz){
        List<Class<?>> result = new ArrayList<>();
        List<Class<?>> fieldClasses = getAllFieldClasses(clazz);
        fieldClasses.parallelStream().forEach(field -> {
            if (CustomClassUtil.isCustomObject(field)){
                result.add(field);
            }
        });
        return result;
    }

}

