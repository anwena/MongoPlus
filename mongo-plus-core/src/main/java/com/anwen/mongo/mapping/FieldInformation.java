package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

/**
 * 对Field的简单封装
 * @author anwen
 **/
public interface FieldInformation {

    /**
     * 获取字段值
     * @return {@link Object}
     * @author anwen
     * @date 2024/7/28 下午11:59
     */
    Object getValue();

    /**
     * 获取字段名，受{@link CollectionField}注解的影响
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/7/28 下午11:59
     */
    String getName();

    /**
     * 获取字段名，受驼峰转下划线配置的影响
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/7/29 上午12:01
     */
    String getCamelCaseName();

    /**
     * 获取id或字段名，受{@link ID}和驼峰转下划线配置的影响
     * @return {@link String}
     * @author anwen
     * @date 2024/7/29 上午12:01
     */
    String getIdOrCamelCaseName();

    /**
     * 是否是Map
     * @return {@link boolean}
     * @author anwen
     * @date 2024/7/29 上午12:01
     */
    boolean isMap();

    /**
     * 获取原始字段
     * @return {@link java.lang.reflect.Field}
     * @author anwen
     * @date 2024/7/29 上午12:01
     */
    Field getField();

    /**
     * 获取字段Type的Class
     * @return {@link java.lang.Class<?>}
     * @author anwen
     * @date 2024/7/29 上午12:02
     */
    Class<?> getTypeClass();

    /**
     * 获取字段的Type
     * @return {@link java.lang.reflect.Type[]}
     * @author anwen
     * @date 2024/7/29 上午12:02
     */
    Type[] getType();

    /**
     * 将字段封装为TypeInformation
     * @return {@link TypeInformation}
     * @author anwen
     * @date 2024/7/29 上午12:02
     */
    TypeInformation getTypeInformation();

    /**
     * 如果是Map，则获取Map的Value类型
     * @return {@link java.lang.Class<?>}
     * @author anwen
     * @date 2024/7/29 上午12:03
     */
    Class<?> mapValueType();

    /**
     * 如果是集合，则获取集合的泛型
     * @return {@link java.lang.Class<?>}
     * @author anwen
     * @date 2024/7/29 上午12:03
     */
    Class<?> collectionValueType();

    /**
     * 是否是集合
     * @return {@link boolean}
     * @author anwen
     * @date 2024/7/29 上午12:03
     */
    boolean isCollection();

    /**
     * 是否是简单类型
     * @return {@link boolean}
     * @author anwen
     * @date 2024/7/29 上午12:03
     */
    boolean isSimpleType();

    /**
     * 是否跳过检查
     * @return {@link boolean}
     * @author anwen
     * @date 2024/7/29 上午12:04
     */
    boolean isSkipCheckField();

    /**
     * exits和@ID字段则跳过
     * @return {@link boolean}
     * @author anwen
     * @date 2024/7/29 上午12:04
     */
    boolean isSkipCheckFieldAndId();

    /**
     * 是否是id字段
     * @return {@link boolean}
     * @author anwen
     * @date 2024/7/29 上午12:04
     */
    boolean isId();

    /**
     * 获取ID字段注解
     * @return {@link com.anwen.mongo.annotation.ID}
     * @author anwen
     * @date 2024/7/29 上午12:04
     */
    ID getId();

    /**
     * 获取字段get方法
     * @return {@link java.lang.reflect.Method}
     * @author anwen
     * @date 2024/7/29 上午12:05
     */
    Method getMethod();

    /**
     * 获取字段set方法
     * @return {@link java.lang.reflect.Method}
     * @author anwen
     * @date 2024/7/29 上午12:05
     */
    Method setMethod();

    /**
     * 设置值
     * @param value 值
     * @author anwen
     * @date 2024/7/29 上午12:05
     */
    void setValue(Object value);

    /**
     * 获取字段的{@link CollectionField} 注解
     * @return {@link com.anwen.mongo.annotation.collection.CollectionField}
     * @author anwen
     * @date 2024/7/29 上午12:05
     */
    CollectionField getCollectionField();

    /**
     * 获取指定注解
     * @param annotationClass 注解Class
     * @return {@link Annotation}
     * @author anwen
     * @date 2024/7/29 上午12:06
     */
    <T extends Annotation> T getAnnotation(Class<T> annotationClass);

    /**
     * 是否存在指定类型的注解
     * @param annotationClass 注解Class
     * @return {@link boolean}
     * @author anwen
     * @date 2024/7/29 上午12:08
     */
    boolean isAnnotation(Class<? extends Annotation> annotationClass);

    /**
     * 获取字段的genericType
     * @return {@link java.lang.reflect.Type}
     * @author anwen
     * @date 2024/7/29 上午12:09
     */
    Type getGenericType();

}
