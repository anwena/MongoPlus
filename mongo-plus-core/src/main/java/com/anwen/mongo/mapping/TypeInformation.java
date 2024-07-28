package com.anwen.mongo.mapping;

import com.anwen.mongo.domain.MongoPlusFieldException;
import com.anwen.mongo.toolkit.ClassTypeUtil;

import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.List;

/**
 * 对Class进行的简单封装，调用请求更便捷
 * @author anwen
 */
public interface TypeInformation {

    /**
     * 根据Class构建一个TypeInformation
     * @param clazz 类
     * @return {@link TypeInformation}
     * @author anwen
     * @date 2024/7/28 下午11:53
     */
    static TypeInformation of(Class<?> clazz){
        return of(ClassTypeUtil.getInstanceByClass(clazz));
    }

    /**
     * 根据实例构建一个TypeInformation
     * @param instance 实例
     * @return {@link com.anwen.mongo.mapping.TypeInformation}
     * @author anwen
     * @date 2024/7/28 下午11:53
     */
    static TypeInformation of(Object instance){
        return SimpleTypeInformation.of(instance);
    }

    /**
     * 获取实例
     * @author anwen
     * @date 2024/7/28 下午11:51
     */
    <T> T getInstance();

    /**
     * 设置实例
     * @param instance 实例
     * @author anwen
     * @date 2024/7/28 下午11:52
     */
    void setInstance(Object instance);

    /**
     * 获取所有字段，并封装为FieldInformation
     * @author anwen
     * @date 2024/7/28 下午11:52
     */
    List<FieldInformation> getFields();

    /**
     * 根据名称获取一个字段
     * @param fieldName 字段名
     * @return {@link com.anwen.mongo.mapping.FieldInformation}
     * @author anwen
     * @date 2024/7/28 下午11:52
     */
    FieldInformation getField(String fieldName);

    /**
     * 根据注解获取字段
     * @param annotationClass 注解类
     * @return {@link java.util.List<com.anwen.mongo.mapping.FieldInformation>}
     * @author anwen
     * @date 2024/7/28 下午11:53
     */
    List<FieldInformation> getAnnotationFields(Class<? extends Annotation> annotationClass);

    /**
     * 根据注解获取一个字段，不存在则抛出{@link MongoPlusFieldException}异常
     * @param annotationClass 注解类
     * @param nullMessage 异常信息
     * @return {@link FieldInformation}
     * @author anwen
     * @date 2024/7/28 下午11:54
     */
    FieldInformation getAnnotationField(Class<? extends Annotation> annotationClass,String nullMessage);

    /**
     * 根据注解获取一个字段，不存在则返回null
     * @param annotationClass 注解类
     * @return {@link com.anwen.mongo.mapping.FieldInformation}
     * @author anwen
     * @date 2024/7/28 下午11:54
     */
    FieldInformation getAnnotationField(Class<? extends Annotation> annotationClass);

    /**
     * 获取注解字段的字段值，不存在则抛出{@link MongoPlusFieldException}异常
     * @param annotationClass 注解类
     * @param nullMessage 异常信息
     * @return {@link Object}
     * @author anwen
     * @date 2024/7/28 下午11:55
     */
    Object getAnnotationFieldValue(Class<? extends Annotation> annotationClass,String nullMessage);

    /**
     * 获取注解字段的字段值，不存在则返回null
     * @param annotationClass 注解类
     * @return {@link Object}
     * @author anwen
     * @date 2024/7/28 下午11:55
     */
    Object getAnnotationFieldValue(Class<? extends Annotation> annotationClass);

    /**
     * 获取原始Class
     * @return {@link Class<?>}
     * @author anwen
     * @date 2024/7/28 下午11:56
     */
    Class<?> getClazz();

    /**
     * 是否是map
     * @return {@link Boolean}
     * @author anwen
     * @date 2024/7/28 下午11:57
     */
    Boolean isMap();

    /**
     * 是否是集合
     * @return {@link java.lang.Boolean}
     * @author anwen
     * @date 2024/7/28 下午11:57
     */
    Boolean isCollection();

    /**
     * 是否是简单类型
     * @return {@link java.lang.Boolean}
     * @author anwen
     * @date 2024/7/28 下午11:57
     */
    Boolean isSimpleType();

    /**
     * 获取Type
     * @return {@link Type[]}
     * @author anwen
     * @date 2024/7/28 下午11:57
     */
    Type[] getType();

}
