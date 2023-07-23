package com.anwen.mongo.sql.support;

import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.utils.StringUtils;
import lombok.SneakyThrows;

import java.io.Serializable;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.function.Function;

/**
 * 支持序列化的Function
 * @author JiaChaoYang
 * @since 2023/2/14 14:18
*/
@FunctionalInterface
public interface SFunction<T,R> extends Function<T,R>, Serializable {

    //默认配置
    String defaultSplit = "";
    Integer defaultToType = 0;

    @SneakyThrows
    default String getFieldName() {
        String methodName = getMethodName();
        if (methodName.startsWith("get")) {
            methodName = methodName.substring(3);
        }
        return StringUtils.firstToLowerCase(methodName);
    }

    /**
     * 获取实体类的字段名称(实体声明的字段名称)
     */
    default String getFieldNameLine() {
        return getFieldName(this, defaultSplit);
    }

    /**
     * 获取实体类的字段名称
     *
     * @param split 分隔符，多个字母自定义分隔符
     */
    default String getFieldName(SFunction<T, ?> fn, String split) {
        return getFieldName(fn, split, defaultToType);
    }

    /**
     * 获取实体类的字段名称
     *
     * @param split  分隔符，多个字母自定义分隔符
     * @param toType 转换方式，多个字母以大小写方式返回 0.不做转换 1.大写 2.小写
     */
    default String getFieldName(SFunction<T, ?> fn, String split, Integer toType) {
        SerializedLambda serializedLambda = getSerializedLambdaOne(fn);

        // 从lambda信息取出method、field、class等
        String fieldName = serializedLambda.getImplMethodName().substring("get".length());
        fieldName = fieldName.replaceFirst(fieldName.charAt(0) + "", (fieldName.charAt(0) + "").toLowerCase());
        Field field;
        try {
            field = Class.forName(serializedLambda.getImplClass().replace("/", ".")).getDeclaredField(fieldName);
        } catch (ClassNotFoundException | NoSuchFieldException e) {
            throw new RuntimeException(e);
        }

        // 从field取出字段名，可以根据实际情况调整
        CollectionField collectionField = field.getAnnotation(CollectionField.class);
        if (collectionField != null && collectionField.value().length() > 0) {
            return collectionField.value();
        } else {
            //0.不做转换 1.大写 2.小写
            switch (toType) {
                case 1:
                    return fieldName.replaceAll("[A-Z]", split + "$0").toUpperCase();
                case 2:
                    return fieldName.replaceAll("[A-Z]", split + "$0").toLowerCase();
                default:
                    return fieldName.replaceAll("[A-Z]", split + "$0");
            }

        }

    }


    @SneakyThrows
    default String getMethodName() {
        return getSerializedLambda().getImplMethodName();
    }

    @SneakyThrows
    default Class<?> getFieldClass() {
        return getReturnType();
    }

    @SneakyThrows
    default SerializedLambda getSerializedLambda() {
        Method method = getClass().getDeclaredMethod("writeReplace");
        method.setAccessible(true);
        return (SerializedLambda) method.invoke(this);
    }

    default SerializedLambda getSerializedLambdaOne(SFunction<T, ?> fn) {
        // 从function取出序列化方法
        Method writeReplaceMethod;
        try {
            writeReplaceMethod = fn.getClass().getDeclaredMethod("writeReplace");
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }

        // 从序列化方法取出序列化的lambda信息
        boolean isAccessible = writeReplaceMethod.isAccessible();
        writeReplaceMethod.setAccessible(true);
        SerializedLambda serializedLambda;
        try {
            serializedLambda = (SerializedLambda) writeReplaceMethod.invoke(fn);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
        writeReplaceMethod.setAccessible(isAccessible);
        return serializedLambda;
    }

    @SneakyThrows
    default Class<?> getReturnType() {
        SerializedLambda lambda = getSerializedLambda();
        Class<?> className = Class.forName(lambda.getImplClass().replace("/", "."));
        Method method = className.getMethod(getMethodName());
        return method.getReturnType();
    }

}
