package com.anwen.mongo.sql.support;

import com.anwen.mongo.utils.StringUtils;
import lombok.SneakyThrows;

import java.io.Serializable;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;
import java.util.function.Function;

/**
 * 支持序列化的Function
 * @author JiaChaoYang
 * @since 2023/2/14 14:18
*/
@FunctionalInterface
public interface SFunction<T,R> extends Function<T,R>, Serializable {

    @SneakyThrows
    default String getFieldName() {
        String methodName = getMethodName();
        if (methodName.startsWith("get")) {
            methodName = methodName.substring(3);
        }
        return StringUtils.firstToLowerCase(methodName);
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

    @SneakyThrows
    default Class<?> getReturnType() {
        SerializedLambda lambda = getSerializedLambda();
        Class<?> className = Class.forName(lambda.getImplClass().replace("/", "."));
        Method method = className.getMethod(getMethodName());
        return method.getReturnType();
    }

}
