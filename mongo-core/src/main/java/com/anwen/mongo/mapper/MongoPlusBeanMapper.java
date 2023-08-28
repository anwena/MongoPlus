package com.anwen.mongo.mapper;

import com.anwen.mongo.service.impl.ServiceImpl;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * @author JiaChaoYang
 **/
public class MongoPlusBeanMapper<T> extends ServiceImpl<T> {

    @Override
    public Class<T> getGenericityClazz() {
        Type genericSuperclass = this.getClazz().getGenericSuperclass();

        if (genericSuperclass instanceof ParameterizedType) {
            Type[] actualTypeArguments = ((ParameterizedType) genericSuperclass).getActualTypeArguments();
            if (actualTypeArguments.length > 0) {
                Type typeArgument = actualTypeArguments[0];
                try {
                    if (typeArgument instanceof Class<?>) {
                        return (Class<T>) typeArgument;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        return null;
    }
}
