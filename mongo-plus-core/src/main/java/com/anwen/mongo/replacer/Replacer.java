package com.anwen.mongo.replacer;

import com.anwen.mongo.support.BoolFunction;

import java.lang.reflect.Method;

/**
 * 替换器接
 *
 * @author loser
 * @date 2024/4/30
 */
public interface Replacer {

    default int order() {
        return Integer.MIN_VALUE;
    }

    Object invoke(Object proxy, Object target, Method method, Object[] args) throws Throwable;

    BoolFunction supplier();

}