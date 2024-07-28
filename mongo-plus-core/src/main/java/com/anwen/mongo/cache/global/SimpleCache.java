package com.anwen.mongo.cache.global;

import com.anwen.mongo.mapping.SimpleTypeHolder;

/**
 * 简单类缓存
 *
 * @author anwen
 */
public class SimpleCache {

    private static SimpleTypeHolder simpleTypeHolder = new SimpleTypeHolder();

    public static SimpleTypeHolder getSimpleTypeHolder() {
        return simpleTypeHolder;
    }

    public static void setSimpleTypeHolder(SimpleTypeHolder simpleTypeHolder) {
        SimpleCache.simpleTypeHolder = simpleTypeHolder;
    }

}
