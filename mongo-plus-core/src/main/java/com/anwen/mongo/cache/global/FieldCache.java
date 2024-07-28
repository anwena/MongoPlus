package com.anwen.mongo.cache.global;

import com.anwen.mongo.annotation.collection.CollectionField;

import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 通用的缓存
 *
 * @author anwen
 */
public class FieldCache {

    private static final Map<Field, Type> genericTypeMapCache = new ConcurrentHashMap<>();

    private static final Map<Field, CollectionField> collectionFieldMapCache = new ConcurrentHashMap<>();

    public static Type getGenericType(Field field) {
        return genericTypeMapCache.getOrDefault(field,null);
    }

    public static Map<Field, Type> getGenericTypeMapCache() {
        return genericTypeMapCache;
    }

    public static void setGenericTypeMapCache(Map<Field, Type> genericTypeMapCache) {
        FieldCache.genericTypeMapCache.putAll(genericTypeMapCache);
    }

    public static void setGenericTypeMapCache(Field field,Type type) {
        FieldCache.genericTypeMapCache.put(field,type);
    }

    public static CollectionField getCollectionField(Field field) {
        return collectionFieldMapCache.getOrDefault(field,null);
    }

    public static Map<Field, CollectionField> getCollectionFieldMapCache() {
        return collectionFieldMapCache;
    }

    public static void setCollectionFieldMapCache(Map<Field, CollectionField> collectionFieldMapCache) {
        FieldCache.collectionFieldMapCache.putAll(collectionFieldMapCache);
    }

    public static void setCollectionFieldMapCache(Field field,CollectionField collectionField) {
        FieldCache.collectionFieldMapCache.put(field,collectionField);
    }

}
