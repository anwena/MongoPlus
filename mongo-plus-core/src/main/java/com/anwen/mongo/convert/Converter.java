package com.anwen.mongo.convert;

import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.toolkit.InstantUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCursor;

import java.util.*;
import java.util.stream.Collectors;

public class Converter {

    /**
     * 将FindIterable<Document>转换为List<Map<String, Object>>。
     *
     * @param iterable 待转换的FindIterable<Document>对象
     * @return java.util.List<java.util.Map < java.lang.String, java.lang.Object>> 转换后的List<Map<String, Object>>对象
     * @author JiaChaoYang
     * @date 2023/6/29/029
     */
    public static List<Map<String, Object>> convertDocumentToMap(FindIterable<Map> iterable) {
        List<Map<String, Object>> resultList = new ArrayList<>();
        try (MongoCursor<Map> cursor = iterable.iterator()) {
            while (cursor.hasNext()) {
                resultList.add(convertKeysToCamelCase(cursor.next()));
            }
        }
        return resultList;
    }

    public static Map<String,Object> convertDocumentToMapOne(FindIterable<Map> iterable){
        try (MongoCursor<Map> cursor = iterable.iterator()) {
            if (cursor.hasNext()) {
                return convertKeysToCamelCase(cursor.next());
            } else {
                return new HashMap<>();
            }
        }
    }

    public static List<Map<String, Object>> convertDocumentToMap(MongoCursor<Map> cursor) {
        List<Map<String, Object>> resultList = new ArrayList<>();
        while (cursor.hasNext()) {
            resultList.add(convertKeysToCamelCase(cursor.next()));
        }
        return resultList;
    }

    public static Map<String, Object> convertKeysToCamelCase(Map<String, Object> map) {
        return map.entrySet().stream()
                .collect(Collectors.toMap(
                        entry -> convertToCamelCaseIfNeeded(entry.getKey()),
                        entry -> convertValue(entry.getValue())
                ));
    }

    private static String convertToCamelCaseIfNeeded(String key) {
        return PropertyCache.mapUnderscoreToCamelCase ? StringUtils.convertToCamelCase(key) : key;
    }

    private static Object convertValue(Object value) {
        if (value instanceof Date) {
            return InstantUtil.convertTimestampToLocalDateTime8(((Date) value).toInstant());
        }
        return value;
    }
}
