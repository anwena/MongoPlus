package com.anwen.mongo.convert;

import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCursor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Converter {

    private static final Logger logger = LoggerFactory.getLogger(Converter.class);

    /**
     * 将FindIterable<Document>转换为List<Map<String, Object>>。
     * @param iterable 待转换的FindIterable<Document>对象
     * @return java.util.List<java.util.Map<java.lang.String,java.lang.Object>> 转换后的List<Map<String, Object>>对象
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

    public static List<Map<String, Object>> convertDocumentToMap(MongoCursor<Map> cursor) {
        List<Map<String, Object>> resultList = new ArrayList<>();
        while (cursor.hasNext()) {
            resultList.add(convertKeysToCamelCase(cursor.next()));
        }
        return resultList;
    }

    public static List<Map<String, Object>> convertDocumentToMap(FindIterable<Map> iterable,Integer total) {
        List<Map<String, Object>> resultList = new ArrayList<>(total);
        for (Map<String,Object> map : iterable.batchSize(total)) {
            resultList.add(convertKeysToCamelCase(map));
        }
        return resultList;
    }

    public static Map<String, Object> convertKeysToCamelCase(Map<String, Object> map) {
        if (!PropertyCache.mapUnderscoreToCamelCase){
            return map;
        }
        return map.entrySet().stream()
                .collect(Collectors.toMap(
                        entry -> StringUtils.convertToCamelCase(entry.getKey()),
                        Map.Entry::getValue)
                );
    }


}
