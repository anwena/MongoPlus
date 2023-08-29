package com.anwen.mongo.convert;

import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCursor;
import org.bson.Document;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Converter {

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
                resultList.add(cursor.next());
            }
        }
        return resultList;
    }

    public static List<Map<String, Object>> convertDocumentToMap(MongoCursor<Map> cursor) {
        List<Map<String, Object>> resultList = new ArrayList<>();
        while (cursor.hasNext()) {
            resultList.add(cursor.next());
        }
        return resultList;
    }

    public static List<Map<String, Object>> convertDocumentToMap(FindIterable<Map> iterable,Integer total) {
        List<Map<String, Object>> resultList = new ArrayList<>(total);
        for (Map<String,Object> map : iterable.batchSize(total)) {
            resultList.add(map);
        }
        return resultList;
    }

    /**
     * 将FindIterable<Document>转换为指定类型的集合。
     * @param iterable 待转换的FindIterable<Document>对象
     * @param clazz 目标类型的Class对象
     * @return java.util.List<T> 目标类型参数
     * @author JiaChaoYang
     * @date 2023/6/29/029
    */
    public static <T> List<T> convertDocumentToList(FindIterable<Document> iterable, Class<T> clazz) {
        List<T> resultList = new ArrayList<>();
        for (Document document : iterable) {
            T obj = convertDocumentToType(document, clazz);
            resultList.add(obj);
        }
        return resultList;
    }

    /**
     * 将Document转换为指定类型的对象。
     * @param document 待转换的Document对象
     * @param clazz 目标类型的Class对象
     * @return T 目标类型参数
     * @author JiaChaoYang
     * @date 2023/6/29/029
    */
    private static <T> T convertDocumentToType(Document document, Class<T> clazz) {
        T obj = null;
        // 假设存在一个名为"fromDocument"的静态方法，接收Document类型参数并返回目标类型的对象
        try {
            obj = (T) clazz.getDeclaredMethod("fromDocument", Document.class)
                    .invoke(null, document);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return obj;
    }


}
