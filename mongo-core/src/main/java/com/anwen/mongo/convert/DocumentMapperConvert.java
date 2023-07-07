package com.anwen.mongo.convert;

import cn.hutool.core.date.DateTime;
import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.table.TableField;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCursor;
import org.bson.Document;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;

/**
 * @Description: Document转对象
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.convert
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-07 19:30
 * @Version: 1.0
 */
public class DocumentMapperConvert {

    private static final Map<Class<?>, List<Field>> FIELD_CACHE = new HashMap<>();

    /**
     * 将一个 Document 对象转换成指定类型的对象
     * @author: JiaChaoYang
     * @date: 2023/6/7 20:58
     **/
    public static <T> T mapDocument(Document doc, Class<T> clazz) {
        T obj = null;
        try {
            obj = clazz.getDeclaredConstructor().newInstance();
            mapDocumentFields(doc, obj, clazz);
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
        return obj;
    }

    /**
     * 将一个Document集合转为对象集合
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:26
     **/
    public static <T> List<T> mapDocumentList(FindIterable<Document> findIterable, Class<T> clazz) {
        List<T> list = new ArrayList<>();
        try (MongoCursor<Document> cursor = findIterable.iterator()) {
            while (cursor.hasNext()) {
                Document doc = cursor.next();
                T obj = mapDocument(doc, clazz);
                list.add(obj);
            }
        }
        return list;
    }

    /**
     * 递归处理字段，并将处理结果合并到最终的对象中
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:26
     **/
    private static void mapDocumentFields(Document doc, Object obj, Class<?> clazz) throws IllegalAccessException, InstantiationException {
        List<Field> fields = getFields(clazz);
        for (Field field : fields) {
            TableField tableField = field.getAnnotation(TableField.class);
            ID id = field.getAnnotation(ID.class);
            String fieldName = tableField != null ? tableField.value() : field.getName();
            if (id != null) fieldName = "_id";
            if (tableField != null && !tableField.exist()) {
                continue;
            }
            if (doc.containsKey(fieldName)) {
                Object fieldValue = doc.get(fieldName);
                if (fieldValue != null) {
                    field.setAccessible(true);
                    if (Objects.equals(fieldName, "_id")){
                        field.set(obj, String.valueOf(fieldValue));
                    } else if (field.getType().equals(Date.class)) {
                        if (fieldValue instanceof Date) {
                            field.set(obj, fieldValue);
                        } else if (fieldValue instanceof Long) {
                            field.set(obj, new Date((Long) fieldValue));
                        }
                    } else if (field.getType().equals(LocalDateTime.class)) {
                        if (fieldValue instanceof Date) {
                            field.set(obj, ((Date) fieldValue).toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime());
                        }
                    } else if (!isPrimitive(field.getType())) {
                        if (fieldValue instanceof Document) {
                            Object nestedObj = mapDocument((Document) fieldValue, field.getType());
                            field.set(obj, nestedObj);
                        } else {
                            field.set(obj, fieldValue);
                        }
                    } else {
                        field.set(obj, fieldValue);
                    }
                }
            }
        }

        // 处理父类中的字段
        Class<?> superClass = clazz.getSuperclass();
        if (superClass != null && !superClass.equals(Object.class)) {
            mapDocumentFields(doc, obj, superClass);
        }
    }

    /**
     * 判断给定类型是否为基本类型或基本类型的包装类型
     */
    private static boolean isPrimitive(Class<?> type) {
        return type.isPrimitive() || Number.class.isAssignableFrom(type) || Boolean.class.isAssignableFrom(type)
                || Character.class.isAssignableFrom(type) || String.class.isAssignableFrom(type);
    }


    /**
     * 处理父类中的字段
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:26
     **/
    private static void mapSuperClassFields(Document doc, Object obj, Class<?> clazz) throws IllegalAccessException {
        List<Field> fields = getFields(clazz);
        for (Field field : fields) {
            TableField tableField = field.getAnnotation(TableField.class);
            String fieldName = tableField != null ? tableField.value() : field.getName();
            if (tableField != null && !tableField.exist()) {
                continue;
            }
            if (doc.containsKey(fieldName)) {
                Object fieldValue = doc.get(fieldName);
                field.setAccessible(true);
                field.set(obj, fieldValue);
            }
        }

        // 如果有父类，递归处理父类
        Class<?> superClass = clazz.getSuperclass();
        if (superClass != null && !superClass.equals(Object.class)) {
            mapSuperClassFields(doc, obj, superClass);
        }
    }

    /**
     * 将一个 Document List 对象转换成指定类型的 List
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:26
     **/
    private static <T> List<T> mapList(List<Document> docList, Class<T> clazz) {
        List<T> list = new ArrayList<>();
        for (Document doc : docList) {
            T obj = mapDocument(doc, clazz);
            list.add(obj);
        }
        return list;
    }

    /**
     * 将一个 Document List 对象转换成指定类型的数组
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:26
     **/
    private static <T> T[] mapArray(List<Document> docList, Class<T> componentType) {
        T[] array = (T[]) java.lang.reflect.Array.newInstance(componentType, docList.size());
        for (int i = 0; i < docList.size(); i++) {
            Document doc = docList.get(i);
            T obj = mapDocument(doc, componentType);
            array[i] = obj;
        }
        return array;
    }

    /**
     * 将一个 Document 中的 Map 对象转换成指定类型的 Map
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:26
     **/
    private static  <K, V> Map<K, V> mapMap(Map<String, Object> docMap, Class<K> keyType, Class<V> valueType) {
        Map<K, V> map = new HashMap<>();
        for (Map.Entry<String, Object> entry : docMap.entrySet()) {
            K key = keyType.cast(entry.getKey());
            V value = valueType.cast(entry.getValue());
            map.put(key, value);
        }
        return map;
    }

    /**
     * 获取类的所有字段，包括父类中的字段
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:27
     **/
    private static List<Field> getFields(Class<?> clazz) {
        List<Field> fields = FIELD_CACHE.get(clazz);
        if (fields == null) {
            fields = new ArrayList<>();
            while (clazz != null && !clazz.equals(Object.class)) {
                fields.addAll(Arrays.asList(clazz.getDeclaredFields()));
                clazz = clazz.getSuperclass();
            }
            FIELD_CACHE.put(clazz, fields);
        }
        return fields;
    }
}
