package com.anwen.mongo.convert;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.convert.factory.DocumentFieldMapperFactory;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCursor;
import org.bson.Document;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
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
                list.add(mapDocument(cursor.next(), clazz));
            }
        }
        return list;
    }

    public static <T> List<T> mapDocumentList(MongoCursor<Document> cursor, Class<T> clazz) {
        List<T> list = new ArrayList<>();
        while (cursor.hasNext()) {
            list.add(mapDocument(cursor.next(), clazz));
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
            field.setAccessible(true);
            CollectionField collectionField = field.getAnnotation(CollectionField.class);
            ID id = field.getAnnotation(ID.class);
            String fieldName = collectionField != null ? collectionField.value() : field.getName();
            if (id != null) fieldName = SqlOperationConstant._ID;
            if (collectionField != null && !collectionField.exist()) {
                continue;
            }
            if (doc.containsKey(fieldName)) {
                Object fieldValue = Objects.equals(fieldName, SqlOperationConstant._ID) ? String.valueOf(doc.get(fieldName)) : doc.get(fieldName);
                DocumentFieldMapper<Object> fieldMapper = DocumentFieldMapperFactory.getMapper(field, fieldValue);
                fieldMapper.mapField(doc, field, obj);
            }
        }

        // 处理父类中的字段
        Class<?> superClass = clazz.getSuperclass();
        if (superClass != null && !superClass.equals(Object.class)) {
            mapDocumentFields(doc, obj, superClass);
        }
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
