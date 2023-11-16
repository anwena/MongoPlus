package com.anwen.mongo.convert;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.cache.PropertyCache;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.FindIterable;
import com.mongodb.client.ListIndexesIterable;
import com.mongodb.client.MongoCursor;
import org.bson.Document;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * @Description: Document转对象
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.convert
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-07 19:30
 * @Version: 1.0
 */
public class DocumentMapperConvert {

    /**
     * 将一个 Document 对象转换成指定类型的对象
     * @author: JiaChaoYang
     * @date: 2023/6/7 20:58
     **/
    public static <T> T mapDocument(Document document, Class<T> clazz) {
        return mapDocument(document,clazz,true);
    }

    public static List<Document> indexesIterableToDocument(ListIndexesIterable<Document> indexesIterable){
        return new ArrayList<Document>(){{
            try (MongoCursor<Document> cursor = indexesIterable.iterator()) {
                while (cursor.hasNext()) {
                    add(cursor.next());
                }
            }
        }};
    }

    public static <T> T mapDocument(Document document,Class<T> clazz,Boolean annotationEffect){
        if (document == null) {
            return null;
        }
        T obj;
        try {
            obj = clazz.getDeclaredConstructor().newInstance();
            mapDocumentFields(document, obj, clazz,annotationEffect);
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
                Optional.ofNullable(mapDocument(cursor.next(), clazz)).ifPresent(list::add);
            }
        }
        return list;
    }

    public static <T> List<T> mapDocumentList(MongoCursor<Document> cursor, Class<?> clazz) {
        List<T> list = new ArrayList<>();
        while (cursor.hasNext()) {
            Optional.ofNullable(mapDocument(cursor.next(), clazz)).ifPresent(obj -> list.add((T) obj));
        }
        return list;
    }

    /**
     * 递归处理字段，并将处理结果合并到最终的对象中
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:26
     **/
    private static void mapDocumentFields(Document doc, Object obj, Class<?> clazz, Boolean annotationEffect) throws IllegalAccessException, InstantiationException {
        List<Field> fields = ClassTypeUtil.getFields(clazz);
        for (Field field : fields) {
            field.setAccessible(true);
            String fieldName = field.getName();
            if (annotationEffect) {
                CollectionField collectionField = field.getAnnotation(CollectionField.class);
                ID id = field.getAnnotation(ID.class);
                if (PropertyCache.mapUnderscoreToCamelCase) {
                    fieldName = StringUtils.convertToCamelCase(fieldName);
                }
                if (collectionField != null && StringUtils.isNotBlank(collectionField.value())) {
                    fieldName = collectionField.value();
                }
                if (id != null) fieldName = SqlOperationConstant._ID;
                if (collectionField != null && !collectionField.exist()) {
                    continue;
                }
            }
            if (doc.get(fieldName) == null) {
                continue;
            }
            Object fieldValue = Objects.equals(fieldName, SqlOperationConstant._ID) ? String.valueOf(doc.get(fieldName)) : doc.get(fieldName);
            ConversionService.setValue(field,obj,fieldValue);
        }

        // 处理父类中的字段
        Class<?> superClass = clazz.getSuperclass();
        if (superClass != null && !superClass.equals(Object.class)) {
            mapDocumentFields(doc, obj, superClass,annotationEffect);
        }
    }

}
