package com.anwen.mongo.utils;

import cn.hutool.core.convert.Convert;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.table.TableField;
import com.anwen.mongo.generate.ObjectId;
import com.anwen.mongo.generate.Sequence;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import org.bson.Document;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;


/**
 * @author JiaChaoYang
 * bean、map操作
 * @since 2023-02-09 15:08
 **/
public class BeanMapUtilByReflect {

    /**
     * 对象转Map
     * @return java.util.Map<java.lang.String,java.lang.Object>
     * @author JiaChaoYang
     * @since 2023/2/9 15:11
    */
    public static Map<String, Object> beanToMap(Object object) {
        // 默认序列化为数字类型的时间戳
        // String jsonStr = JSON.toJSONString(obj);

        // Fastjson内置了一个默认的日期格式yyyy-MM-dd HH:mm:ss，
        // 可以通过在调用JSON.toJSONString时传入SerializerFeature.WriteDateUseDateFormat来启用。
        // 通过修改默认的时间格式，结合启用默认日期格式，也可以达到按指定日期格式序列化的目的
        // JSON.DEFFAULT_DATE_FORMAT = "yyyy-MM-dd HH:mm:ss.SSS";
        String jsonStr = JSON.toJSONString(object, SerializerFeature.WriteDateUseDateFormat);
        return JSON.parseObject(jsonStr, new TypeReference<Map<String, Object>>() {});
    }
    /**
     * map转对象
     * @param map map
     * @param beanClass class
     * @return T
     * @author JiaChaoYang
     * @since 2023/2/9 15:12
    */
    public static <T> T mapToBean(Map<String,Object> map, Class<T> beanClass) {
        return Convert.convert(beanClass,map);
    }

    public static <T> List<Document> listToDocumentList(Collection<T> collection){
        List<Document> documentList = new ArrayList<>();
        collection.forEach(c -> {
            documentList.add(new Document(checkTableField(c)));
        });
        return documentList;
    }

    /**
     * 使用guava缓存
     * @author: JiaChaoYang
     * @date: 2023/6/7 22:52
     **/
    private static final LoadingCache<Class<?>, Boolean> ID_ANNOTATION_CACHE = CacheBuilder.newBuilder()
            .maximumSize(1000)
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .build(new CacheLoader<Class<?>, Boolean>() {
                @Override
                public Boolean load(Class<?> clazz) {
                    return hasIdAnnotation(clazz);
                }
            });

    /**
     * 检查对象属性并返回属性值Map。
     * @param entity 对象实例
     * @return 属性值Map
     */
    public static <T> Map<String, Object> checkTableField(T entity) {
        // 存放结果的Map
        Map<String, Object> resultMap = new HashMap<>();
        // 获取对象的Class对象
        Class<?> entityClass = entity.getClass();

        try {
            // 判断是否存在ID注解
            if (!ID_ANNOTATION_CACHE.get(entityClass)) {
                String id = generateObjectId(entity);
                resultMap.put("_id", id);
            }

            // 设置所有属性可访问
            Field[] fields = entityClass.getDeclaredFields();
            AccessibleObject.setAccessible(fields, true);

            // 遍历对象的所有属性，将其添加到结果中
            for (Field field : fields) {
                TableField tableField = field.getAnnotation(TableField.class);

                // 如果TableField注解的exist值为false，则跳过该属性
                if (tableField != null && !tableField.exist()) {
                    continue;
                }

                // 获取属性名和属性值，并添加到结果中
                String fieldName = tableField != null && StringUtils.isNotBlank(tableField.value()) ? tableField.value() : field.getName();
                Object fieldValue = field.get(entity);
                if (fieldValue != null) {
                    resultMap.put(fieldName, fieldValue);
                }
            }
        } catch (ExecutionException | IllegalAccessException e) {
            handleException(e);
        }

        // 返回结果Map
        return resultMap;
    }

    /**
     * 判断类是否有ID注解。
     * @param clazz 类
     * @return 是否有ID注解
     */
    private static boolean hasIdAnnotation(Class<?> clazz) {
        Field[] fields = clazz.getDeclaredFields();

        for (Field field : fields) {
            if (field.isAnnotationPresent(ID.class)) {
                return true;
            }
        }

        return false;
    }

    /**
     * 生成对象的ID。
     * @param entity 对象实例
     * @return ID字符串
     */
    private static String generateObjectId(Object entity) {
        try {
            Class<?> entityClass = entity.getClass();
            Field[] fields = entityClass.getSuperclass().getFields();

            // 遍历父类的字段，查找ID字段的值并返回
            for (Field field : fields) {
                if ("id".equals(field.getName())) {
                    Object idValue = field.get(entity);

                    if (idValue != null) {
                        return String.valueOf(idValue);
                    }
                }
            }
        } catch (IllegalAccessException e) {
            handleException(e);
        }

        // 如果没有ID字段，则生成新的ID
        return ObjectId.next(false);
    }

    /**
     * 处理异常并将其抛出。
     * @param e 异常对象
     */
    private static void handleException(Exception e) {
        throw new RuntimeException(e);
    }
}
