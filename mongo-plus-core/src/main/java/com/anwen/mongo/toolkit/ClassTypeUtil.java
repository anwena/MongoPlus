package com.anwen.mongo.toolkit;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.cache.MapCodecCache;
import com.anwen.mongo.model.BaseModelID;
import com.mongodb.MongoException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @Description: 获取对象中的所有字段类型
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.utils
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-06 23:34
 * @Version: 1.0
 */
public class ClassTypeUtil {

    private static final Logger logger = LoggerFactory.getLogger(ClassTypeUtil.class);

    // 内部缓存，存储已经处理过的对象类型及其字段的类型
    private static final Map<Class<?>, List<Class<?>>> cacheMap = new ConcurrentHashMap<>();

    private static final Map<Class<?>, List<Field>> FIELD_CACHE = new HashMap<>();

    private static volatile ClassTypeUtil instance;

    private static final Map<Class<?>,Set<Class<?>>> cacheClass = new ConcurrentHashMap<>();

    private ClassTypeUtil() {
    }

    public static ClassTypeUtil getInstance() {
        if (instance == null) {
            synchronized (ClassTypeUtil.class) {
                if (instance == null) {
                    instance = new ClassTypeUtil();
                }
            }
        }
        return instance;
    }

    /**
     * 获取对象的所有字段类型
     * @param clazz 待获取类型字段的class
     * @return 对象的所有字段类型列表
     */
    public static synchronized List<Class<?>> getAllFieldClasses(Class<?> clazz) {
        // 获取对象类型
        // 查找缓存中是否已有该类型对象的记录
        if (cacheMap.containsKey(clazz)) {
            // 如果已有记录，直接返回缓存中存储的结果
            return cacheMap.get(clazz);
        }
        // 如果缓存中没有记录，则使用反射获取对象类型及其所有字段的类型
        List<Class<?>> classList = new ArrayList<>();
        Field[] declaredFields = clazz.getDeclaredFields();
        for (Field field : declaredFields) {
            classList.add(getClassByFieldType(field));
        }
        // 将结果存储到缓存中
        cacheMap.put(clazz, classList);
        // 返回结果
        return classList;
    }

    /**
     * 获取field的类型
     * @author JiaChaoYang
     * @date 2023/8/9 22:04
    */
    public static Class<?> getClassByFieldType(Field field){
        Class<?> fieldType = field.getType();
        if (fieldType.isArray()) {
            // 如果字段类型为数组，获取数组元素类型并添加到列表中
            return fieldType.getComponentType();
        } else if (Collection.class.isAssignableFrom(fieldType)) {
            // 如果字段类型为集合，则获取集合元素的类型并添加到列表中
            Type genericType = field.getGenericType();
            if (genericType instanceof ParameterizedType) {
                ParameterizedType parameterizedType = (ParameterizedType) genericType;
                return (Class<?>) parameterizedType.getActualTypeArguments()[0];
            }
        }
        // 如果字段类型为普通类型，则直接添加到列表中
        return fieldType;
    }

    /**
     * 判断字段是否是自定义类型
     * @param field 字段
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @date 2023/8/9 22:05
    */
    public static Boolean isItCustomType(Field field){
        return CustomClassUtil.isCustomObject(getClassByFieldType(field));
    }

    /**
     * 获取对象的所有自定义类字段类型
     * @param clazz 待获取类型字段的class
     * @return 对象的所有字段类型列表
     */
    public static synchronized List<Class<?>> getAllCustomFieldClasses(Class<?> clazz){
        List<Class<?>> result = new ArrayList<>();
        List<Class<?>> fieldClasses = getAllFieldClasses(clazz);
        fieldClasses.parallelStream().forEach(field -> {
            if (CustomClassUtil.isCustomObject(field)){
                result.add(field);
                result.addAll(getAllCustomFieldClasses(field));
            }
        });
        return result;
    }

    public static <T> Object getClassFieldValue(T entity,String field){
        Field declaredField;
        try {
            declaredField = entity.getClass().getDeclaredField(field);
            declaredField.setAccessible(true);
            return declaredField.get(entity);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 获取实体类中，ID注解的值的值
     * @author JiaChaoYang
     * @date 2023/8/30 22:05
    */
    public static <T> String getIdByEntity(T entity,boolean exception){
        Optional<Field> fieldOptional = getFields(entity.getClass()).stream().peek(field -> field.setAccessible(true)).filter(field -> field.getAnnotation(ID.class) != null).findFirst();
        if (!fieldOptional.isPresent()){
            if (exception){
                return null;
            }
            throw new MongoException("_id undefined");
        }
        try {
            return String.valueOf(fieldOptional.get().get(entity));
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 获取类中所有的字段，如果是自定义类型或者Map、Bson类型，也将子字段获取
     * @author JiaChaoYang
     * @date 2023/11/9 23:23
    */
    public static <T> Set<Class<?>> getAllClass(T entity){
        Set<Class<?>> set = new HashSet<>();
        if (entity == null){
            return set;
        }
        System.out.println("进来了");
        Class<?> clazz = entity.getClass();
        if (cacheClass.containsKey(clazz)){
            return cacheClass.get(clazz);
        }
        Field[] fields = clazz.getDeclaredFields();
        for (Field field : fields) {
            System.out.println("进入循环了");
            field.setAccessible(true);
            Class<?> fieldType = field.getType();
            if (MapCodecCache.codecClassCache.contains(fieldType)){
                logger.info("这个类已经在存在默认解码器了，名称：{}",field.getName());
                continue;
            }
            try {
                if (Map.class.isAssignableFrom(fieldType)){
                    set.addAll(getMapClass((Map<?, ?>) field.get(entity)));
                }
                if (CustomClassUtil.isCustomObject(fieldType)){
                    System.out.println("去递归了");
                    set.addAll(getAllClass(field.get(entity)));
                }
                if (fieldType.equals(List.class)){
                    System.out.println("集合类型");
                    Class<?> listGenericType = ClassTypeUtil.getListGenericType(field);
                    if (Map.class.equals(listGenericType) || CustomClassUtil.isCustomObject(listGenericType)){
                        if (Map.class.equals(listGenericType)){
                            Type[] typeArguments = ((ParameterizedType) field.getGenericType()).getActualTypeArguments();
                            List<Map<?, ?>> mapList = (List<Map<?, ?>>) field.get(entity);
                            if (typeArguments[1].equals(Object.class)){
                                for (Map<?, ?> map : mapList) {
                                    set.addAll(getMapClass(map));
                                }
                            }else {
                                set.addAll(getMapClass(mapList.get(0)));
                            }
                        }else {
                            set.add(listGenericType);
                            set.addAll(getAllClass(((List<?>) field.get(entity)).get(0)));
                        }
                    }
                }
                set.add(fieldType);
            } catch (IllegalAccessException e) {
                logger.error("get value error: {}",field.getName());
            }
        }
        cacheClass.put(clazz,set);
        return set;
        /*return new HashSet<Class<?>>(){{
            Field[] fields = clazz.getDeclaredFields();
            for (Field field : fields) {
                System.out.println("进入循环了");
                field.setAccessible(true);
                Class<?> fieldType = field.getType();
                if (MapCodecCache.codecClassCache.contains(fieldType)){
                    continue;
                }
                add(fieldType);
                try {
                    if (Map.class.isAssignableFrom(fieldType)){
                        addAll(getMapClass((Map<?, ?>) field.get(entity)));
                    }
                    if (CustomClassUtil.isCustomObject(fieldType)){
                        System.out.println("去递归了");
                        addAll(getAllClass(field.get(entity)));
                    }
                } catch (IllegalAccessException e) {
                    logger.error("get value error: {}",field.getName());
                }
            }
        }};*/
    }

    private static Set<Class<?>> getMapClass(Map<?,?> map){
        return new HashSet<Class<?>>(){{
            map.values().forEach(value -> {
                Class<?> clazz = value.getClass();
                if (MapCodecCache.codecClassCache.contains(clazz)){
                    return;
                }
                add(clazz);
                if (CustomClassUtil.isCustomObject(clazz)){
                    addAll(getAllClass(value));
                }
            });
        }};
    }

    /**
     * 获取类的所有字段，包括父类中的字段
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:27
     **/
    public static List<Field> getFields(Class<?> clazz) {
        List<Field> fields = FIELD_CACHE.get(clazz);
        if (fields == null) {
            fields = new ArrayList<>();
            if (!clazz.equals(Object.class)){
                fields.addAll(Arrays.asList(clazz.getDeclaredFields()));
                getSupperFields(fields,clazz.getSuperclass());
            }
            FIELD_CACHE.put(clazz, fields);
        }
        return fields;
    }

    private static void getSupperFields(List<Field> fieldList,Class<?> clazz){
        if (clazz != null && !clazz.equals(Object.class)){
            fieldList.addAll(Arrays.asList(clazz.getDeclaredFields()));
            getSupperFields(fieldList,clazz.getSuperclass());
        }
    }

    /**
     * 获取List的泛型
     * @author JiaChaoYang
     * @date 2023/11/10 14:54
    */
    public static Class<?> getListGenericType(Field field) {
        Type genericType = field.getGenericType();
        if (genericType instanceof ParameterizedType) {
            ParameterizedType parameterizedType = (ParameterizedType) genericType;
            Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
            if (actualTypeArguments.length > 0 && actualTypeArguments[0] instanceof Class) {
                return (Class<?>) actualTypeArguments[0];
            }
        }
        return Object.class;
    }

    public static Class<?> getListClass(List<?> list){
        ParameterizedType parameterizedType = (ParameterizedType) list.getClass().getGenericSuperclass();
        Type[] typeArguments = parameterizedType.getActualTypeArguments();
        return (Class<?>) typeArguments[0];
    }

}

