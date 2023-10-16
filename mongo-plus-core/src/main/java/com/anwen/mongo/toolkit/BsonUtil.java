package com.anwen.mongo.toolkit;

import com.anwen.mongo.annotation.collection.CollectionField;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import org.bson.Document;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;

/**
 * Document转换
 *
 * @author JiaChaoYang
 **/
public class BsonUtil {

    /**
     * 将Document列换转换JavaList
     * @param documents
     * @param clazz
     * @param <T>
     * @return
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     */
    public static <T> List<T> toBeans(List<Document> documents, Class<T> clazz)
            throws IllegalArgumentException, InstantiationException,
            IllegalAccessException, InvocationTargetException {
        List<T> list = new ArrayList<T>();
        for (int i = 0; null != documents && i < documents.size(); i++) {
            list.add(toBean(documents.get(i), clazz));
        }
        return list;
    }
    /*
     * 将Bson 转化为对象
     *
     * @param:Bson文档
     *
     * @param:类pojo
     *
     * @param:返回对象
     */
    public static <T> T toBean(Document document, Class<T> clazz) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        T obj = clazz.newInstance();// 声明一个对象
        Field[] fields = clazz.getDeclaredFields();// 获取所有属性
        Method[] methods = clazz.getMethods();// 获取所有的方法
        /*
         * 查找所有的属性，并通过属性名和数据库字段名通过相等映射
         */
        for (int i = 0; i < fields.length; i++) {
            String fieldName = fields[i].getName();
            CollectionField column = fields[i].getAnnotation(CollectionField.class);
            Object bson = null;
            if (null != column && null != column.value()) {
                bson = document.get(column.value());
            } else if ("id".equals(fieldName)) {
                bson = document.get("_id");
            } else {
                bson = document.get(fieldName);
            }
            if (null == bson) {
                continue;
            } else if (bson instanceof Document) {// 如果字段是文档了递归调用
                bson = toBean((Document) bson, fields[i].getType());
            } else if (bson instanceof MongoCollection) {// 如果字段是文档集了调用colTOList方法
                bson = colToList(bson, fields[i]);
            }
            for (int j = 0; j < methods.length; j++) {// 为对象赋值
                String metdName = methods[j].getName();
                if (equalFieldAndSet(fieldName, metdName)) {
                    methods[j].invoke(obj, bson);
                    break;
                }
            }
        }
        return obj;
    }
    
    public static List<Document> toBsons(List<Object> objs) throws IllegalArgumentException, SecurityException, IllegalAccessException, InvocationTargetException, NoSuchFieldException {
        List<Document> documents = new ArrayList<Document>();
        for (int i = 0; null != objs && i < objs.size(); i++) {
            documents.add(toBson(objs.get(i)));
        }
        return documents;
    }

    /*
     * 将对象转化为Bson文档
     * @param:对象
     * @param:类型
     * @return:文档
     */
    public static Document toBson(Object obj) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException, SecurityException, NoSuchFieldException {
        if (null == obj) {
            return null;
        }
        Class<? extends Object> clazz = obj.getClass();
        Document document = new Document();
        Method[] methods = clazz.getDeclaredMethods();
        Field[] fields = clazz.getDeclaredFields();
        for (int i = 0; i < fields.length; i++) {
            CollectionField collectionField = fields[i].getAnnotation(CollectionField.class);// 获取列注解内容
            String key = null;// 对应的文档键值
            if (null != collectionField) {
                if (null != collectionField.value()) {// 存在列映射取值
                    key = collectionField.value();
                } else if (!collectionField.exist()) {// 不是列的情况
                    continue;
                } else {
                    key = fields[i].getName();// 默认情况通过属性名映射
                    if ("id".equals(key)) {// 替换id为_id
                        key = "_id";
                    }
                }
            }
            String fieldName = fields[i].getName();
            /*
             * 获取对象属性值并映射到Document中
             */
            for (int j = 0; j < methods.length; j++) {
                String methdName = methods[j].getName();
                if (equalFieldAndGet(fieldName, methdName)) {
                    Object val = methods[j].invoke(obj);// 得到值
                    if (null == val) {
                        continue;
                    }
                    if (isJavaClass(methods[j].getReturnType())) {
                        if (methods[j].getReturnType().getName()
                                .equals("java.util.List")) {// 列表处理
                            @SuppressWarnings("unchecked")
                            List<Object> list = (List<Object>) val;
                            List<Document> documents = new ArrayList<>();
                            for (Object obj1 : list) {
                                documents.add(toBson(obj1));
                            }
                            document.append(key, documents);
                        } else {// 其它对象处理，基本类型
                            document.append(key, val);
                        }
                    } else {// 自定义类型
                        document.append(key, toBson(val));
                    }
                }
            }
        }
        return document;
    }
    /*
     * 是否是自定义类型】
     *
     * false:是自定义
     */
    private static boolean isJavaClass(Class<?> clz) {
        return clz != null && clz.getClassLoader() == null;
    }

    /*
     * 将文档集转化为列表
     *
     * @param:文档集
     *
     * @param:属性类型
     *
     * @return:返回列表
     */
    private static List<Object> colToList(Object bson, Field field) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        ParameterizedType pt = (ParameterizedType) field.getGenericType();// 获取列表的类型
        List<Object> objs = new ArrayList<Object>();
        @SuppressWarnings("unchecked")
        MongoCollection<Document> cols = (MongoCollection<Document>) bson;
        MongoCursor<Document> cursor = cols.find().iterator();
        while (cursor.hasNext()) {
            Document child = cursor.next();
            @SuppressWarnings("rawtypes")
            Class clz = (Class) pt.getActualTypeArguments()[0];// 获取元素类型
            @SuppressWarnings("unchecked")
            Object obj = toBean(child, clz);
            System.out.println(child);
            objs.add(obj);

        }
        return objs;
    }

    /*
     * 比较setter方法和属性相等
     */
    private static boolean equalFieldAndSet(String field, String name) {
        if (name.toLowerCase().matches("set" + field.toLowerCase())) {
            return true;
        } else {
            return false;
        }
    }

    /*
     * 比较getter方法和属性相等
     */
    private static boolean equalFieldAndGet(String field, String name) {
        if (name.toLowerCase().matches("get" + field.toLowerCase())) {
            return true;
        } else {
            return false;
        }
    }

}
