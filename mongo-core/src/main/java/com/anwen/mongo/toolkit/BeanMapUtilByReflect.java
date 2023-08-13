package com.anwen.mongo.toolkit;

import cn.hutool.core.convert.Convert;
import com.alibaba.fastjson.JSON;
import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.constant.IdAutoConstant;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.IdTypeEnum;
import org.bson.Document;

import java.io.File;
import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;


/**
 * @author JiaChaoYang
 * bean、map操作
 * @since 2023-02-09 15:08
 **/
public class BeanMapUtilByReflect {

    public static List<Document> mapListToDocumentList(Collection<Map<String,Object>> mapCollection){
        return mapCollection.stream().map(Document::new).collect(Collectors.toList());
    }

    /**
     * 检查对象属性并返回属性值Map。
     * @param entity 对象实例
     * @return 属性值Map
     */
    public static <T> Map<String, Object> checkTableField(T entity) {
        //定义返回结果Map
        Map<String,Object> resultMap = new HashMap<>();
        //获取实体class
        Class<?> entityClass = entity.getClass();
        //获取所有字段
        Field[] fields = entityClass.getDeclaredFields();
        //设置所有属性可访问
        AccessibleObject.setAccessible(fields,true);
        for (Field field : fields) {
            //获取CollectionField注解
            CollectionField collectionField = field.getAnnotation(CollectionField.class);
            //判断是否跳过该属性
            if (collectionField != null && !collectionField.exist()){
                continue;
            }
            Object fieldValue = null;
            //获取字段值
            try {
                fieldValue = field.get(entity);
            } catch (IllegalAccessException e) {
                throw new RuntimeException(e);
            }
            //获取id注解
            ID id = field.getAnnotation(ID.class);
            //如果使用了ID注解
            if (id != null){
                //如果使用了自增id
                if (id.type() == IdTypeEnum.AUTO){
                    //将此常量设置为true，表示需要自增处理
                    IdAutoConstant.IS_IT_AUTO_ID = true;
                }
                //设置值
                resultMap.put(SqlOperationConstant._ID,fieldValue == null ? Generate.generateId(id.type()) : fieldValue);
            }
            //获取属性名
            String fieldName = collectionField != null && StringUtils.isNotBlank(collectionField.value()) ? collectionField.value() : field.getName();
            //不为null再进行映射
            if (fieldValue != null){
                setFieldValue(field,fieldValue,fieldName,resultMap);
            }
        }
        return resultMap;
    }

    private static void setFieldValue(Field field,Object fieldValue,String fieldName,Map<String,Object> resultMap){
        //如果是自定义类，需要特殊处理，不然会修改该字段的所有值
        if (ClassTypeUtil.isItCustomType(field)){
            //获取类型
            Class<?> fieldType = field.getType();
            if (fieldType.isArray() || Collection.class.isAssignableFrom(fieldType)){
                List<Map> mapList = JSON.parseArray(JSON.toJSONString(fieldValue), Map.class);
                mapList.forEach(childMap -> {
                    setChildFieldValue(fieldName,childMap,resultMap);
                });
            }else {
                setChildFieldValue(fieldName,Convert.convert(Map.class, fieldValue),resultMap);
            }
        }else {
            resultMap.put(fieldName, fieldValue);
        }
    }

    private static void setChildFieldValue(String fieldName , Map<String,Object> childMap,Map<String,Object> resultMap){
        childMap.values().removeIf(Objects::isNull);
        childMap.keySet().forEach(map -> {
            resultMap.put(fieldName + "." + map, childMap.get(map));
        });
    }

    /**
     * 处理异常并将其抛出。
     * @param e 异常对象
     */
    private static void handleException(Exception e) {
        throw new RuntimeException(e);
    }
}
