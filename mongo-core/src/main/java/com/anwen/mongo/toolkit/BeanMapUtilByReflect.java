package com.anwen.mongo.toolkit;

import cn.hutool.core.convert.Convert;
import com.alibaba.fastjson.JSON;
import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.enums.IdTypeEnum;
import org.bson.Document;

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
    public static <T> Map<String, Object> checkTableField(T entity,boolean... isItAutoId) {
        // 存放结果的Map
        Map<String, Object> resultMap = new HashMap<>();
        // 获取对象的Class对象
        Class<?> entityClass = entity.getClass();

        try {
            // 设置所有属性可访问
            Field[] fields = entityClass.getDeclaredFields();
            AccessibleObject.setAccessible(fields, true);

            // 遍历对象的所有属性，将其添加到结果中
            for (Field field : fields) {
                CollectionField collectionField = field.getAnnotation(CollectionField.class);
                // 如果TableField注解的exist值为false，则跳过该属性
                if (collectionField != null && !collectionField.exist()) {
                    continue;
                }
                Object fieldValue = field.get(entity);
                ID id = field.getAnnotation(ID.class);
                if (id != null){
                    if (id.type() == IdTypeEnum.AUTO && isItAutoId != null){
                        resultMap.put(SqlOperationConstant.IS_IT_AUTO_ID,true);
                    }
                    resultMap.put("_id",fieldValue == null ? IdTypeEnum.generateId(id.type()) : fieldValue);
                }
                // 获取属性名和属性值，并添加到结果中
                String fieldName = collectionField != null && StringUtils.isNotBlank(collectionField.value()) ? collectionField.value() : field.getName();
                if (fieldValue != null) {
                    if (id != null){
                        resultMap.put("_id",new org.bson.types.ObjectId(String.valueOf(fieldValue)));
                        continue;
                    }
                    if (ClassTypeUtil.isItCustomType(field)){
                        Class<?> fieldType = field.getType();
                        if (fieldType.isArray() || Collection.class.isAssignableFrom(fieldType)){
                            List<Map> mapList = JSON.parseArray(JSON.toJSONString(fieldValue), Map.class);
                            mapList.forEach(childMap -> {
                                childMap.values().removeIf(Objects::isNull);
                                childMap.keySet().forEach(map -> {
                                    resultMap.put(fieldName+"."+map,childMap.get(map));
                                });
                            });
                        }else {
                            Map<String, Object> childMap = Convert.convert(Map.class, fieldValue);
                            childMap.values().removeIf(Objects::isNull);
                            childMap.keySet().forEach(map -> {
                                resultMap.put(fieldName + "." + map, childMap.get(map));
                            });
                        }
                    }else {
                        resultMap.put(fieldName, fieldValue);
                    }
                }
            }
        } catch (IllegalAccessException e) {
            handleException(e);
        }

        // 返回结果Map
        return resultMap;
    }

    /**
     * 处理异常并将其抛出。
     * @param e 异常对象
     */
    private static void handleException(Exception e) {
        throw new RuntimeException(e);
    }
}
