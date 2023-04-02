package com.anwen.mongo.utils;

import com.mongodb.BasicDBObject;
import com.mongodb.DBObject;

import java.lang.reflect.Field;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * @Description:
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.utils
 * @Author: JiaChaoYang
 * @CreateTime: 2023-03-07 19:22
 * @Version: 1.0
 */
public class ConvertUtil {

    public static <T> DBObject beanDBObject(T bean) {
        if (bean == null) {
            return null;
        }
        DBObject dbObject = new BasicDBObject();
        // 获取对象对应类中的所有属性域
        Field[] fields = bean.getClass().getDeclaredFields();
        for (Field field : fields) {
            // 获取属性名
            String varName = field.getName();
            // 修改访问控制权限
            boolean accessFlag = field.isAccessible();
            if (!accessFlag) {
                field.setAccessible(true);
            }
            Object param = null;
            try {
                param = field.get(bean);
            } catch (IllegalAccessException e) {
                throw new RuntimeException(e);
            }
            if (param == null) {
                continue;
            } else if (param instanceof Integer) {// 判断变量的类型
                int value = ((Integer) param).intValue();
                dbObject.put(varName, value);
            } else if (param instanceof String) {
                String value = (String) param;
                dbObject.put(varName, value);
            } else if (param instanceof Double) {
                double value = ((Double) param).doubleValue();
                dbObject.put(varName, value);
            } else if (param instanceof Float) {
                float value = ((Float) param).floatValue();
                dbObject.put(varName, value);
            } else if (param instanceof Long) {
                long value = ((Long) param).longValue();
                dbObject.put(varName, value);
            } else if (param instanceof Boolean) {
                boolean value = ((Boolean) param).booleanValue();
                dbObject.put(varName, value);
            } else if (param instanceof Date) {
                Date value = (Date) param;
                dbObject.put(varName, value);
            } else if (param instanceof List) {
                List list = (List) param;
                dbObject.put(varName, list);
            } else if (param instanceof Map) {
                Map map = (Map) param;
                dbObject.put(varName, map);
            }else {
                dbObject.put(varName, param);
            }
            // 恢复访问控制权限
            field.setAccessible(accessFlag);

        }
        return dbObject;

    }

}
