package com.anwen.mongo.utils;

import java.lang.reflect.Modifier;
import java.util.Map;

/**
 * @Description: 获取集合中的自定义类
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.utils
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-06 23:42
 * @Version: 1.0
 */
public class CustomClassUtil {

    public static boolean isCustomObject(Class<?> clazz) {
        // 获取对象所属的包。
        Package pkg = clazz.getPackage();
        String pkgName = pkg != null ? pkg.getName() : "";
        // 检查类所属的包名，如果以 java. 或 javax. 开头，则认为它是系统类。
        if (pkgName.startsWith("java.") || pkgName.startsWith("javax.")) {
            return false;
        }
        // 判断该类是否为枚举类型。
        if (clazz.isEnum()) {
            return false;
        }
        // 判断该类是否为匿名类。
        if (clazz.isAnonymousClass()) {
            return false;
        }
        // 判断该类是否为局部类。
        if (clazz.isLocalClass()) {
            return false;
        }
        // 判断该类是否为数组类型。
        if (clazz.isArray()) {
            return false;
        }
        // 判断该类是否为集合类型。
        if (Iterable.class.isAssignableFrom(clazz) || Map.class.isAssignableFrom(clazz)) {
            return false;
        }
        // 判断该类是否为抽象类。
        if (Modifier.isAbstract(clazz.getModifiers())) {
            return false;
        }
        // 判断该类是否为基础类型。
        if (clazz.isPrimitive()) {
            return false;
        }
        // 判断该类是否为合成类。
        if (clazz.isSynthetic()) {
            return false;
        }
        // 判断该类是否为 String 类型。
        if (clazz == String.class) {
            return false;
        }
        // 判断该类是否为 Boolean 类型。
        if (clazz == Boolean.class) {
            return false;
        }
        // 判断该类是否为 Character 类型。
        if (clazz == Character.class) {
            return false;
        }
        // 判断该类是否为 Byte 类型。
        if (clazz == Byte.class) {
            return false;
        }
        // 判断该类是否为 Short 类型。
        if (clazz == Short.class) {
            return false;
        }
        // 判断该类是否为 Integer 类型。
        if (clazz == Integer.class) {
            return false;
        }
        // 判断该类是否为 Long 类型。
        if (clazz == Long.class) {
            return false;
        }
        // 判断该类是否为 Float 类型。
        if (clazz == Float.class) {
            return false;
        }
        // 判断该类是否为 Double 类型。
        if (clazz == Double.class) {
            return false;
        }
        // 判断该类是否为 Void 类型。
        if (clazz == Void.class) {
            return false;
        }
        // 如果以上所有情况都排除完毕，那么就认为该类是自定义类。
        return true;
    }

}
