package com.anwen.mongo.cache.global;

import com.anwen.mongo.model.LogicDeleteResult;
import com.anwen.mongo.model.LogicProperty;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 全局逻辑删除基础信息缓存
 *
 * @author loser
 * @date 2024/4/29
 */
public class CollectionLogicDeleteCache {

    private static final ThreadLocal<Boolean> logicIgnore = new InheritableThreadLocal<>();

    /**
     * 是否开启逻辑删除功能
     */
    public static Boolean open = false;

    /**
     * 逻辑删除配置
     */
    public static LogicProperty logicProperty = new LogicProperty();

    /**
     * 目标文档对应的逻辑删除字段
     */
    public static final Map<Class<?>, LogicDeleteResult> logicDeleteResultHashMap = new HashMap<>();

    /**
     * 存储 mongo 连接对象关联的 实体 {"mongo连接fullName":"mongo集合实体class"}
     */
    public static final Map<String, Class<?>> fullNameMap = new ConcurrentHashMap<>();

    /**
     * mongo 连接对象关联的 实体(处理直接通过 BaseMapper 操作的 class)
     *
     * @param fullName mongo 连接 fullName
     * @param clazz    实体
     */
    public static void mapperClassByCollection(String fullName, Class<?> clazz) {
        if (!fullNameMap.containsKey(fullName)) {
            fullNameMap.put(fullName, clazz);
        }
    }

    public static void setLogicIgnore(boolean ignore) {
        logicIgnore.set(ignore);
    }

    public static boolean getLogicIgnore() {
        Boolean ignore = logicIgnore.get();
        return Objects.nonNull(ignore) && ignore;

    }

    public static void clear() {
        logicIgnore.remove();
    }

}
