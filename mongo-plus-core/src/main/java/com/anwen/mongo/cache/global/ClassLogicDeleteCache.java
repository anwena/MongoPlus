package com.anwen.mongo.cache.global;

import com.anwen.mongo.model.LogicDeleteResult;

import java.util.HashMap;
import java.util.Map;

/**
 * 全局逻辑删除基础信息缓存
 *
 * @author loser
 * @date 2024/4/29
 */
public class ClassLogicDeleteCache {

    /**
     * 是否开启逻辑删除功能
     */
    public static Boolean open = false;

    /**
     * 目标文档对应的逻辑删除字段
     */
    public static final Map<Class<?>, LogicDeleteResult> logicDeleteResultHashMap = new HashMap<>();

}
