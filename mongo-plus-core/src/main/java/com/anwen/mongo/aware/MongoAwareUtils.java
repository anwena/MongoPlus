package com.anwen.mongo.aware;

import com.anwen.mongo.cache.global.AwareHandlerCache;

import java.util.List;

/**
 * 感知工具类 获取对应的 class 处理类
 *
 * @author loser
 * @date 2024/6/29
 */
public class MongoAwareUtils {

    private MongoAwareUtils() {
    }

    public static <T extends Aware> List<T> listHandlers(Class<T> clazz) {
        return AwareHandlerCache.listHandlers(clazz);
    }

}
