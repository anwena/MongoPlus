package com.anwen.mongo.cache.global;

import com.anwen.mongo.aware.Aware;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 感知处理类缓存
 *
 * @author loser
 * @date 2024/6/29
 */
public class AwareHandlerCache<T extends Aware> {

    private final Map<Class<T>, List<T>> handlerMap = new ConcurrentHashMap<>();

    private static final AwareHandlerCache instant = new AwareHandlerCache();

    private AwareHandlerCache() {
    }

    public static AwareHandlerCache getInstance() {
        return instant;
    }

    /**
     * 获取感知类下的所有处理器
     *
     * @param clazz 感知类
     * @param <T>   感知类
     * @return 感知处理器集合
     */
    public static <T extends Aware> List<T> listHandlers(Class<T> clazz) {

        Object o = getInstance().handlerMap.get(clazz);
        if (Objects.isNull(o)) {
            return Collections.emptyList();
        }
        return (List<T>) o;

    }

    /**
     * 将感知类添加到缓存
     *
     * @param aware 感知类
     * @param <T>   感知类型
     */
    public static synchronized <T extends Aware> void putAware(T aware) {

        Class<?>[] interfaces = aware.getClass().getInterfaces();
        for (Class<?> anInterface : interfaces) {
            if (Aware.class.isAssignableFrom(anInterface)) {
                List<T> handlers;
                Object o = getInstance().handlerMap.get(anInterface);
                if (Objects.nonNull(o)) {
                    handlers = (List<T>) o;
                } else {
                    handlers = new ArrayList<>();
                    getInstance().handlerMap.put(anInterface, handlers);
                }
                handlers.add(aware);
            }
        }

    }

}
