package com.anwen.mongo.aware;

import com.anwen.mongo.annotation.aware.AwareInvoke;
import com.anwen.mongo.cache.global.AwareHandlerCache;
import com.anwen.mongo.domain.MongoPlusConvertException;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 感知工具类 获取对应的 class 处理类
 *
 * @author loser
 * @date 2024/6/29
 */
public class MongoAwareUtils {

    private final static Map<Class<? extends Aware>, Method> methodMap = new HashMap<>();

    private MongoAwareUtils() {
    }

    public static <T extends Aware> void doInvoke(Class<T> clazz, Object... args) {

        List<T> handlers = AwareHandlerCache.listHandlers(clazz);
        for (T handler : handlers) {
            try {
                Method method = getMethod(clazz);
                method.invoke(handler, args);
            } catch (Exception ex) {
                throw new MongoPlusConvertException(ex.getMessage());
            }
        }

    }

    private static Method getMethod(Class<? extends Aware> clazz) {

        Method cacheMethod = methodMap.get(clazz);
        if (Objects.nonNull(cacheMethod)) {
            return cacheMethod;
        }

        List<Method> methods = new ArrayList<>();
        for (Method method : clazz.getMethods()) {
            if (method.isAnnotationPresent(AwareInvoke.class)) {
                methods.add(method);
            }
        }
        if (methods.size() == 1) {
            Method method = methods.get(0);
            methodMap.put(clazz, method);
            return method;
        }
        throw new MongoPlusConvertException(clazz.getName() + " add @AwareInvoke method must be unique but find " + methods.size());

    }

}
