package com.anwen.mongo.proxy;

import com.anwen.mongo.cache.global.ExecutorProxyCache;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Objects;

/**
 * 执行器代理
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-02-05 09:15
 **/
public class ExecutorProxy implements InvocationHandler {

    private final Execute target;

    public ExecutorProxy(Execute target) {
        this.target = target;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String name = method.getName();
        MethodExecutorStrategy executor = ExecutorProxyCache.EXECUTOR_MAP.get(name);
        if (Objects.nonNull(executor)) {
            InterceptorCache.interceptors.forEach(interceptor -> executor.invoke(interceptor, args));
        }
        return method.invoke(target, args);
    }

}
