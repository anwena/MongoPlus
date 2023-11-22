package com.anwen.mongo.interceptor;

import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.model.command.CommandFailed;
import com.anwen.mongo.model.command.CommandStarted;
import com.anwen.mongo.model.command.CommandSucceeded;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description MongoPlus拦截器
 * @date 2023-11-22 14:55
 **/
public class MongoPlusInterceptor implements Interceptor {

    @Override
    public void commandStarted(CommandStarted commandStarted) {
        InterceptorCache.interceptors.forEach(interceptor -> interceptor.commandStarted(commandStarted));
    }

    @Override
    public void commandSucceeded(CommandSucceeded commandSucceeded) {
        InterceptorCache.interceptors.forEach(interceptor -> interceptor.commandSucceeded(commandSucceeded));
    }

    @Override
    public void commandFailed(CommandFailed commandFailed) {
        InterceptorCache.interceptors.forEach(interceptor -> interceptor.commandFailed(commandFailed));
    }

    @Override
    public int getOrder() {
        return HIGHEST_PRECEDENCE;
    }
}
