package com.anwen.mongo.listener;

import com.anwen.mongo.cache.global.ListenerCache;
import com.anwen.mongo.model.command.CommandFailed;
import com.anwen.mongo.model.command.CommandStarted;
import com.anwen.mongo.model.command.CommandSucceeded;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description MongoPlus拦截器
 * @date 2023-11-22 14:55
 **/
public class MongoPlusListener implements Listener {

    @Override
    public void commandStarted(CommandStarted commandStarted) {
        ListenerCache.listeners.forEach(interceptor -> interceptor.commandStarted(commandStarted));
    }

    @Override
    public void commandSucceeded(CommandSucceeded commandSucceeded) {
        ListenerCache.listeners.forEach(interceptor -> interceptor.commandSucceeded(commandSucceeded));
    }

    @Override
    public void commandFailed(CommandFailed commandFailed) {
        ListenerCache.listeners.forEach(interceptor -> interceptor.commandFailed(commandFailed));
    }

    @Override
    public int getOrder() {
        return HIGHEST_PRECEDENCE;
    }
}
