package com.anwen.mongo.listener.business;

import com.anwen.mongo.cache.global.OrderCache;
import com.anwen.mongo.listener.Listener;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.model.command.CommandFailed;
import com.anwen.mongo.model.command.CommandStarted;
import com.anwen.mongo.model.command.CommandSucceeded;

import java.util.Objects;

/**
 * Mongo拦截器，这里可以打印日志
 * @author JiaChaoYang
 * @date 2023/11/22 10:54
*/
public class LogListener implements Listener {

    private static final Log log = LogFactory.getLog(LogListener.class);

    @Override
    public void commandStarted(CommandStarted commandStarted) {
        log.info(commandStarted.getCommandName()+" Statement Execution ==> ");
        log.info(commandStarted.getCommand());
    }

    @Override
    public void commandSucceeded(CommandSucceeded commandSucceeded) {
        Integer resultCount = null;
        if (Objects.equals(commandSucceeded.getCommandName(), "find") || Objects.equals(commandSucceeded.getCommandName(), "aggregate")){
            resultCount = commandSucceeded.getResponse().getDocument("cursor").get("firstBatch").asArray().getValues().size();
        } else if (Objects.equals(commandSucceeded.getCommandName(), "update")) {
            resultCount = commandSucceeded.getResponse().get("nModified").asInt32().getValue();
        } else if (Objects.equals(commandSucceeded.getCommandName(), "insert") || Objects.equals(commandSucceeded.getCommandName(), "delete")) {
            resultCount = commandSucceeded.getResponse().get("n").asInt32().getValue();
        }
        if (resultCount != null) {
            log.info(commandSucceeded.getCommandName() + " results of execution ==> " + resultCount);
        }
    }

    @Override
    public void commandFailed(CommandFailed commandFailed) {
        String commandName = commandFailed.getCommandName();
        Throwable throwable = commandFailed.getThrowable();
        log.error("error ==> : " + commandName + ", " + throwable.getMessage());
    }

    @Override
    public int getOrder() {
        return OrderCache.LOG_ORDER;
    }
}
