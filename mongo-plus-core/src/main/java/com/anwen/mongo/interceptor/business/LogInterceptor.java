package com.anwen.mongo.interceptor.business;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.anwen.mongo.cache.global.OrderCache;
import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.command.CommandFailed;
import com.anwen.mongo.model.command.CommandStarted;
import com.anwen.mongo.model.command.CommandSucceeded;

import java.util.Objects;

/**
 * Mongo拦截器，这里可以打印日志
 * @author JiaChaoYang
 * @date 2023/11/22 10:54
*/
public class LogInterceptor implements Interceptor {

    private String formattingStatement(String statement){
        return PropertyCache.format ? JSON.toJSONString(JSONObject.parse(statement), SerializerFeature.PrettyFormat, SerializerFeature.WriteMapNullValue,
                SerializerFeature.WriteDateUseDateFormat) : statement;
    }

    @Override
    public void commandStarted(CommandStarted commandStarted) {
        System.out.println(commandStarted.getCommandName()+" Statement Execution ==> ");
        System.out.println(formattingStatement(commandStarted.getCommand()));
    }

    @Override
    public void commandSucceeded(CommandSucceeded commandSucceeded) {
        System.out.println(commandSucceeded.getCommandName()+" results of execution ==> ");
        if (Objects.equals(commandSucceeded.getCommandName(), "find") || Objects.equals(commandSucceeded.getCommandName(), "aggregate")){
            System.out.println(commandSucceeded.getResponse().getDocument("cursor").get("firstBatch").asArray().getValues().size());
        } else if (Objects.equals(commandSucceeded.getCommandName(), "update")) {
            System.out.println(commandSucceeded.getResponse().get("nModified").asInt32().getValue());
        } else if (Objects.equals(commandSucceeded.getCommandName(), "insert") || Objects.equals(commandSucceeded.getCommandName(), "delete")) {
            System.out.println(commandSucceeded.getResponse().get("n").asInt32().getValue());
        }
    }

    @Override
    public void commandFailed(CommandFailed commandFailed) {
        String commandName = commandFailed.getCommandName();
        Throwable throwable = commandFailed.getThrowable();
        System.out.println("error ==> : " + commandName + ", " + throwable.getMessage());
    }

    @Override
    public int getOrder() {
        return OrderCache.LOG_ORDER;
    }
}
