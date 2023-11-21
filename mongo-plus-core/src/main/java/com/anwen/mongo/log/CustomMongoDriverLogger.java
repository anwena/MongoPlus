package com.anwen.mongo.log;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.mongodb.event.CommandFailedEvent;
import com.mongodb.event.CommandListener;
import com.mongodb.event.CommandStartedEvent;
import com.mongodb.event.CommandSucceededEvent;

import java.util.Objects;

public class CustomMongoDriverLogger implements CommandListener {

    /**
     * 是否格式化
     * @author JiaChaoYang
     * @date 2023/8/29 0:53
    */
    private final Boolean format;

    public CustomMongoDriverLogger(Boolean format) {
        this.format = format;
    }

    /**
     * 处理命令开始事件
     * @author JiaChaoYang
     * @date 2023/6/28/028 21:32
    */
    @Override
    public void commandStarted(CommandStartedEvent event) {
        System.out.println(event.getCommandName()+" Statement Execution ==> ");
        System.out.println(formattingStatement(event.getCommand().toJson()));
    }

    /**
     * 处理命令成功事件
     * @author JiaChaoYang
     * @date 2023/6/28/028 21:32
    */
    @Override
    public void commandSucceeded(CommandSucceededEvent event) {
        if (Objects.equals(event.getCommandName(), "find") || Objects.equals(event.getCommandName(), "aggregate")){
            System.out.println(event.getCommandName()+" results of execution ==> ");
            System.out.println(event.getResponse().getDocument("cursor").get("firstBatch").asArray().getValues().size());
        } else if (Objects.equals(event.getCommandName(), "insert") || Objects.equals(event.getCommandName(), "delete")) {
            System.out.println(event.getCommandName()+" results of execution ==> ");
            System.out.println(event.getResponse().get("n").asInt32().getValue());
        }
    }

    /**
     * 处理命令失败事件
     * @author JiaChaoYang
     * @date 2023/6/28/028 21:32
    */
    @Override
    public void commandFailed(CommandFailedEvent event) {
        String commandName = event.getCommandName();
        Throwable throwable = event.getThrowable();
        System.out.println("error ==> : " + commandName + ", " + throwable.getMessage());
    }

    private String formattingStatement(String statement){
        return format ? JSON.toJSONString(JSONObject.parse(statement), SerializerFeature.PrettyFormat, SerializerFeature.WriteMapNullValue,
                SerializerFeature.WriteDateUseDateFormat) : statement;
    }

}
