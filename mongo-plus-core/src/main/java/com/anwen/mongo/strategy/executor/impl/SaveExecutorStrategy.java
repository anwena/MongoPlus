package com.anwen.mongo.strategy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;
import com.mongodb.client.MongoCollection;
import org.bson.Document;

import java.util.List;

/**
 * SAVE 策略执行器
 *
 * @author loser
 * @date 2024/4/30
 */
@SuppressWarnings("unchecked")
public class SaveExecutorStrategy implements MethodExecutorStrategy {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.SAVE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        args[0] = interceptor.executeSave((List<Document>) args[0]);
        args[0] = interceptor.executeSave((List<Document>) args[0], (MongoCollection<Document>) args[1]);
    }

}
