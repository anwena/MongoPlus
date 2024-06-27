package com.anwen.mongo.strategy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;
import com.mongodb.client.MongoCollection;
import org.bson.Document;
import org.bson.conversions.Bson;


/**
 * REMOVE 策略执行器
 *
 * @author loser
 * @date 2024/4/30
 */
@SuppressWarnings("unchecked")
public class RemoveExecutorStrategy implements MethodExecutorStrategy {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.REMOVE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        args[0] = interceptor.executeRemove((Bson) args[0]);
        args[0] = interceptor.executeRemove((Bson) args[0], (MongoCollection<Document>) args[args.length-1]);
    }

}
