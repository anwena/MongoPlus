package com.anwen.mongo.strategy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;
import com.mongodb.client.MongoCollection;
import org.bson.Document;

import java.util.List;

/**
 * AGGREGATE 策略执行器
 *
 * @author loser
 * @date 2024/4/30
 */
public class AggregateExecutorStrategy implements MethodExecutorStrategy {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.AGGREGATE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        args[0] = interceptor.executeAggregate((List<AggregateBasicDBObject>) args[0]);
        args[0] = interceptor.executeAggregate((List<AggregateBasicDBObject>) args[0], (MongoCollection<Document>) args[1]);
    }

}
