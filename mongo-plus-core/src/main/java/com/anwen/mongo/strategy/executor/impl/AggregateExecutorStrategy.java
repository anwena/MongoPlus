package com.anwen.mongo.strategy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;

import java.util.List;

public class AggregateExecutorStrategy implements MethodExecutorStrategy {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.AGGREGATE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        args[0] = interceptor.executeAggregate((List<AggregateBasicDBObject>) args[0]);
    }

}
