package com.anwen.mongo.proxy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.proxy.executor.MethodExecutor;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.CountOptions;

public class CountExecutor implements MethodExecutor {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.COUNT;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        MutablePair<BasicDBObject, CountOptions> basicDBObjectCountOptionsMutablePair = interceptor.executeCount((BasicDBObject) args[0], (CountOptions) args[1]);
        args[0] = basicDBObjectCountOptionsMutablePair.getLeft();
        args[1] = basicDBObjectCountOptionsMutablePair.getRight();
    }

}
