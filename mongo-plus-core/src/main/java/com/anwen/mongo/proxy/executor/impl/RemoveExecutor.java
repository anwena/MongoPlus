package com.anwen.mongo.proxy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.proxy.executor.MethodExecutor;
import org.bson.conversions.Bson;

public class RemoveExecutor implements MethodExecutor {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.REMOVE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        args[0] = interceptor.executeRemove((Bson) args[0]);
    }

}
