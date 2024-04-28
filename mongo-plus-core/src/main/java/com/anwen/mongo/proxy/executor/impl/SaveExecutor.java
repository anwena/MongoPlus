package com.anwen.mongo.proxy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.proxy.executor.MethodExecutor;
import org.bson.Document;

import java.util.List;

public class SaveExecutor implements MethodExecutor {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.SAVE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        args[0] = interceptor.executeSave((List<Document>) args[0]);
    }

}
