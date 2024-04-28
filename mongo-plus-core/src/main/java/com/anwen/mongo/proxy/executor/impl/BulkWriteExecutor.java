package com.anwen.mongo.proxy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.proxy.executor.MethodExecutor;
import com.mongodb.client.model.WriteModel;
import org.bson.Document;

import java.util.List;

public class BulkWriteExecutor implements MethodExecutor {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.BULK_WRITE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        args[0] = interceptor.executeBulkWrite((List<WriteModel<Document>>) args[0]);
    }

}
