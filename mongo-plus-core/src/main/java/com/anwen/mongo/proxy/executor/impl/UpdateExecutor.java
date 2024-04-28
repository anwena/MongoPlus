package com.anwen.mongo.proxy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.proxy.executor.MethodExecutor;
import org.bson.conversions.Bson;

public class UpdateExecutor implements MethodExecutor {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.UPDATE;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        MutablePair<Bson, Bson> bsonBsonMutablePair = interceptor.executeUpdate((Bson) args[0], (Bson) args[1]);
        args[0] = bsonBsonMutablePair.getLeft();
        args[1] = bsonBsonMutablePair.getRight();
    }

}
