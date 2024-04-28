package com.anwen.mongo.strategy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.QueryParam;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;
import com.mongodb.BasicDBObject;
import org.bson.conversions.Bson;

public class QueryExecutorStrategy implements MethodExecutorStrategy {

    @Override
    public ExecuteMethodEnum method() {
        return ExecuteMethodEnum.QUERY;
    }

    @Override
    public void invoke(Interceptor interceptor, Object[] args) {
        QueryParam queryParam = interceptor.executeQuery((Bson) args[0], (BasicDBObject) args[1], (BasicDBObject) args[2]);
        args[0] = queryParam.getQuery();
        args[1] = queryParam.getProjection();
        args[2] = queryParam.getSort();
    }

}
