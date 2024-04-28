package com.anwen.mongo.proxy.executor.impl;

import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.model.QueryParam;
import com.anwen.mongo.proxy.executor.MethodExecutor;
import com.mongodb.BasicDBObject;
import org.bson.conversions.Bson;

public class QueryExecutor implements MethodExecutor {

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
