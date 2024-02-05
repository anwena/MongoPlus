package com.anwen.mongo.proxy;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.model.AggregateBasicDBObject;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.WriteModel;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.List;

/**
 * 执行器代理
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-02-05 09:15
 **/
public class ExecutorProxy implements InvocationHandler {

    Logger logger = LoggerFactory.getLogger(ExecutorProxy.class);

    private final Execute target;

    public ExecutorProxy(Execute target) {
        this.target = target;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String name = method.getName();
        InterceptorCache.interceptors.forEach(interceptor -> {
            if (name.equals(ExecuteMethodEnum.SAVE.getMethod())){
                interceptor.executeSave((List<Document>) args[0]);
            }
            if (name.equals(ExecuteMethodEnum.REMOVE.getMethod())){
                interceptor.executeRemove((Bson) args[0]);
            }
            if (name.equals(ExecuteMethodEnum.UPDATE.getMethod())){
                interceptor.executeUpdate((Bson) args[0], (Bson) args[1]);
            }
            if (name.equals(ExecuteMethodEnum.QUERY.getMethod())){
                interceptor.executeQuery((Bson) args[0], (BasicDBObject) args[1], (BasicDBObject) args[2]);
            }
            if (name.equals(ExecuteMethodEnum.AGGREGATE.getMethod())){
                interceptor.executeAggregate((List<AggregateBasicDBObject>) args[0]);
            }
            if (name.equals(ExecuteMethodEnum.COUNT.getMethod())){
                interceptor.executeCount((BasicDBObject) args[0], (CountOptions) args[1]);
            }
            if (name.equals(ExecuteMethodEnum.BULK_WRITE.getMethod())){
                interceptor.executeBulkWrite((List<WriteModel<Document>>) args[0]);
            }
        });
        return method.invoke(target, args);
    }
}
