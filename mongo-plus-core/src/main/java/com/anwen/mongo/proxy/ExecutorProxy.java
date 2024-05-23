package com.anwen.mongo.proxy;

import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.model.AggregateBasicDBObject;
import com.anwen.mongo.model.MutablePair;
import com.anwen.mongo.model.QueryParam;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.WriteModel;
import org.bson.Document;
import org.bson.conversions.Bson;

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

    private final Execute target;

    public ExecutorProxy(Execute target) {
        this.target = target;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String name = method.getName();
        InterceptorCache.interceptors.forEach(interceptor -> {
            if (name.equals(ExecuteMethodEnum.SAVE.getMethod())){
                args[0] = interceptor.executeSave((List<Document>) args[0]);
            }
            if (name.equals(ExecuteMethodEnum.REMOVE.getMethod())){
                args[0] = interceptor.executeRemove((Bson) args[0]);
            }
            if (name.equals(ExecuteMethodEnum.UPDATE.getMethod())){
                MutablePair<Bson, Bson> bsonBsonMutablePair = interceptor.executeUpdate((Bson) args[0], (Bson) args[1]);
                args[0] = bsonBsonMutablePair.getLeft();
                args[1] = bsonBsonMutablePair.getRight();
            }
            if (name.equals(ExecuteMethodEnum.QUERY.getMethod())){
                QueryParam queryParam = interceptor.executeQuery((Bson) args[0], (BasicDBObject) args[1], (BasicDBObject) args[2]);
                args[0] = queryParam.getQuery();
                args[1] = queryParam.getProjection();
                args[2] = queryParam.getSort();
            }
            if (name.equals(ExecuteMethodEnum.AGGREGATE.getMethod())){
                args[0] = interceptor.executeAggregate((List<AggregateBasicDBObject>) args[0]);
            }
            if (name.equals(ExecuteMethodEnum.COUNT.getMethod())){
                MutablePair<BasicDBObject, CountOptions> basicDBObjectCountOptionsMutablePair = interceptor.executeCount((BasicDBObject) args[0], (CountOptions) args[1]);
                args[0] = basicDBObjectCountOptionsMutablePair.getLeft();
                args[1] = basicDBObjectCountOptionsMutablePair.getRight();
            }
            if (name.equals(ExecuteMethodEnum.BULK_WRITE.getMethod())){
                args[0] = interceptor.executeBulkWrite((List<WriteModel<Document>>) args[0]);
            }
        });
        return method.invoke(target, args);
    }
}
