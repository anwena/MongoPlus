package com.anwen.mongo.proxy;

import com.anwen.mongo.cache.global.ClassLogicDeleteCache;
import com.anwen.mongo.cache.global.ExecutorProxyCache;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.enums.ExecuteMethodEnum;
import com.anwen.mongo.enums.SpecialConditionEnum;
import com.anwen.mongo.execute.Execute;
import com.anwen.mongo.logic.LogicDeleteHandler;
import com.anwen.mongo.model.LogicDeleteResult;
import com.anwen.mongo.strategy.executor.MethodExecutorStrategy;
import com.mongodb.BasicDBObject;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.UpdateResult;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Objects;

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
        boolean close = LogicDeleteHandler.close();
        try {
            MethodExecutorStrategy executor = ExecutorProxyCache.EXECUTOR_MAP.get(name);
            if (Objects.nonNull(executor)) {
                InterceptorCache.interceptors.forEach(interceptor -> executor.invoke(interceptor, args));
            }
            // 将删除替换成逻辑删除
            if (!close && name.equals(ExecuteMethodEnum.REMOVE.getMethod())) {
                return doLogicRemove(method, args);
            }
            return method.invoke(target, args);
        } finally {
            if (!close) {
                ClassLogicDeleteCache.clear();
            }
        }

    }

    private Object doLogicRemove(Method method, Object[] args) throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {

        Class<?> clazz = ClassLogicDeleteCache.getLogicCollection();
        if (Objects.isNull(clazz)) {
            return method.invoke(target, args);
        }
        LogicDeleteResult result = LogicDeleteHandler.mapper().get(clazz);
        if (Objects.isNull(result)) {
            return method.invoke(target, args);
        }
        Method updateMethod = target.getClass().getMethod(ExecuteMethodEnum.UPDATE.getMethod(), Bson.class, Bson.class, MongoCollection.class);
        Document updateBasic = new Document(result.getColumn(), result.getLogicDeleteValue());
        BasicDBObject update = new BasicDBObject(SpecialConditionEnum.SET.getCondition(), updateBasic);
        Object[] updateArgs = new Object[]{args[0], update, args[1]};
        UpdateResult res = (UpdateResult) updateMethod.invoke(target, updateArgs);
        return new DeleteResult() {
            @Override
            public boolean wasAcknowledged() {
                return false;
            }

            @Override
            public long getDeletedCount() {
                return res.getModifiedCount();
            }
        };

    }

}
