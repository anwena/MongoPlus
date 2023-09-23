package com.anwen.mongo.proxy;

import com.anwen.mongo.execute.SqlExecute;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 泛型entity动态代理
 * @date 2023-09-05 15:27
 **/
public class MongoEntityDynamicProxy implements InvocationHandler {

    private final Object object;

    private final SqlExecute sqlExecute;

    private final Class<?> clazz;

    public MongoEntityDynamicProxy(Object object, SqlExecute sqlExecute,Class<?> clazz) {
        this.object = object;
        this.sqlExecute = sqlExecute;
        this.clazz = clazz;
    }

    //生成代理类的方法
    public Object getInstance(){
        return Proxy.newProxyInstance(object.getClass().getClassLoader()
                ,object.getClass().getInterfaces(), this);
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        return method.invoke(proxy, args);
    }
}
