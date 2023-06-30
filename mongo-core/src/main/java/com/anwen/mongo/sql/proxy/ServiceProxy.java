package com.anwen.mongo.sql.proxy;

import com.anwen.mongo.annotation.proxy.AutoSetMongoEntity;
import com.anwen.mongo.sql.SqlOperation;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * ServiceImpl的动态代理类，用于调用设置泛型方法
 * @author JiaChaoYang
 * @date 2023/6/30/030 0:12
*/
public class ServiceProxy<T> implements InvocationHandler {

    private final Object target;

    private SqlOperation<T> sqlOperation;

    public ServiceProxy(Object target,SqlOperation<T> sqlOperation) {
        this.target = target;
        this.sqlOperation = sqlOperation;
    }

/*    public static <T> T createProxy(T target) {
        Class<?>[] interfaces = target.getClass().getInterfaces();
        ServiceProxy proxy = new ServiceProxy(target);
        return (T) Proxy.newProxyInstance(target.getClass().getClassLoader(), interfaces, proxy);
    }*/

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        Method targetMethod = target.getClass().getMethod(method.getName(), method.getParameterTypes());
        if (targetMethod.isAnnotationPresent(AutoSetMongoEntity.class)) {
            setMongoEntity();
        }
        return method.invoke(target, args);
    }

    private void setMongoEntity() {
        // 在这里实现 setMongoEntity() 方法的逻辑
        // 这里可以调用 setMongoEntity() 方法来设置 Mongo 实体
    }

}
