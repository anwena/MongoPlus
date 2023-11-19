package com.anwen.mongo.proxy;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.mapper.BaseMapper;
import org.springframework.beans.factory.FactoryBean;

import java.lang.reflect.*;


/**
 * @Author Bomber
 * @Description bean动态代理类
 * @Date 2023/11/16 19:50
 * @Version 1.0
 */
public class MapperProxy<M extends BaseMapper<T>, T> implements FactoryBean<M> {

    // mapper类型
    private Class<M> mapperClass;

    // 动态代理执行器
    private final MapperInvokeHandler<T> mapperInvokeHandler = new MapperInvokeHandler<>();

    public void setMapperClass(Class<M> mapperClass) {
        this.mapperClass = mapperClass;
    }

    @Override
    @SuppressWarnings("unchecked")
    public M getObject() throws Exception {
        // 实例化代理
        return (M) Proxy.newProxyInstance(mapperClass.getClassLoader(), new Class[]{mapperClass}, mapperInvokeHandler);
    }

    @Override
    public Class<M> getObjectType() {
        return mapperClass;
    }
}
