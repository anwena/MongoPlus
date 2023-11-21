package com.anwen.mongo.proxy;

import com.anwen.mongo.mapper.BaseMapper;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;


/**
 * @Author Bomber
 * @Description bean动态代理类
 * @Date 2023/11/16 19:50
 * @Version 1.0
 */
public class MapperProxy<M extends BaseMapper<T>, T> {

    // mapper类型
    private Class<M> mapperClass;

    // 动态代理执行器
    private final MapperInvokeHandler<T> mapperInvokeHandler = new MapperInvokeHandler<>();

    public void setMapperClass(Class<M> mapperClass) {
        this.mapperClass = mapperClass;
        this.mapperInvokeHandler.setClazz(getEntityClassByMapperClass());
    }

    /**
     * 通过mapperClass获取实体类型
     * @return
     */
    @SuppressWarnings("unchecked")
    private Class<T> getEntityClassByMapperClass() {
        Type[] interfaces = mapperClass.getGenericInterfaces();
        for (Type type : interfaces) {
            if (type instanceof ParameterizedType && type.getTypeName().startsWith("com.anwen.mongo.mapper.BaseMapper")) {
                ParameterizedType parameterizedType = (ParameterizedType) type;
                String entityClassName = parameterizedType.getActualTypeArguments()[0].getTypeName();
                try {
                    return (Class<T>) Class.forName(entityClassName);
                } catch (ClassNotFoundException e) {
                    e.printStackTrace();
                }
            }
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public M getObject() throws Exception {
        // 实例化代理
        return (M) Proxy.newProxyInstance(mapperClass.getClassLoader(), new Class[]{mapperClass}, mapperInvokeHandler);
    }

    public Class<M> getObjectType() {
        return mapperClass;
    }
}
