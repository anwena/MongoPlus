package com.anwen.mongo.config;

import com.anwen.mongo.execute.SqlOperation;
import com.anwen.mongo.mapper.MongoPlusBeanMapper;
import com.anwen.mongo.service.impl.ServiceImpl;
import org.reflections.Reflections;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Configuration;

import javax.annotation.Resource;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Set;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration implements InitializingBean {

    @Resource
    private SqlOperation sqlOperation;

    @Resource
    private ApplicationContext applicationContext;

    private Set<Class<? extends ServiceImpl>> loadClassByLoader(){
        Reflections reflections = new Reflections("");
        return reflections.getSubTypesOf(ServiceImpl.class);
    }

    @Override
    public void afterPropertiesSet() {
        loadClassByLoader().forEach(clazz -> {
            String className = clazz.getSimpleName();
            String firstChar = className.substring(0, 1).toLowerCase();
            ServiceImpl<?> serviceImpl = (ServiceImpl<?>) applicationContext.getBean(firstChar + className.substring(1));
            Class<?> genericityClass = serviceImpl.getGenericityClazz();
            setOperation(serviceImpl,genericityClass);
        });/*
        MongoPlusBeanMapper mongoPlusBeanMapper = applicationContext.getBean(MongoPlusBeanMapper.class);
        Class clazz = getGenericityClazz(mongoPlusBeanMapper);
        setOperation(mongoPlusBeanMapper,clazz);*/
    }

    private void setOperation(ServiceImpl<?> serviceImpl,Class clazz) {
        sqlOperation.setMongoEntity(clazz);
        sqlOperation.init();
        serviceImpl.setClazz(clazz);
        serviceImpl.setSqlOperation(sqlOperation);
    }

    public <T> Class getGenericityClazz(T genericClass) {
        Type genericSuperclass = genericClass.getClass().getGenericSuperclass();

        if (genericSuperclass instanceof ParameterizedType) {
            Type[] actualTypeArguments = ((ParameterizedType) genericSuperclass).getActualTypeArguments();
            if (actualTypeArguments.length > 0) {
                Type typeArgument = actualTypeArguments[0];
                try {
                    if (typeArgument instanceof Class<?>) {
                        return (Class) typeArgument;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        return null;
    }

}
