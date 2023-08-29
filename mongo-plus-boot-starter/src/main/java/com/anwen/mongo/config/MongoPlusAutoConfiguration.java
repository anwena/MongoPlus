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

    @Override
    public void afterPropertiesSet() {
        new Reflections("").getSubTypesOf(ServiceImpl.class).forEach(clazz -> {
            ServiceImpl<?> serviceImpl = (ServiceImpl<?>) applicationContext.getBean(clazz);
            Class<?> genericityClass = serviceImpl.getGenericityClazz();
            setOperation(serviceImpl,genericityClass);
        });
    }

    private void setOperation(ServiceImpl<?> serviceImpl,Class clazz) {
        sqlOperation.setMongoEntity(clazz);
        sqlOperation.init();
        serviceImpl.setClazz(clazz);
        serviceImpl.setSqlOperation(sqlOperation);
    }

}
