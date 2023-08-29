package com.anwen.mongo.config;

import com.anwen.mongo.execute.SqlOperation;
import com.anwen.mongo.service.impl.ServiceImpl;
import org.noear.solon.Solon;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;
import org.noear.solon.core.AopContext;
import org.noear.solon.core.BeanWrap;
import org.reflections.Reflections;

import java.util.Set;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration {

    private final SqlOperation sqlOperation;

    private Set<Class<? extends ServiceImpl>> loadClassByLoader(){
        Reflections reflections = new Reflections("");
        return reflections.getSubTypesOf(ServiceImpl.class);
    }

    public MongoPlusAutoConfiguration(SqlOperation sqlOperation){
        this.sqlOperation = sqlOperation;
        AopContext context = Solon.context();
        loadClassByLoader().forEach(clazz -> {
            String className = clazz.getSimpleName();
            String firstChar = className.substring(0, 1).toLowerCase();
            ServiceImpl<?> serviceImpl = context.getBean(firstChar + className.substring(1));
            if (null == serviceImpl){
                BeanWrap beanWrap = context.beanMake(clazz);
                serviceImpl = beanWrap.get();
            }
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
