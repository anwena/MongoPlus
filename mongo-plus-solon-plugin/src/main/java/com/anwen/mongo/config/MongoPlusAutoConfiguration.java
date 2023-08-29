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

    public MongoPlusAutoConfiguration(SqlOperation sqlOperation){
        this.sqlOperation = sqlOperation;
        AopContext context = Solon.context();
        new Reflections("").getSubTypesOf(ServiceImpl.class).forEach(clazz -> {
            ServiceImpl<?> serviceImpl = context.getBean(clazz);
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
