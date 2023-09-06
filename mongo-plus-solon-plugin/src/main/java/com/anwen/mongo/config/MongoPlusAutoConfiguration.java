package com.anwen.mongo.config;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.service.impl.ServiceImpl;
import org.noear.solon.Solon;
import org.noear.solon.core.AopContext;
import org.noear.solon.core.BeanWrap;
import org.reflections.Reflections;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration {

    private final SqlExecute sqlExecute;

    public MongoPlusAutoConfiguration(SqlExecute sqlExecute){
        this.sqlExecute = sqlExecute;
        AopContext context = Solon.context();
        new Reflections("").getSubTypesOf(ServiceImpl.class).forEach(clazz -> {
            ServiceImpl<?> serviceImpl = context.getBean(clazz);
            if (null == serviceImpl){
                BeanWrap beanWrap = context.beanMake(clazz);
                serviceImpl = beanWrap.get();
            }
            Class<?> genericityClass = serviceImpl.getGenericityClazz();
            setSqlExecute(serviceImpl,genericityClass);
        });
    }

    private void setSqlExecute(ServiceImpl<?> serviceImpl,Class<?> clazz) {
        sqlExecute.init(clazz);
        serviceImpl.setClazz(clazz);
        serviceImpl.setSqlOperation(sqlExecute);
    }

}
