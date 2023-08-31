package com.anwen.mongo.config;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.service.impl.ServiceImpl;
import org.reflections.Reflections;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration implements InitializingBean {

    @Autowired
    private SqlExecute sqlExecute;

    @Autowired
    private ApplicationContext applicationContext;

    @Override
    public void afterPropertiesSet() {
        new Reflections("").getSubTypesOf(ServiceImpl.class).forEach(clazz -> {
            ServiceImpl<?> serviceImpl = (ServiceImpl<?>) applicationContext.getBean(clazz);
            Class<?> genericityClass = serviceImpl.getGenericityClazz();
            setSqlExecute(serviceImpl,genericityClass);
        });
    }

    private void setSqlExecute(ServiceImpl<?> serviceImpl,Class clazz) {
        sqlExecute.setMongoEntity(clazz);
        sqlExecute.init();
        serviceImpl.setClazz(clazz);
        serviceImpl.setSqlOperation(sqlExecute);
    }

}
