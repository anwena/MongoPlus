package com.anwen.mongo.config;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.service.impl.ServiceImpl;
import org.reflections.Reflections;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration implements InitializingBean {

    private static final Logger logger = LoggerFactory.getLogger(MongoPlusAutoConfiguration.class);

    @Autowired
    private SqlExecute sqlExecute;

    @Autowired
    private ApplicationContext applicationContext;

    @Override
    public void afterPropertiesSet() {
        new Reflections("").getSubTypesOf(ServiceImpl.class).forEach(this::processServiceImpl);
    }

    private void processServiceImpl(Class<?> serviceClazz) {
        try {
            ServiceImpl<?> serviceImpl = (ServiceImpl<?>) applicationContext.getBean(serviceClazz);
            Class<?> genericityClass = serviceImpl.getGenericityClazz();
            setSqlExecute(serviceImpl, genericityClass);
        } catch (BeansException e) {
            logger.error("{} is not a spring bean, exception message: {}", serviceClazz, e.getMessage());
        }
    }

    private void setSqlExecute(ServiceImpl<?> serviceImpl,Class<?> clazz) {
        sqlExecute.init(clazz);
        serviceImpl.setClazz(clazz);
        serviceImpl.setSqlOperation(sqlExecute);
    }

}
