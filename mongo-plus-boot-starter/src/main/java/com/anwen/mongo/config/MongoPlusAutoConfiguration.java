package com.anwen.mongo.config;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
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
        applicationContext.getBeansOfType(IService.class)
                .values()
                .stream()
                .filter(s -> s instanceof ServiceImpl)
                .forEach(s -> setSqlExecute((ServiceImpl<?>) s, s.getGenericityClazz()));
    }

    private void setSqlExecute(ServiceImpl<?> serviceImpl,Class<?> clazz) {
        sqlExecute.init(clazz);
        serviceImpl.setClazz(clazz);
        serviceImpl.setSqlOperation(sqlExecute);
    }

}
