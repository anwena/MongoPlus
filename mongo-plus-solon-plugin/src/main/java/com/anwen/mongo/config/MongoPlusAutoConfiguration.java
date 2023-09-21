package com.anwen.mongo.config;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import org.noear.solon.Solon;
import org.noear.solon.annotation.Inject;
import org.noear.solon.core.AopContext;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration {

    private final SqlExecute sqlExecute;

    public MongoPlusAutoConfiguration(@Inject SqlExecute sqlExecute){
        this.sqlExecute = sqlExecute;
        AopContext context = Solon.context();
        context.getBeansOfType(IService.class)
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
