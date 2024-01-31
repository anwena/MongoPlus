package com.anwen.mongo.config;

import com.anwen.mongo.annotation.transactional.MongoTransactional;
import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.anwen.mongo.property.MongoDBFieldProperty;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.client.MongoClient;
import org.noear.solon.core.AppContext;
import org.noear.solon.core.Plugin;

/**
 * 使用插件处理配置
 * @author JiaChaoYang
 **/
public class XPluginAuto implements Plugin {
    @Override
    public void start(AppContext context) throws Throwable {
        //mongo-plus插件配置
        context.beanMake(MongoPlusConfiguration.class);
        context.getBeanAsync(MongoClient.class,bean -> context.beanInterceptorAdd(MongoTransactional.class,new MongoTransactionalAspect(bean)));
        context.beanMake(MongoDBFieldProperty.class);
    }
}
