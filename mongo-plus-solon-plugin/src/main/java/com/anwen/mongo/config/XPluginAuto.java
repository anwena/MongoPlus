package com.anwen.mongo.config;

import com.anwen.mongo.annotation.transactional.MongoTransactional;
import com.anwen.mongo.cache.MongoClientCache;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import org.noear.solon.core.AopContext;
import org.noear.solon.core.Plugin;

/**
 * 使用插件处理配置
 * @author JiaChaoYang
 **/
public class XPluginAuto implements Plugin {
    @Override
    public void start(AopContext context) throws Throwable {
        //mongo-plus配置
        context.beanMake(MongoPlusConfiguration.class);
        context.beanInterceptorAdd(MongoTransactional.class,new MongoTransactionalAspect(MongoClientCache.mongoClient));
    }
}
