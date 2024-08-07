package com.anwen.mongo.config;

import com.anwen.mongo.annotation.transactional.MongoTransactional;
import com.anwen.mongo.datasource.MongoDataSourceAspect;
import com.anwen.mongo.logic.MongoLogicIgnoreAspect;
import com.anwen.mongo.property.MongoDBFieldProperty;
import com.anwen.mongo.property.MongoEncryptorProperty;
import com.anwen.mongo.tenant.TenantAspect;
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
        context.beanMake(MongoEncryptorProperty.class);
        context.beanMake(MongoDataSourceAspect.class);
        context.beanMake(TenantAspect.class);
        context.beanMake(MongoLogicIgnoreAspect.class);
    }
}
