package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.AutoFillCache;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.handlers.MetaObjectHandler;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.proxy.MapperInvokeHandler;
import com.anwen.mongo.registrar.MongoPlusMapperRegistrar;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.mongodb.MongoException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Import;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
@Import(MongoPlusMapperRegistrar.class)
public class MongoPlusAutoConfiguration implements InitializingBean {

    private final SqlExecute sqlExecute;

    private final ApplicationContext applicationContext;

    Logger logger = LoggerFactory.getLogger(MongoPlusAutoConfiguration.class);

    public MongoPlusAutoConfiguration(SqlExecute sqlExecute, ApplicationContext applicationContext) {
        this.sqlExecute = sqlExecute;
        this.applicationContext = applicationContext;
        setConversion();
        setMetaObjectHandler();
    }

    @Override
    public void afterPropertiesSet() {
        applicationContext.getBeansOfType(IService.class)
                .values()
                .stream()
                .filter(s -> s instanceof ServiceImpl)
                .forEach(s -> setSqlExecute((ServiceImpl<?>) s, s.getGenericityClazz()));

        // baseMapperProxy设置sqlExecute
        applicationContext.getBeansOfType(BaseMapper.class)
                .values()
                .forEach(m -> {
                    MapperInvokeHandler<?> invocationHandler = (MapperInvokeHandler<?>) Proxy.getInvocationHandler(m);
                    invocationHandler.setSqlExecute(sqlExecute);
                });
    }

    private void setSqlExecute(ServiceImpl<?> serviceImpl,Class<?> clazz) {
        sqlExecute.init(clazz);
        serviceImpl.setClazz(clazz);
        serviceImpl.setSqlOperation(sqlExecute);
    }

    /**
     * 从Bean中拿到转换器
     * @author JiaChaoYang
     * @date 2023/10/19 12:49
    */
    @SuppressWarnings("unchecked")
    private void setConversion(){
        applicationContext.getBeansOfType(ConversionStrategy.class).values().forEach(conversionStrategy -> {
            try {
                Type[] genericInterfaces = conversionStrategy.getClass().getGenericInterfaces();
                for (Type anInterface : genericInterfaces) {
                    ParameterizedType parameterizedType = (ParameterizedType) anInterface;
                    if (parameterizedType.getRawType().equals(ConversionStrategy.class)){
                        Class<?> clazz = (Class<?>) parameterizedType.getActualTypeArguments()[0];
                        ConversionService.appendConversion(clazz,conversionStrategy);
                        break;
                    }
                }
            }catch (Exception e){
                logger.error("Unknown converter type",e);
                throw new MongoException("Unknown converter type");
            }
        });
    }

    /**
     * 从Bean中拿到自动填充策略
     * @author JiaChaoYang
     * @date 2023/11/21 12:18
    */
    private void setMetaObjectHandler(){
        applicationContext.getBeansOfType(MetaObjectHandler.class).values().forEach(metaObjectHandler -> {
            AutoFillCache.metaObjectHandler = metaObjectHandler;
        });
    }

}
