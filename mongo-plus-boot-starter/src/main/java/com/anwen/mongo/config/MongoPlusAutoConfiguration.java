package com.anwen.mongo.config;

import com.anwen.mongo.annotation.MongoConversion;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration implements InitializingBean {

    private final SqlExecute sqlExecute;

    private final ApplicationContext applicationContext;

    Logger logger = LoggerFactory.getLogger(MongoPlusAutoConfiguration.class);

    public MongoPlusAutoConfiguration(SqlExecute sqlExecute, ApplicationContext applicationContext) {
        this.sqlExecute = sqlExecute;
        this.applicationContext = applicationContext;
        setConversion();

    }

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

    /**
     * 从Bean中拿到转换器
     * @author JiaChaoYang
     * @date 2023/10/19 12:49
    */
    private void setConversion(){
        applicationContext.getBeansOfType(ConversionStrategy.class).values().forEach(conversionStrategy -> {
            MongoConversion mongoConversion = conversionStrategy.getClass().getAnnotation(MongoConversion.class);
            if (null == mongoConversion){
                logger.error("Received the converter, but did not use the @MongoConversion annotation, so the type is unknown, ConversionStrategy: {}",conversionStrategy.getClass().getName());
            }else {
                ConversionService.appendConversion(mongoConversion.type(), conversionStrategy);
            }
        });
    }

}
