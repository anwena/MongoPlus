package com.anwen.mongo.config;

import com.anwen.mongo.annotation.MongoConversion;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import org.noear.solon.Solon;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;
import org.noear.solon.core.AopContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration {

    private final SqlExecute sqlExecute;

    Logger logger = LoggerFactory.getLogger(MongoPlusAutoConfiguration.class);

    public MongoPlusAutoConfiguration(@Inject SqlExecute sqlExecute){
        this.sqlExecute = sqlExecute;
        AopContext context = Solon.context();
        context.subBeansOfType(IService.class, bean -> {
            System.out.println("获取到Bean，beanName,"+bean.getClass().getName());
            logger.info("获取到Bean，beanName {}",bean.getClass().getName());
            if (bean instanceof ServiceImpl){
                setSqlExecute((ServiceImpl<?>) bean,bean.getGenericityClazz());
            }
        });
        /*context.getBeansOfType(IService.class)
                .stream()
                .filter(s -> s instanceof ServiceImpl)
                .forEach(s -> setSqlExecute((ServiceImpl<?>) s, s.getGenericityClazz()));*/
        //拿到转换器
        setConversion(context);
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
    private void setConversion(AopContext context){
        context.getBeansOfType(ConversionStrategy.class).forEach(conversionStrategy -> {
            MongoConversion mongoConversion = conversionStrategy.getClass().getAnnotation(MongoConversion.class);
            if (null == mongoConversion){
                logger.error("Received the converter, but did not use the @MongoConversion annotation, so the type is unknown, ConversionStrategy: {}",conversionStrategy.getClass().getName());
            }else {
                ConversionService.appendConversion(mongoConversion.type(), conversionStrategy);
            }
        });
    }

}
