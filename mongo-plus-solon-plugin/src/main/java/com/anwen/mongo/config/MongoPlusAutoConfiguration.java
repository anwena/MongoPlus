package com.anwen.mongo.config;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.mongodb.MongoException;
import org.noear.solon.Solon;
import org.noear.solon.annotation.Inject;
import org.noear.solon.core.AopContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

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
            if (bean instanceof ServiceImpl){
                setSqlExecute((ServiceImpl<?>) bean,bean.getGenericityClazz());
            }
        });
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
    @SuppressWarnings("unchecked")
    private void setConversion(AopContext context){
        context.getBeansOfType(ConversionStrategy.class).forEach(conversionStrategy -> {
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
                logger.error("Unknown converter type");
                throw new MongoException("Unknown converter type");
            }
        });
    }

}
