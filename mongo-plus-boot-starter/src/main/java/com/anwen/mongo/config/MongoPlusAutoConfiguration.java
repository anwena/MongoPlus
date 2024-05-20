package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.*;
import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.handlers.DocumentHandler;
import com.anwen.mongo.handlers.MetaObjectHandler;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.listener.Listener;
import com.anwen.mongo.listener.business.BlockAttackInnerListener;
import com.anwen.mongo.listener.business.LogListener;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBLogProperty;
import com.anwen.mongo.property.MongoLogicDelProperty;
import com.anwen.mongo.replacer.Replacer;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
import com.anwen.mongo.toolkit.CollUtil;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationContext;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * MongoPlus自动注入配置
 *
 * @author JiaChaoYang
 **/
@EnableConfigurationProperties(MongoDBLogProperty.class)
public class MongoPlusAutoConfiguration implements InitializingBean {

    private final ApplicationContext applicationContext;

    private final MongoDBLogProperty mongodbLogProperty;

    private final MongoDBCollectionProperty mongodbCollectionProperty;

    private final MongoLogicDelProperty mongoLogicDelProperty;

    private final BaseMapper baseMapper;

    Log log = LogFactory.getLog(MongoPlusAutoConfiguration.class);

    public MongoPlusAutoConfiguration(MongoDBLogProperty mongodbLogProperty,
                                      MongoDBCollectionProperty mongodbCollectionProperty,
                                      MongoLogicDelProperty mongoLogicDelProperty,
                                      BaseMapper baseMapper,
                                      ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
        this.mongodbLogProperty = mongodbLogProperty;
        this.mongodbCollectionProperty = mongodbCollectionProperty;
        this.mongoLogicDelProperty = mongoLogicDelProperty;
        setConversion();
        setMetaObjectHandler();
        setDocumentHandler();
        setListener();
        setInterceptor();
        setReplacer();
        this.baseMapper = baseMapper;
    }

    @Override
    public void afterPropertiesSet() {
        Collection<IService> values = applicationContext.getBeansOfType(IService.class).values();
        values.forEach(s -> setExecute((ServiceImpl<?>) s, s.getGenericityClass()));
        setLogicFiled(values.stream().map(IService::getGenericityClass).toArray(Class[]::new));
    }

    /**
     * 配置逻辑删除
     *
     * @param collectionClasses 需要进行逻辑删除的 collection class 集合
     * @author loser
     */
    private void setLogicFiled(Class<?>... collectionClasses) {
        Configuration.builder().logic(this.mongoLogicDelProperty).setLogicFiled(collectionClasses);
    }

    private void setExecute(ServiceImpl<?> serviceImpl, Class<?> clazz) {
        serviceImpl.setClazz(clazz);
        serviceImpl.setBaseMapper(baseMapper);
    }


    /**
     * 从Bean中拿到转换器
     *
     * @author JiaChaoYang
     * @date 2023/10/19 12:49
     */
    private void setConversion() {
        applicationContext.getBeansOfType(ConversionStrategy.class).values().forEach(conversionStrategy -> {
            try {
                if (conversionStrategy.getClass().isInterface()){
                    ConversionCache.putConversionStrategy(conversionStrategy.getClass(), conversionStrategy);
                    return;
                }
                Type[] genericInterfaces = conversionStrategy.getClass().getGenericInterfaces();
                for (Type anInterface : genericInterfaces) {
                    ParameterizedType parameterizedType = (ParameterizedType) anInterface;
                    if (parameterizedType.getRawType().equals(ConversionStrategy.class)) {
                        Class<?> clazz = (Class<?>) parameterizedType.getActualTypeArguments()[0];
                        ConversionCache.putConversionStrategy(clazz, conversionStrategy);
                        break;
                    }
                }
            } catch (Exception e) {
                log.error("Unknown converter type", e);
                throw new MongoPlusConvertException("Unknown converter type");
            }
        });
    }

    /**
     * 从Bean中拿到自动填充策略
     *
     * @author JiaChaoYang
     * @date 2023/11/21 12:18
     */
    private void setMetaObjectHandler() {
        applicationContext.getBeansOfType(MetaObjectHandler.class).values().forEach(metaObjectHandler -> HandlerCache.metaObjectHandler = metaObjectHandler);
    }

    /**
     * 从Bean中拿到Document的处理器
     *
     * @author JiaChaoYang
     * @date 2023/11/23 12:58
     */
    private void setDocumentHandler() {
        applicationContext.getBeansOfType(DocumentHandler.class).values().forEach(documentHandler -> HandlerCache.documentHandler = documentHandler);
    }

    /**
     * 从Bean中拿到监听器
     *
     * @author JiaChaoYang
     * @date 2023/11/22 18:39
     */
    private void setListener() {
        List<Listener> listeners = ListenerCache.listeners;
        if (mongodbLogProperty.getLog()) {
            listeners.add(new LogListener(mongodbLogProperty.getPretty()));
        }
        if (mongodbCollectionProperty.getBlockAttackInner()) {
            listeners.add(new BlockAttackInnerListener());
        }
        Collection<Listener> listenerCollection = applicationContext.getBeansOfType(Listener.class).values();
        if (CollUtil.isNotEmpty(listenerCollection)) {
            listeners.addAll(listenerCollection);
        }
        ListenerCache.sorted();
    }

    /**
     * 从Bean中拿到拦截器
     *
     * @author JiaChaoYang
     * @date 2024/3/17 0:30
     */
    private void setInterceptor() {
        Collection<Interceptor> interceptorCollection = applicationContext.getBeansOfType(Interceptor.class).values();
        if (CollUtil.isNotEmpty(interceptorCollection)) {
            interceptorCollection = interceptorCollection.stream().sorted(Comparator.comparing(Interceptor::order)).collect(Collectors.toList());
        }
        InterceptorCache.interceptors = new ArrayList<>(interceptorCollection);
    }

    /**
     * 从bean 容器中获取替换器
     *
     * @author loser
     */
    private void setReplacer() {
        Collection<Replacer> replacers = applicationContext.getBeansOfType(Replacer.class).values();
        if (CollUtil.isNotEmpty(replacers)) {
            replacers = replacers.stream().sorted(Comparator.comparing(Replacer::order)).collect(Collectors.toList());
        }
        ExecutorReplacerCache.replacers = new ArrayList<>(replacers);
    }

}
