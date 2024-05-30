package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.*;
import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.handlers.DocumentHandler;
import com.anwen.mongo.handlers.MetaObjectHandler;
import com.anwen.mongo.incrementer.IdentifierGenerator;
import com.anwen.mongo.incrementer.id.IdWorker;
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
import com.anwen.mongo.strategy.mapping.MappingStrategy;
import com.anwen.mongo.toolkit.CollUtil;
import org.noear.solon.Solon;
import org.noear.solon.core.AppContext;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.stream.Collectors;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
public class MongoPlusAutoConfiguration {

    private final BaseMapper baseMapper;

    private final MongoDBLogProperty mongoDBLogProperty;

    private final MongoDBCollectionProperty mongoDBCollectionProperty;

    private final MongoLogicDelProperty mongoLogicDelProperty;

    Log log = LogFactory.getLog(MongoPlusAutoConfiguration.class);

    public MongoPlusAutoConfiguration(BaseMapper baseMapper,
                                      MongoDBLogProperty mongoDBLogProperty,
                                      MongoDBCollectionProperty mongoDBCollectionProperty,
                                      MongoLogicDelProperty mongoLogicDelProperty){
        mongoDBCollectionProperty = Optional.ofNullable(mongoDBCollectionProperty).orElseGet(MongoDBCollectionProperty::new);
        mongoLogicDelProperty = Optional.ofNullable(mongoLogicDelProperty).orElseGet(MongoLogicDelProperty::new);
        this.mongoDBLogProperty = mongoDBLogProperty;
        this.mongoDBCollectionProperty = mongoDBCollectionProperty;
        this.baseMapper = baseMapper;
        this.mongoLogicDelProperty = mongoLogicDelProperty;
        AppContext context = Solon.context();
        context.subBeansOfType(IService.class, bean -> {
            if (bean instanceof ServiceImpl){
                ServiceImpl<?> service = (ServiceImpl<?>) bean;
                setExecute(service,bean.getGenericityClass());
                setLogicFiled(service.getGenericityClass());
            }
        });
        //拿到转换器
        setConversion(context);
        //拿到自动填充处理器
        setMetaObjectHandler(context);
        //拿到Document处理器
        setDocumentHandler(context);
        //拿到监听器
        setListener(context);
        //拿到拦截器
        setInterceptor(context);
        //拿到替换器
        setReplacer(context);
        //拿到属性映射器
        setMapping(context);
        //拿到自定义id生成
        setIdGenerator(context);
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

    /**
     * 从Bean中拿到Document的处理器
     * @author JiaChaoYang
     * @date 2023/11/23 12:56
    */
    private void setExecute(ServiceImpl<?> serviceImpl, Class<?> clazz) {
        serviceImpl.setClazz(clazz);
        serviceImpl.setBaseMapper(baseMapper);
    }

    /**
     * 从Bean中拿到转换器
     * @author JiaChaoYang
     * @date 2023/10/19 12:49
     */
    @SuppressWarnings("unchecked")
    private void setConversion(AppContext context){
        context.getBeansOfType(ConversionStrategy.class).forEach(conversionStrategy -> {
            try {
                Type[] genericInterfaces = conversionStrategy.getClass().getGenericInterfaces();
                for (Type anInterface : genericInterfaces) {
                    ParameterizedType parameterizedType = (ParameterizedType) anInterface;
                    if (parameterizedType.getRawType().equals(ConversionStrategy.class)){
                        Class<?> clazz = (Class<?>) parameterizedType.getActualTypeArguments()[0];
                        ConversionCache.putConversionStrategy(clazz,conversionStrategy);
                        break;
                    }
                }
            }catch (Exception e){
                log.error("Unknown converter type");
                throw new MongoPlusConvertException("Unknown converter type");
            }
        });
    }

    /**
     * 从Bean中拿到自动填充策略
     * @author JiaChaoYang
     * @date 2023/11/21 12:18
     */
    private void setMetaObjectHandler(AppContext context){
        context.getBeansOfType(MetaObjectHandler.class).forEach(metaObjectHandler -> HandlerCache.metaObjectHandler = metaObjectHandler);
    }

    private void setDocumentHandler(AppContext appContext){
        appContext.getBeansOfType(DocumentHandler.class).forEach(documentHandler -> HandlerCache.documentHandler = documentHandler);
    }

    /**
     * 从Bean中拿到监听器
     * @author JiaChaoYang
     * @date 2023/11/22 18:39
     */
    private void setListener(AppContext context){
        List<Listener> listeners = ListenerCache.listeners;
        if (mongoDBLogProperty.getLog()){
            listeners.add(new LogListener(mongoDBLogProperty.getPretty()));
        }
        if (mongoDBCollectionProperty.getBlockAttackInner()){
            listeners.add(new BlockAttackInnerListener());
        }
        List<Listener> listenerCollection = context.getBeansOfType(Listener.class);
        if (CollUtil.isNotEmpty(listenerCollection)){
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
    private void setInterceptor(AppContext context) {
        List<Interceptor> beansOfType = context.getBeansOfType(Interceptor.class);
        if (CollUtil.isNotEmpty(beansOfType)) {
            beansOfType = beansOfType.stream().sorted(Comparator.comparing(Interceptor::order)).collect(Collectors.toList());
        }
        InterceptorCache.interceptors = new ArrayList<>(beansOfType);
    }

    /**
     * 从bean 容器中获取替换器
     *
     * @author loser
     */
    private void setReplacer(AppContext context) {
        Collection<Replacer> replacers = context.getBeansOfType(Replacer.class);
        if (CollUtil.isNotEmpty(replacers)) {
            replacers = replacers.stream().sorted(Comparator.comparing(Replacer::order)).collect(Collectors.toList());
        }
        ExecutorReplacerCache.replacers = new ArrayList<>(replacers);
    }

    /**
     * 从Bean中拿到映射器
     *
     * @author JiaChaoYang
     * @date 2024/3/17 0:30
     */
    private void setMapping(AppContext context) {
        context.getBeansOfType(MappingStrategy.class).forEach(mappingStrategy -> {
            try {
                if (mappingStrategy.getClass().isInterface()){
                    MappingCache.putMappingStrategy(mappingStrategy.getClass(), mappingStrategy);
                    return;
                }
                Type[] genericInterfaces = mappingStrategy.getClass().getGenericInterfaces();
                for (Type anInterface : genericInterfaces) {
                    ParameterizedType parameterizedType = (ParameterizedType) anInterface;
                    if (parameterizedType.getRawType().equals(MappingStrategy.class)) {
                        Class<?> clazz = (Class<?>) parameterizedType.getActualTypeArguments()[0];
                        MappingCache.putMappingStrategy(clazz, mappingStrategy);
                        break;
                    }
                }
            } catch (Exception e) {
                log.error("Unknown Mapping type", e);
                throw new MongoPlusConvertException("Unknown converter type");
            }
        });
    }

    /**
     * 自定义id生成器
     * @author anwen
     * @date 2024/5/30 下午1:35
     */
    private void setIdGenerator(AppContext context) {
        try {
            IdWorker.setIdentifierGenerator(context.getBean(IdentifierGenerator.class));
        } catch (Exception ignored){}
    }

}
