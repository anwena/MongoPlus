package com.anwen.mongo.config;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.cache.global.ExecutorReplacerCache;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.cache.global.ListenerCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.conn.ConnectMongoDB;
import com.anwen.mongo.constant.DataSourceConstant;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.domain.MongoPlusConvertException;
import com.anwen.mongo.handlers.DocumentHandler;
import com.anwen.mongo.handlers.MetaObjectHandler;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.listener.Listener;
import com.anwen.mongo.listener.business.BlockAttackInnerListener;
import com.anwen.mongo.listener.business.LogListener;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBLogProperty;
import com.anwen.mongo.property.MongoLogicDelProperty;
import com.anwen.mongo.replacer.Replacer;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.CollUtil;
import com.mongodb.MongoException;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationContext;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * MongoPlus自动注入配置
 *
 * @author JiaChaoYang
 **/
@EnableConfigurationProperties(MongoDBLogProperty.class)
public class MongoPlusAutoConfiguration implements InitializingBean {

    private final MongoPlusClient mongoPlusClient;

    private final ApplicationContext applicationContext;

    private final MongoDBLogProperty mongodbLogProperty;

    private final MongoDBCollectionProperty mongodbCollectionProperty;

    private final MongoLogicDelProperty mongoLogicDelProperty;

    private final CollectionNameConvert collectionNameConvert;

    private final BaseMapper baseMapper;

    Log log = LogFactory.getLog(MongoPlusAutoConfiguration.class);

    public MongoPlusAutoConfiguration(MongoDBLogProperty mongodbLogProperty, MongoDBCollectionProperty mongodbCollectionProperty, MongoLogicDelProperty mongoLogicDelProperty, BaseMapper baseMapper, MongoPlusClient mongoPlusClient, ApplicationContext applicationContext, CollectionNameConvert collectionNameConvert) {
        this.mongoPlusClient = mongoPlusClient;
        this.applicationContext = applicationContext;
        this.mongodbLogProperty = mongodbLogProperty;
        this.mongodbCollectionProperty = mongodbCollectionProperty;
        this.mongoLogicDelProperty = mongoLogicDelProperty;
        this.collectionNameConvert = collectionNameConvert;
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
        String database = initFactory(clazz);
        //这里需要将MongoPlusClient给工厂
        serviceImpl.setDatabase(database);
        serviceImpl.setBaseMapper(baseMapper);
    }

    public String initFactory(Class<?> clazz) {
        String collectionName = clazz.getSimpleName().toLowerCase();
        final String[] dataBaseName = {""};
        if (clazz.isAnnotationPresent(CollectionName.class)) {
            CollectionName annotation = clazz.getAnnotation(CollectionName.class);
            collectionName = annotation.value();
            dataBaseName[0] = annotation.database();
        }
        try {
            String finalCollectionName = collectionName;
            final String[] finalDataBaseName = {dataBaseName[0]};
            List<MongoDatabase> mongoDatabaseList = new ArrayList<>();
            String database = mongoPlusClient.getBaseProperty().getDatabase();
            Arrays.stream(database.split(",")).collect(Collectors.toList()).forEach(db -> {
                CollectionManager collectionManager = new CollectionManager(mongoPlusClient.getMongoClient(), collectionNameConvert, db);
                ConnectMongoDB connectMongodb = new ConnectMongoDB(mongoPlusClient.getMongoClient(), db, finalCollectionName);
                MongoDatabase mongoDatabase = mongoPlusClient.getMongoClient().getDatabase(db);
                mongoDatabaseList.add(mongoDatabase);
                if (Objects.equals(db, finalDataBaseName[0])) {
                    MongoCollection<Document> collection = connectMongodb.open(mongoDatabase);
                    collectionManager.setCollectionMap(finalCollectionName, collection);
                }
                mongoPlusClient.getCollectionManagerMap().put(DataSourceConstant.DEFAULT_DATASOURCE, new HashMap<String, CollectionManager>() {{
                    put(db, collectionManager);
                }});
            });
            mongoPlusClient.setMongoDatabase(mongoDatabaseList);
        } catch (MongoException e) {
            log.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
        return dataBaseName[0];
    }

    /**
     * 从Bean中拿到转换器
     *
     * @author JiaChaoYang
     * @date 2023/10/19 12:49
     */
    @SuppressWarnings("unchecked")
    private void setConversion() {
        applicationContext.getBeansOfType(ConversionStrategy.class).values().forEach(conversionStrategy -> {
            try {
                Type[] genericInterfaces = conversionStrategy.getClass().getGenericInterfaces();
                for (Type anInterface : genericInterfaces) {
                    ParameterizedType parameterizedType = (ParameterizedType) anInterface;
                    if (parameterizedType.getRawType().equals(ConversionStrategy.class)) {
                        Class<?> clazz = (Class<?>) parameterizedType.getActualTypeArguments()[0];
                        ConversionService.appendConversion(clazz, conversionStrategy);
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
            listeners.add(new LogListener());
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
