package com.anwen.mongo.config;

import com.anwen.mongo.annotation.TableLogic;
import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.cache.global.ClassLogicDeleteCache;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.cache.global.ListenerCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.conn.ConnectMongoDB;
import com.anwen.mongo.constant.DataSourceConstant;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.handlers.DocumentHandler;
import com.anwen.mongo.handlers.MetaObjectHandler;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.listener.Listener;
import com.anwen.mongo.listener.business.BlockAttackInnerListener;
import com.anwen.mongo.listener.business.LogListener;
import com.anwen.mongo.logic.AnnotationHandler;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.model.ClassAnnotationFiled;
import com.anwen.mongo.model.LogicDeleteResult;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBLogProperty;
import com.anwen.mongo.property.MongoLogicDelProperty;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.MongoException;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import java.util.Map;
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

    Logger logger = LoggerFactory.getLogger(MongoPlusAutoConfiguration.class);

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
        setLogicDelete();
        this.baseMapper = baseMapper;
    }

    @Override
    public void afterPropertiesSet() {
        applicationContext.getBeansOfType(IService.class)
                .values()
                .stream()
                .filter(s -> s instanceof ServiceImpl)
                .forEach(s -> {
                    setExecute((ServiceImpl<?>) s, s.getGenericityClass());
                    setLogicFiled(s.getGenericityClass());
                });
    }

    private void setLogicDelete() {

        if (Objects.isNull(mongoLogicDelProperty)) {
            return;
        }
        ClassLogicDeleteCache.open = mongoLogicDelProperty.getOpen();

    }

    private void setLogicFiled(Class<?> clazz) {

        if (Objects.isNull(mongoLogicDelProperty) || !mongoLogicDelProperty.getOpen()) {
            return;
        }

        Map<Class<?>, LogicDeleteResult> logicDeleteResultHashMap = ClassLogicDeleteCache.logicDeleteResultHashMap;
        ClassAnnotationFiled<TableLogic> targetInfo = AnnotationHandler.getAnnotationOnFiled(clazz, TableLogic.class);
        // 优先使用每个对象自定义规则
        if (Objects.nonNull(targetInfo)) {
            TableLogic annotation = targetInfo.getTargetAnnotation();
            if (annotation.close()) {
                return;
            }
            LogicDeleteResult result = new LogicDeleteResult();
            result.setColumn(targetInfo.getField().getName());
            result.setLogicDeleteValue(StringUtils.isNotBlank(annotation.delval()) ? annotation.delval() : mongoLogicDelProperty.getLogicDeleteValue());
            result.setLogicNotDeleteValue(StringUtils.isNotBlank(annotation.value()) ? annotation.value() : mongoLogicDelProperty.getLogicNotDeleteValue());
            logicDeleteResultHashMap.put(clazz, result);
            return;
        }

        // 其次使用全局配置规则
        if (StringUtils.isNotEmpty(mongoLogicDelProperty.getLogicDeleteField())
                && StringUtils.isNotEmpty(mongoLogicDelProperty.getLogicDeleteValue())
                && StringUtils.isNotEmpty(mongoLogicDelProperty.getLogicNotDeleteValue())) {
            LogicDeleteResult result = new LogicDeleteResult();
            result.setColumn(mongoLogicDelProperty.getLogicDeleteField());
            result.setLogicDeleteValue(mongoLogicDelProperty.getLogicDeleteValue());
            result.setLogicNotDeleteValue(mongoLogicDelProperty.getLogicNotDeleteValue());
            logicDeleteResultHashMap.put(clazz, result);
        }

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
            logger.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
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
                logger.error("Unknown converter type", e);
                throw new MongoException("Unknown converter type");
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
        List<Listener> listeners = new ArrayList<>();
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
        ListenerCache.listeners = listeners.stream().sorted(Comparator.comparingInt(Listener::getOrder)).collect(Collectors.toList());
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

}
