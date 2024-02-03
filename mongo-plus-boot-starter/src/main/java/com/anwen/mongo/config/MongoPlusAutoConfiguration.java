package com.anwen.mongo.config;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.conn.ConnectMongoDB;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.handlers.DocumentHandler;
import com.anwen.mongo.handlers.MetaObjectHandler;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.interceptor.business.BlockAttackInnerInterceptor;
import com.anwen.mongo.interceptor.business.LogInterceptor;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBLogProperty;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.CollUtil;
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
import java.util.*;
import java.util.stream.Collectors;

/**
 * MongoPlus自动注入配置
 * @author JiaChaoYang
 **/
@EnableConfigurationProperties(MongoDBLogProperty.class)
public class MongoPlusAutoConfiguration implements InitializingBean {

    private final ExecutorFactory factory;

    private final MongoPlusClient mongoPlusClient;

    private final ApplicationContext applicationContext;

    private final MongoDBLogProperty mongoDBLogProperty;

    private final MongoDBCollectionProperty mongoDBCollectionProperty;

    private final CollectionNameConvert collectionNameConvert;

    Logger logger = LoggerFactory.getLogger(MongoPlusAutoConfiguration.class);

    public MongoPlusAutoConfiguration(MongoDBLogProperty mongoDBLogProperty, MongoDBCollectionProperty mongoDBCollectionProperty, ExecutorFactory factory, MongoPlusClient mongoPlusClient, ApplicationContext applicationContext, CollectionNameConvert collectionNameConvert) {
        this.mongoPlusClient = mongoPlusClient;
        this.applicationContext = applicationContext;
        this.mongoDBLogProperty = mongoDBLogProperty;
        this.mongoDBCollectionProperty = mongoDBCollectionProperty;
        this.factory = factory;
        this.collectionNameConvert = collectionNameConvert;
        setConversion();
        setMetaObjectHandler();
        setDocumentHandler();
        setInterceptor();
    }

    @Override
    public void afterPropertiesSet() {
        applicationContext.getBeansOfType(IService.class)
                .values()
                .stream()
                .filter(s -> s instanceof ServiceImpl)
                .forEach(s -> setExecute((ServiceImpl<?>) s, s.getGenericityClass()));
    }

    private void setExecute(ServiceImpl<?> serviceImpl, Class<?> clazz) {
        serviceImpl.setClazz(clazz);
        String database = initFactory(clazz);
        //这里需要将MongoPlusClient给工厂
        factory.setMongoPlusClient(mongoPlusClient);
        serviceImpl.setDatabase(database);
        serviceImpl.setFactory(factory);
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
            mongoPlusClient.setCollectionManager(new LinkedHashMap<String, CollectionManager>(){{
                String database = mongoPlusClient.getBaseProperty().getDatabase();
                Arrays.stream(database.split(",")).collect(Collectors.toList()).forEach(db -> {
                    CollectionManager collectionManager = new CollectionManager(mongoPlusClient.getMongoClient(), collectionNameConvert, db);
                    ConnectMongoDB connectMongoDB = new ConnectMongoDB(mongoPlusClient.getMongoClient(), db, finalCollectionName);
                    MongoDatabase mongoDatabase = mongoPlusClient.getMongoClient().getDatabase(db);
                    mongoDatabaseList.add(mongoDatabase);
                    if (Objects.equals(db, finalDataBaseName[0])){
                        MongoCollection<Document> collection = connectMongoDB.open(mongoDatabase);
                        collectionManager.setCollectionMap(finalCollectionName,collection);
                    }
                    put(db,collectionManager);
                });
            }});
            mongoPlusClient.setMongoDatabase(mongoDatabaseList);
        } catch (MongoException e) {
            logger.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
        return dataBaseName[0];
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
        applicationContext.getBeansOfType(MetaObjectHandler.class).values().forEach(metaObjectHandler -> HandlerCache.metaObjectHandler = metaObjectHandler);
    }

    /**
     * 从Bean中拿到Document的处理器
     * @author JiaChaoYang
     * @date 2023/11/23 12:58
    */
    private void setDocumentHandler(){
        applicationContext.getBeansOfType(DocumentHandler.class).values().forEach(documentHandler -> HandlerCache.documentHandler = documentHandler);
    }

    /**
     * 从Bean中拿到拦截器
     * @author JiaChaoYang
     * @date 2023/11/22 18:39
    */
    private void setInterceptor(){
        List<Interceptor> interceptors = new ArrayList<>();
        if (mongoDBLogProperty.getLog()){
            interceptors.add(new LogInterceptor());
        }
        if (mongoDBCollectionProperty.getBlockAttackInner()){
            interceptors.add(new BlockAttackInnerInterceptor());
        }
        Collection<Interceptor> interceptorCollection = applicationContext.getBeansOfType(Interceptor.class).values();
        if (CollUtil.isNotEmpty(interceptorCollection)){
            interceptors.addAll(interceptorCollection);
        }
        InterceptorCache.interceptors = interceptors.stream().sorted(Comparator.comparingInt(Interceptor::getOrder)).collect(Collectors.toList());
    }

}
