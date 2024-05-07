package com.anwen.mongo.config;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.cache.global.ExecutorReplacerCache;
import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.cache.global.ListenerCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.conn.ConnectMongoDB;
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
import org.noear.solon.Solon;
import org.noear.solon.annotation.Inject;
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

    private final MongoPlusClient mongoPlusClient;

    private final CollectionNameConvert collectionNameConvert;

    private final MongoDBLogProperty mongoDBLogProperty;

    private final MongoDBCollectionProperty mongoDBCollectionProperty;

    Log log = LogFactory.getLog(MongoPlusAutoConfiguration.class);

    public MongoPlusAutoConfiguration(BaseMapper baseMapper, MongoPlusClient mongoPlusClient, @Inject CollectionNameConvert collectionNameConvert, MongoDBLogProperty mongoDBLogProperty, MongoDBCollectionProperty mongoDBCollectionProperty){
        mongoDBCollectionProperty = Optional.ofNullable(mongoDBCollectionProperty).orElseGet(MongoDBCollectionProperty::new);
        this.collectionNameConvert = collectionNameConvert;
        this.mongoPlusClient = mongoPlusClient;
        this.mongoDBLogProperty = mongoDBLogProperty;
        this.mongoDBCollectionProperty = mongoDBCollectionProperty;
        this.baseMapper = baseMapper;
        AppContext context = Solon.context();
        context.subBeansOfType(IService.class, bean -> {
            if (bean instanceof ServiceImpl){
                setExecute((ServiceImpl<?>) bean,bean.getGenericityClass());
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
    }

    /**
     * 从Bean中拿到Document的处理器
     * @author JiaChaoYang
     * @date 2023/11/23 12:56
    */
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
                if (Objects.equals(db, finalDataBaseName[0])){
                    MongoCollection<Document> collection = connectMongodb.open(mongoDatabase);
                    collectionManager.setCollectionMap(finalCollectionName,collection);
                }
                mongoPlusClient.getCollectionManagerMap().put(db,collectionManager);
            });
            mongoPlusClient.setMongoDatabase(mongoDatabaseList);
        } catch (MongoException e) {
            log.error("Failed to connect to MongoDB: {}", e.getMessage(), e);
        }
        return dataBaseName[0];
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
                        ConversionService.appendConversion(clazz,conversionStrategy);
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
            listeners.add(new LogListener());
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

}
