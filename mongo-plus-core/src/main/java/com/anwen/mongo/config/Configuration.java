package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.HandlerCache;
import com.anwen.mongo.cache.global.InterceptorCache;
import com.anwen.mongo.cache.global.ListenerCache;
import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.domain.InitMongoPlusException;
import com.anwen.mongo.enums.CollectionNameConvertEnum;
import com.anwen.mongo.handlers.DocumentHandler;
import com.anwen.mongo.handlers.MetaObjectHandler;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.listener.BaseListener;
import com.anwen.mongo.listener.Listener;
import com.anwen.mongo.listener.business.BlockAttackInnerListener;
import com.anwen.mongo.listener.business.LogListener;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.mapper.DefaultBaseMapperImpl;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.strategy.convert.ConversionService;
import com.anwen.mongo.strategy.convert.ConversionStrategy;
import com.anwen.mongo.toolkit.MongoCollectionUtils;
import com.anwen.mongo.toolkit.StringUtils;
import com.anwen.mongo.toolkit.UrlJoint;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoDatabase;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.stream.Collectors;

/**
 * MongoPlus配置
 *
 * @author JiaChaoYang
 **/
public class Configuration {

    /**
     * MongoDB连接URL
     * @author JiaChaoYang
     * @date 2024/3/19 18:25
    */
    private String url;

    /**
     * 属性配置文件，url和baseProperty存在一个即可
     * @author JiaChaoYang
     * @date 2024/3/19 18:25
    */
    private BaseProperty baseProperty = new BaseProperty();

    /**
     * 集合名称获取策略
     * @author JiaChaoYang
     * @date 2024/3/19 18:25
    */
    private CollectionNameConvert collectionNameConvert = MongoCollectionUtils.build(CollectionNameConvertEnum.ALL_CHAR_LOWERCASE);

    /**
     * 获取一个空的Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:26
    */
    public static Configuration builder(){
        return new Configuration();
    }

    /**
     * 设置url
     * @author JiaChaoYang
     * @date 2024/3/19 18:26
    */
    public Configuration connection(String url){
        this.url = url;
        return this;
    }

    /**
     * 设置属性配置文件
     * @author JiaChaoYang
     * @date 2024/3/19 18:26
    */
    public Configuration connection(BaseProperty baseProperty){
        this.baseProperty = baseProperty;
        UrlJoint urlJoint = new UrlJoint(baseProperty);
        return connection(urlJoint.jointMongoUrl());
    }

    /**
     * 配置数据库
     * @param database 数据库 多个库使用逗号隔开
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 19:08
    */
    public Configuration database(String database){
        this.baseProperty.setDatabase(database);
        return this;
    }

    /**
     * 设置集合名称获取策略
     * @author JiaChaoYang
     * @date 2024/3/19 18:27
    */
    public Configuration collectionNameConvert(CollectionNameConvertEnum collectionNameConvertEnum){
        this.collectionNameConvert = MongoCollectionUtils.build(collectionNameConvertEnum);
        return this;
    }

    /**
     * 设置转换器
     * @param clazzConversions 转换器类
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:29
    */
    @SafeVarargs
    public final Configuration convert(Class<? extends ConversionStrategy<?>>... clazzConversions){
        for (Class<? extends ConversionStrategy<?>> clazzConversion : clazzConversions) {
            Type[] genericInterfaces = clazzConversion.getGenericInterfaces();
            for (Type anInterface : genericInterfaces) {
                ParameterizedType parameterizedType = (ParameterizedType) anInterface;
                if (parameterizedType.getRawType().equals(ConversionStrategy.class)){
                    Class<?> clazz = (Class<?>) parameterizedType.getActualTypeArguments()[0];
                    try {
                        ConversionService.appendConversion(clazz,clazzConversion.getDeclaredConstructor().newInstance());
                    } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                             NoSuchMethodException e) {
                        throw new RuntimeException(e);
                    }
                    break;
                }
            }
        }
        return this;
    }

    /**
     * 设置自动填充
     * @param metaObjectHandler 元数据填充
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:29
    */
    public Configuration metaObjectHandler(MetaObjectHandler metaObjectHandler){
        HandlerCache.metaObjectHandler = metaObjectHandler;
        return this;
    }

    /**
     * 设置Document处理器
     * @param documentHandler document处理器
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:30
    */
    public Configuration documentHandler(DocumentHandler documentHandler){
        HandlerCache.documentHandler = documentHandler;
        return this;
    }

    /**
     * 开启日志打印
     * @author JiaChaoYang
     * @date 2024/3/19 18:31
    */
    public Configuration log(){
        ListenerCache.listeners.add(new LogListener());
        return this;
    }

    /**
     * 开启防攻击
     * @author JiaChaoYang
     * @date 2024/3/19 18:31
    */
    public Configuration blockAttackInner(){
        ListenerCache.listeners.add(new BlockAttackInnerListener());
        return this;
    }

    /**
     * 设置监听器
     * @param listeners 监听器
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:38
    */
    @SafeVarargs
    public final Configuration listener(Class<? extends Listener>... listeners){
        for (Class<? extends Listener> listener : listeners) {
            try {
                ListenerCache.listeners.add(listener.getDeclaredConstructor().newInstance());
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                     NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        }
        return this;
    }

    /**
     * 设置拦截器
     * @param interceptors 拦截器
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:38
    */
    @SafeVarargs
    public final Configuration interceptor(Class<? extends Interceptor>... interceptors){
        for (Class<? extends Interceptor> interceptor : interceptors) {
            try {
                InterceptorCache.interceptors.add(interceptor.getDeclaredConstructor().newInstance());
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                     NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        }
        return this;
    }

    /**
     * 获取MongoPlusClient
     * @author JiaChaoYang
     * @date 2024/3/19 18:38
    */
    public MongoPlusClient getMongoPlusClient(){
        if (StringUtils.isBlank(url)){
            throw new InitMongoPlusException("Connection URL not configured");
        }
        if (StringUtils.isBlank(baseProperty.getDatabase())){
            throw new InitMongoPlusException("Connection database not configured");
        }
        return initMongoPlusClient();
    }

    public MongoPlusClient initMongoPlusClient(){
        return initMongoPlusClient(MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(this.url)).commandListenerList(Collections.singletonList(new BaseListener())).build()),collectionNameConvert,baseProperty);
    }

    public MongoPlusClient initMongoPlusClient(CollectionNameConvert collectionNameConvert){
        return initMongoPlusClient(MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(this.url)).commandListenerList(Collections.singletonList(new BaseListener())).build()),collectionNameConvert,baseProperty);
    }

    public MongoPlusClient initMongoPlusClient(CollectionNameConvert collectionNameConvert,BaseProperty baseProperty){
        return initMongoPlusClient(MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(this.url)).commandListenerList(Collections.singletonList(new BaseListener())).build()),collectionNameConvert,baseProperty);
    }

    public MongoPlusClient initMongoPlusClient(MongoClient mongoClient,CollectionNameConvert collectionNameConvert,BaseProperty baseProperty){
        if (StringUtils.isBlank(baseProperty.getDatabase())){
            throw new InitMongoPlusException("Connection database not configured");
        }
        MongoPlusClient mongoPlusClient = new MongoPlusClient();
        mongoPlusClient.setMongoClient(mongoClient);
        mongoPlusClient.setBaseProperty(baseProperty);
        mongoPlusClient.setCollectionNameConvert(collectionNameConvert);
        List<MongoDatabase> mongoDatabaseList = new ArrayList<>();
        mongoPlusClient.setCollectionManager(new LinkedHashMap<String, CollectionManager>(){{
            String database = mongoPlusClient.getBaseProperty().getDatabase();
            Arrays.stream(database.split(",")).collect(Collectors.toList()).forEach(db -> {
                CollectionManager collectionManager = new CollectionManager(mongoPlusClient.getMongoClient(), collectionNameConvert, db);
                MongoDatabase mongoDatabase = mongoPlusClient.getMongoClient().getDatabase(db);
                mongoDatabaseList.add(mongoDatabase);
                put(db,collectionManager);
            });
        }});
        mongoPlusClient.setMongoDatabase(mongoDatabaseList);
        MongoPlusClientCache.mongoPlusClient = mongoPlusClient;
        return mongoPlusClient;
    }

    /**
     * 获取BaseMapper
     * @author JiaChaoYang
     * @date 2024/3/19 18:39
    */
    public BaseMapper getBaseMapper(){
        return new DefaultBaseMapperImpl(getMongoPlusClient());
    }

    public MongoPlusMapMapper getMongoPlusMapMapper(){
        return new MongoPlusMapMapper(getMongoPlusClient());
    }

}
