package com.anwen.mongo.config;

import com.anwen.mongo.annotation.collection.CollectionLogic;
import com.anwen.mongo.aware.Aware;
import com.anwen.mongo.cache.global.*;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.constant.DataSourceConstant;
import com.anwen.mongo.domain.InitMongoLogicException;
import com.anwen.mongo.domain.InitMongoPlusException;
import com.anwen.mongo.enums.CollectionNameConvertEnum;
import com.anwen.mongo.factory.MongoClientFactory;
import com.anwen.mongo.handlers.DocumentHandler;
import com.anwen.mongo.handlers.MetaObjectHandler;
import com.anwen.mongo.handlers.TenantHandler;
import com.anwen.mongo.handlers.collection.AnnotationOperate;
import com.anwen.mongo.interceptor.Interceptor;
import com.anwen.mongo.interceptor.business.CollectionLogiceInterceptor;
import com.anwen.mongo.interceptor.business.LogicAutoFillInterceptor;
import com.anwen.mongo.interceptor.business.TenantInterceptor;
import com.anwen.mongo.listener.BaseListener;
import com.anwen.mongo.listener.Listener;
import com.anwen.mongo.listener.business.BlockAttackInnerListener;
import com.anwen.mongo.listener.business.LogListener;
import com.anwen.mongo.logic.replacer.LogicRemoveReplacer;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.mapper.DefaultBaseMapperImpl;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.mapping.*;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.model.LogicDeleteResult;
import com.anwen.mongo.model.LogicProperty;
import com.anwen.mongo.replacer.Replacer;
import com.anwen.mongo.strategy.conversion.ConversionStrategy;
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
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * MongoPlus配置
 *
 * @author JiaChaoYang
 **/
public class Configuration {

    /**
     * MongoDB连接URL
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:25
     */
    private String url;

    /**
     * 属性配置文件，url和baseProperty存在一个即可
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:25
     */
    private BaseProperty baseProperty = new BaseProperty();

    /**
     * 逻辑删除配置
     *
     * @author loser
     */
    private LogicProperty logicProperty = new LogicProperty();


    /**
     * 获取一个空的Configuration
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:26
     */
    public static Configuration builder() {
        return new Configuration();
    }

    /**
     * 设置url
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:26
     */
    public Configuration connection(String url) {
        this.url = url;
        return this;
    }

    /**
     * 设置属性配置文件
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:26
     */
    public Configuration connection(BaseProperty baseProperty) {
        this.baseProperty = baseProperty;
        UrlJoint urlJoint = new UrlJoint(baseProperty);
        return connection(urlJoint.jointMongoUrl());
    }

    /**
     * 配置数据库
     *
     * @param database 数据库 多个库使用逗号隔开
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 19:08
     */
    public Configuration database(String database) {
        this.baseProperty.setDatabase(database);
        return this;
    }

    /**
     * 设置集合名称获取策略
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:27
     */
    public Configuration collectionNameConvert(CollectionNameConvertEnum collectionNameConvertEnum) {
        AnnotationOperate.setCollectionNameConvertEnum(collectionNameConvertEnum);
        return this;
    }

    /**
     * 设置转换器
     *
     * @param clazzConversions 转换器类
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:29
     */
    @SafeVarargs
    public final Configuration convert(Class<? extends ConversionStrategy<?>>... clazzConversions) {
        for (Class<? extends ConversionStrategy<?>> clazzConversion : clazzConversions) {
            Type[] genericInterfaces = clazzConversion.getGenericInterfaces();
            for (Type anInterface : genericInterfaces) {
                ParameterizedType parameterizedType = (ParameterizedType) anInterface;
                if (parameterizedType.getRawType().equals(ConversionStrategy.class)) {
                    Class<?> clazz = (Class<?>) parameterizedType.getActualTypeArguments()[0];
                    try {
                        ConversionCache.putConversionStrategy(clazz, clazzConversion.getDeclaredConstructor().newInstance());
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
     *
     * @param metaObjectHandler 元数据填充
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:29
     */
    public Configuration metaObjectHandler(MetaObjectHandler metaObjectHandler) {
        HandlerCache.metaObjectHandler = metaObjectHandler;
        return this;
    }

    /**
     * 设置Document处理器
     *
     * @param documentHandler document处理器
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:30
     */
    public Configuration documentHandler(DocumentHandler documentHandler) {
        HandlerCache.documentHandler = documentHandler;
        return this;
    }

    /**
     * 开启日志打印
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:31
     */
    public Configuration log() {
        ListenerCache.listeners.add(new LogListener());
        return this;
    }

    /**
     * 开启日志打印
     *
     * @param pretty 是否将 mongo 语句格式化可执行语句
     * @author loser
     */
    public Configuration log(Boolean pretty) {
        ListenerCache.listeners.add(new LogListener(pretty));
        return this;
    }

    /**
     * 开启防攻击
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:31
     */
    public Configuration blockAttackInner() {
        ListenerCache.listeners.add(new BlockAttackInnerListener());
        return this;
    }

    /**
     * 设置监听器
     *
     * @param listeners 监听器
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:38
     */
    @SafeVarargs
    public final Configuration listener(Class<? extends Listener>... listeners) {
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
     *
     * @param interceptors 拦截器
     * @return com.anwen.mongo.config.Configuration
     * @author JiaChaoYang
     * @date 2024/3/19 18:38
     */
    @SafeVarargs
    public final Configuration interceptor(Class<? extends Interceptor>... interceptors) {
        for (Class<? extends Interceptor> interceptor : interceptors) {
            try {
                InterceptorCache.interceptors.add(interceptor.getDeclaredConstructor().newInstance());
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                     NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        }
        InterceptorCache.sorted();
        return this;
    }

    /**
     * 设置多租户处理器
     *
     * @author anwen
     * @date 2024/6/27 下午12:47
     */
    public Configuration tenantHandler(TenantHandler tenantHandler) {
        InterceptorCache.interceptors.add(new TenantInterceptor(tenantHandler));
        return this;
    }

    /**
     * 设置 替换器
     *
     * @param replacers 替换器
     * @return 配置对象
     * @author loser
     */
    @SafeVarargs
    public final Configuration replacer(Class<? extends Replacer>... replacers) {
        for (Class<? extends Replacer> replacer : replacers) {
            try {
                ExecutorReplacerCache.replacers.add(replacer.getDeclaredConstructor().newInstance());
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                     NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        }
        ExecutorReplacerCache.sorted();
        return this;
    }

    /**
     * 获取MongoPlusClient
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:38
     */
    public MongoPlusClient getMongoPlusClient() {
        if (StringUtils.isBlank(url)) {
            throw new InitMongoPlusException("Connection URL not configured");
        }
        if (StringUtils.isBlank(baseProperty.getDatabase())) {
            throw new InitMongoPlusException("Connection database not configured");
        }
        return initMongoPlusClient();
    }

    public MongoPlusClient initMongoPlusClient() {
        return initMongoPlusClient(MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(this.url)).commandListenerList(Collections.singletonList(new BaseListener())).build()), baseProperty);
    }

    public MongoPlusClient initMongoPlusClient(BaseProperty baseProperty) {
        return initMongoPlusClient(MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(this.url)).commandListenerList(Collections.singletonList(new BaseListener())).build()), baseProperty);
    }

    public MongoPlusClient initMongoPlusClient(MongoClient mongoClient, BaseProperty baseProperty) {
        if (StringUtils.isBlank(baseProperty.getDatabase())) {
            throw new InitMongoPlusException("Connection database not configured");
        }
        MongoClientFactory.getInstance(mongoClient);
        MongoPlusClient mongoPlusClient = new MongoPlusClient();
        mongoPlusClient.setBaseProperty(baseProperty);
        List<MongoDatabase> mongoDatabaseList = new ArrayList<>();
        mongoPlusClient.setCollectionManagerMap(new ConcurrentHashMap<String, Map<String, CollectionManager>>() {{
            put(DataSourceConstant.DEFAULT_DATASOURCE, new LinkedHashMap<String, CollectionManager>() {{
                String database = mongoPlusClient.getBaseProperty().getDatabase();
                Arrays.stream(database.split(",")).collect(Collectors.toList()).forEach(db -> {
                    CollectionManager collectionManager = new CollectionManager(db);
                    MongoDatabase mongoDatabase = mongoPlusClient.getMongoClient().getDatabase(db);
                    mongoDatabaseList.add(mongoDatabase);
                    put(db, collectionManager);
                });
            }});
        }});
        mongoPlusClient.setMongoDatabase(mongoDatabaseList);
        MongoPlusClientCache.mongoPlusClient = mongoPlusClient;
        return mongoPlusClient;
    }

    /**
     * 设置数据源
     *
     * @author JiaChaoYang
     * @date 2024/4/5 1:48
     */
    public void setOtherDataSource(Map<String, MongoClient> mongoClientMap) {
        MongoClientFactory.getInstance(mongoClientMap);
    }

    /**
     * 获取BaseMapper
     *
     * @author JiaChaoYang
     * @date 2024/3/19 18:39
     */
    public BaseMapper getBaseMapper() {
        return new DefaultBaseMapperImpl(getMongoPlusClient(), new MappingMongoConverter(getMongoPlusClient(), new SimpleTypeHolder()));
    }

    public BaseMapper getBaseMapper(MongoConverter mongoConverter) {
        return new DefaultBaseMapperImpl(getMongoPlusClient(), mongoConverter);
    }

    public MongoPlusMapMapper getMongoPlusMapMapper() {
        return new MongoPlusMapMapper(getMongoPlusClient(), new MappingMongoConverter(getMongoPlusClient(), new SimpleTypeHolder()));
    }

    public MongoPlusMapMapper getMongoPlusMapMapper(MongoConverter mongoConverter) {
        return new MongoPlusMapMapper(getMongoPlusClient(), mongoConverter);
    }

    /**
     * 配置逻辑删除
     *
     * @param logicProperty 逻辑删除配置
     * @return 全局配置对象
     * @author loser
     */
    public Configuration logic(LogicProperty logicProperty) {

        if (Objects.isNull(logicProperty)) {
            throw new InitMongoLogicException("Config logic logicProperty not null");
        }
        this.logicProperty = logicProperty;
        CollectionLogicDeleteCache.open = logicProperty.getOpen();
        CollectionLogicDeleteCache.logicProperty = logicProperty;
        if (logicProperty.getOpen()) {
            InterceptorCache.interceptors.add(new CollectionLogiceInterceptor());
            if (logicProperty.getAutoFill()) {
                InterceptorCache.interceptors.add(new LogicAutoFillInterceptor());
            }
            InterceptorCache.sorted();
            ExecutorReplacerCache.replacers.add(new LogicRemoveReplacer());
            ExecutorReplacerCache.sorted();
        }
        return this;

    }

    /**
     * 注册逻辑删除 class
     *
     * @param collectionClasses 需要注册的 class 集合
     * @return 全局配置对象
     * @author loser
     */
    public Configuration setLogicFiled(Class<?>... collectionClasses) {
        return setLogicFiled(logicProperty, collectionClasses);
    }

    /**
     * 注册逻辑删除 class
     *
     * @param collectionClasses 需要注册的 class 集合
     * @return 全局配置对象
     * @author loser
     */
    public Configuration setLogicFiled(LogicProperty logicProperty, Class<?>... collectionClasses) {

        if (Objects.isNull(collectionClasses) || Objects.isNull(logicProperty) || !logicProperty.getOpen()) {
            return this;
        }
        Map<Class<?>, LogicDeleteResult> logicDeleteResultHashMap = CollectionLogicDeleteCache.logicDeleteResultHashMap;

        for (Class<?> clazz : collectionClasses) {
            if (logicDeleteResultHashMap.containsKey(clazz)) {
                continue;
            }
            TypeInformation typeInformation = TypeInformation.of(clazz);
            FieldInformation annotationField = typeInformation.getAnnotationField(CollectionLogic.class);
            // 优先使用每个对象自定义规则
            if (Objects.nonNull(annotationField)) {
                CollectionLogic annotation = (CollectionLogic) annotationField.getAnnotation(CollectionLogic.class);
                if (annotation.close()) {
                    continue;
                }
                LogicDeleteResult result = new LogicDeleteResult();
                String column = annotationField.getName();
                result.setColumn(column);
                result.setLogicDeleteValue(StringUtils.isNotBlank(annotation.delval()) ? annotation.delval() : logicProperty.getLogicDeleteValue());
                result.setLogicNotDeleteValue(StringUtils.isNotBlank(annotation.value()) ? annotation.value() : logicProperty.getLogicNotDeleteValue());
                logicDeleteResultHashMap.put(clazz, result);
                continue;
            }

            // 其次使用全局配置规则
            if (StringUtils.isNotEmpty(logicProperty.getLogicDeleteField())
                    && StringUtils.isNotEmpty(logicProperty.getLogicDeleteValue())
                    && StringUtils.isNotEmpty(logicProperty.getLogicNotDeleteValue())) {
                LogicDeleteResult result = new LogicDeleteResult();
                result.setColumn(logicProperty.getLogicDeleteField());
                result.setLogicDeleteValue(logicProperty.getLogicDeleteValue());
                result.setLogicNotDeleteValue(logicProperty.getLogicNotDeleteValue());
                logicDeleteResultHashMap.put(clazz, result);
                continue;
            }
            logicDeleteResultHashMap.put(clazz, null);

        }
        return this;

    }

    /**
     * 设置感知类
     */
    public Configuration aware(Aware aware) {
        AwareHandlerCache.putAware(aware);
        return this;
    }

}
