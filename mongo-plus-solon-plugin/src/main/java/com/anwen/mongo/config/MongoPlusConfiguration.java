package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.interceptor.BaseInterceptor;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBConnectProperty;
import com.anwen.mongo.property.MongoDBLogProperty;
import com.anwen.mongo.toolkit.MongoCollectionUtils;
import com.anwen.mongo.toolkit.UrlJoint;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import org.noear.solon.annotation.Bean;
import org.noear.solon.annotation.Condition;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

import java.util.Collections;
import java.util.Optional;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Configuration
public class MongoPlusConfiguration {

    private final MongoDBConnectProperty mongoDBConnectProperty;

    private final MongoDBCollectionProperty mongoDBCollectionProperty;

    public MongoPlusConfiguration(@Inject("${mongo-plus.data.mongodb}") MongoDBConnectProperty mongoDBConnectProperty,@Inject(value = "${mongo-plus.configuration.collection}",required = false) MongoDBCollectionProperty mongoDBCollectionProperty) {
        mongoDBCollectionProperty = Optional.ofNullable(mongoDBCollectionProperty).orElseGet(MongoDBCollectionProperty::new);
        this.mongoDBCollectionProperty = mongoDBCollectionProperty;
        this.mongoDBConnectProperty = mongoDBConnectProperty;
    }

    /**
     * 这里将MongoClient注册为Bean，但是只是给MongoTemplate使用，master的client
     * @author JiaChaoYang
     * @date 2024/1/4 23:49
     */
    @Bean
    @Condition(onMissingBean = MongoClient.class)
    public MongoClient mongoClient(){
        return MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(new UrlJoint(mongoDBConnectProperty).jointMongoUrl())).commandListenerList(Collections.singletonList(new BaseInterceptor())).build());
    }

    @Bean
    @Condition(onMissingBean = MongoPlusClient.class)
    public MongoPlusClient mongoPlusClient(MongoClient mongoClient){
        MongoPlusClient mongoPlusClient = new MongoPlusClient();
        mongoPlusClient.setMongoClient(mongoClient);
        mongoPlusClient.setBaseProperty(mongoDBConnectProperty);
        MongoPlusClientCache.mongoPlusClient = mongoPlusClient;
        return mongoPlusClient;
    }

    @Bean
    @Condition(onMissingBean = CollectionNameConvert.class)
    public CollectionNameConvert collectionNameConvert(){
        return MongoCollectionUtils.build(mongoDBCollectionProperty.getMappingStrategy());
    }

    @Bean
    @Condition(onMissingBean = ExecutorFactory.class)
    public ExecutorFactory executeFactory(CollectionNameConvert collectionNameConvert){
        return ExecutorFactory.builder()
                .baseProperty(mongoDBConnectProperty)
                .collectionNameConvert(collectionNameConvert)
                .build();
    }

    @Bean("mongoPlusMapMapper")
    @Condition(onMissingBean = MongoPlusMapMapper.class)
    public MongoPlusMapMapper mongoPlusMapMapper(ExecutorFactory factory) {
        return new MongoPlusMapMapper(factory);
    }

    @Bean("mongoTransactionalAspect")
    @Deprecated
    @Condition(onMissingBean = MongoTransactionalAspect.class)
    public MongoTransactionalAspect mongoTransactionalAspect(MongoClient mongoClient) {
        return new MongoTransactionalAspect(mongoClient);
    }

    @Bean
    public MongoPlusAutoConfiguration mongoPlusAutoConfiguration(@Inject ExecutorFactory factory,
                                                                 @Inject MongoPlusClient mongoPlusClient,
                                                                 @Inject CollectionNameConvert collectionNameConvert,
                                                                 @Inject("${mongo-plus}") MongoDBLogProperty mongoDBLogProperty){
        return new MongoPlusAutoConfiguration(factory,mongoPlusClient,collectionNameConvert,mongoDBLogProperty,mongoDBCollectionProperty);
    }

}
