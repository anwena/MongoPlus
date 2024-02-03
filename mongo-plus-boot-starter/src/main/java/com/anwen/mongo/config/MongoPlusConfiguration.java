package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.interceptor.BaseInterceptor;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBConfigurationProperty;
import com.anwen.mongo.property.MongoDBConnectProperty;
import com.anwen.mongo.toolkit.MongoCollectionUtils;
import com.anwen.mongo.toolkit.UrlJoint;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.mongo.MongoProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@EnableConfigurationProperties(value = {MongoDBConnectProperty.class, MongoDBCollectionProperty.class, MongoDBConfigurationProperty.class})
public class MongoPlusConfiguration {

    private final MongoDBConnectProperty mongoDBConnectProperty;

    private final MongoDBCollectionProperty mongoDBCollectionProperty;

    private final MongoDBConfigurationProperty mongoDBConfigurationProperty;

    public MongoPlusConfiguration(MongoDBConnectProperty mongoDBConnectProperty, MongoDBCollectionProperty mongoDBCollectionProperty, MongoDBConfigurationProperty mongoDBConfigurationProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoDBCollectionProperty = mongoDBCollectionProperty;
        this.mongoDBConfigurationProperty = mongoDBConfigurationProperty;
    }

    /**
     * 这里将MongoClient注册为Bean，但是只是给MongoTemplate使用，master的client
     * @author JiaChaoYang
     * @date 2024/1/4 23:49
    */
    @Bean
    @ConditionalOnMissingBean
    public MongoClient mongo(){
        return MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(new UrlJoint(mongoDBConnectProperty).jointMongoUrl())).commandListenerList(Collections.singletonList(new BaseInterceptor())).build());
    }

    @Bean
    @ConditionalOnMissingBean(MongoPlusClient.class)
    public MongoPlusClient mongoPlusClient(MongoClient mongo){
        MongoPlusClient mongoPlusClient = new MongoPlusClient();
        mongoPlusClient.setMongoClient(mongo);
        mongoPlusClient.setBaseProperty(mongoDBConnectProperty);
        MongoPlusClientCache.mongoPlusClient = mongoPlusClient;
        if (mongoDBConfigurationProperty.getBanner()){
            System.out.println("___  ___                       ______ _           \n" +
                    "|  \\/  |                       | ___ \\ |          \n" +
                    "| .  . | ___  _ __   __ _  ___ | |_/ / |_   _ ___ \n" +
                    "| |\\/| |/ _ \\| '_ \\ / _` |/ _ \\|  __/| | | | / __|\n" +
                    "| |  | | (_) | | | | (_| | (_) | |   | | |_| \\__ \\\n" +
                    "\\_|  |_/\\___/|_| |_|\\__, |\\___/\\_|   |_|\\__,_|___/\n" +
                    "                     __/ |                        \n" +
                    "                    |___/                         ");
        }
        return mongoPlusClient;
    }

    @Bean
    @ConditionalOnMissingBean(CollectionNameConvert.class)
    public CollectionNameConvert collectionNameConvert(){
        return MongoCollectionUtils.build(mongoDBCollectionProperty.getMappingStrategy());
    }

    @Bean
    @ConditionalOnMissingBean
    public ExecutorFactory factory(CollectionNameConvert collectionNameConvert,MongoPlusClient mongoPlusClient){
        return ExecutorFactory.builder()
                .baseProperty(mongoDBConnectProperty)
                .mongoPlusClient(mongoPlusClient)
                .collectionNameConvert(collectionNameConvert)
                .build();
    }

    @Bean("mongoPlusMapMapper")
    @ConditionalOnMissingBean
    public MongoPlusMapMapper mongoPlusMapMapper(ExecutorFactory factory) {
        return new MongoPlusMapMapper(factory);
    }

    @Bean("mongoTransactionalAspect")
    @ConditionalOnMissingBean
    public MongoTransactionalAspect mongoTransactionalAspect(MongoClient mongo) {
        return new MongoTransactionalAspect(mongo);
    }

}
