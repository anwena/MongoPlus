package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.interceptor.BaseInterceptor;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBConnectProperty;
import com.anwen.mongo.toolkit.MongoCollectionUtils;
import com.anwen.mongo.toolkit.UrlJoint;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoCollection;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

import java.util.Collections;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@EnableConfigurationProperties(value = {MongoDBConnectProperty.class, MongoDBCollectionProperty.class})
public class MongoPlusConfiguration {

    private final MongoDBConnectProperty mongoDBConnectProperty;

    private final MongoDBCollectionProperty mongoDBCollectionProperty;

    private SqlExecute sqlExecute;

    public SqlExecute getSqlExecute() {
        return sqlExecute;
    }

    public MongoPlusConfiguration(MongoDBConnectProperty mongoDBConnectProperty, MongoDBCollectionProperty mongoDBCollectionProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoDBCollectionProperty = mongoDBCollectionProperty;
    }

    /**
     * 这里将MongoClient注册为Bean，但是只是给MongoTemplate使用，master的client
     * @author JiaChaoYang
     * @date 2024/1/4 23:49
    */
    @Bean
    @ConditionalOnMissingBean
    public MongoClient mongoClient(){
        return MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(new UrlJoint(mongoDBConnectProperty).jointMongoUrl())).commandListenerList(Collections.singletonList(new BaseInterceptor())).build());
    }

    @Bean
    @ConditionalOnMissingBean(MongoPlusClient.class)
    public MongoPlusClient mongoPlusClient(MongoClient mongoClient){
        MongoPlusClient mongoPlusClient = new MongoPlusClient();
        mongoPlusClient.setMongoClient(mongoClient);
        mongoPlusClient.setBaseProperty(mongoDBConnectProperty);
        MongoPlusClientCache.mongoPlusClient = mongoPlusClient;
        return mongoPlusClient;
    }

    @Bean("sqlExecute")
    @ConditionalOnMissingBean
    public SqlExecute sqlExecute(MongoClient mongo) {
        if (this.sqlExecute != null) {
            return this.sqlExecute;
        }
        SqlExecute sqlExecute = new SqlExecute();
        sqlExecute.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlExecute.setBaseProperty(mongoDBConnectProperty);
        CollectionNameConvert collectionNameConvert =
                MongoCollectionUtils.build(mongoDBCollectionProperty.getMappingStrategy());
        sqlExecute.setCollectionNameConvert(collectionNameConvert);
        sqlExecute.setMongoClient(mongo);
        this.sqlExecute = sqlExecute;
        return sqlExecute;
    }

    @Bean
    @ConditionalOnMissingBean(CollectionNameConvert.class)
    public CollectionNameConvert collectionNameConvert(){
        return MongoCollectionUtils.build(mongoDBCollectionProperty.getMappingStrategy());
    }

    @Bean
    @ConditionalOnMissingBean
    public ExecutorFactory executeFactory(CollectionNameConvert collectionNameConvert){
        return ExecutorFactory.builder()
                .baseProperty(mongoDBConnectProperty)
                .collectionNameConvert(collectionNameConvert)
                .build();
    }

    @Bean("mongoPlusMapMapper")
    @ConditionalOnMissingBean
    public MongoPlusMapMapper mongoPlusMapMapper(SqlExecute sqlExecute,ExecutorFactory factory) {
        return new MongoPlusMapMapper(sqlExecute,factory);
    }

    @Bean("mongoTransactionalAspect")
    @Deprecated
    @ConditionalOnMissingBean
    public MongoTransactionalAspect mongoTransactionalAspect(MongoClient mongoClient) {
        return new MongoTransactionalAspect(mongoClient);
    }

}
