package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.MongoClientCache;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.interceptor.BaseInterceptor;
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
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.DependsOn;

import java.util.ArrayList;
import java.util.Arrays;
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

    private MongoClient mongoClient;

    private SqlExecute sqlExecute;

    public SqlExecute getSqlExecute() {
        return sqlExecute;
    }

    public MongoPlusConfiguration(MongoDBConnectProperty mongoDBConnectProperty, MongoDBCollectionProperty mongoDBCollectionProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoDBCollectionProperty = mongoDBCollectionProperty;
    }

    @Bean("sqlExecute")
    @ConditionalOnMissingBean
    public SqlExecute sqlExecute() {
        if (this.sqlExecute != null) {
            return this.sqlExecute;
        }
        SqlExecute sqlExecute = new SqlExecute();
        sqlExecute.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlExecute.setBaseProperty(mongoDBConnectProperty);
        CollectionNameConvert collectionNameConvert =
                MongoCollectionUtils.build(mongoDBCollectionProperty.getMappingStrategy());
        sqlExecute.setCollectionNameConvert(collectionNameConvert);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        this.mongoClient = MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(urlJoint.jointMongoUrl())).commandListenerList(Collections.singletonList(new BaseInterceptor())).build());
        sqlExecute.setMongoClient(this.mongoClient);
        this.sqlExecute = sqlExecute;
        return sqlExecute;
    }

    @Bean("mongo")
    @ConditionalOnMissingBean
    @DependsOn("sqlExecute")
    public MongoClient mongo() {
        MongoClientCache.mongoClient = this.mongoClient;
        return this.mongoClient;
    }

    @Bean("mongoPlusMapMapper")
    @ConditionalOnMissingBean
    public MongoPlusMapMapper mongoPlusMapMapper(SqlExecute sqlExecute) {
        return new MongoPlusMapMapper(sqlExecute);
    }

    @Bean("mongoTransactionalAspect")
    @Deprecated
    @ConditionalOnMissingBean
    public MongoTransactionalAspect mongoTransactionalAspect() {
        return new MongoTransactionalAspect(this.mongoClient);
    }

}
