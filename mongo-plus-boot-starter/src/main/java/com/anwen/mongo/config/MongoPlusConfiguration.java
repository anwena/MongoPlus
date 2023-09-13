package com.anwen.mongo.config;

import com.anwen.mongo.cache.MongoClientCache;
import com.anwen.mongo.config.log.MongoDBLogProperty;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.log.CustomMongoDriverLogger;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.toolkit.UrlJoint;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@EnableConfigurationProperties(value = {MongoDBConnectProperty.class, MongoDBLogProperty.class})
public class MongoPlusConfiguration {

    private final MongoDBLogProperty mongoDBLogProperty;

    final
    MongoDBConnectProperty mongoDBConnectProperty;

    private MongoClient mongoClient;

    private SqlExecute sqlExecute;

    public MongoPlusConfiguration(MongoDBConnectProperty mongoDBConnectProperty,MongoDBLogProperty mongoDBLogProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoDBLogProperty = mongoDBLogProperty;
    }

    @Bean("sqlExecute")
    @ConditionalOnMissingBean
    public SqlExecute sqlExecute() {
        if (this.sqlExecute != null){
            return this.sqlExecute;
        }
        SqlExecute sqlExecute = new SqlExecute();
        sqlExecute.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlExecute.setBaseProperty(mongoDBConnectProperty);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        MongoClientSettings.Builder builder = MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(urlJoint.jointMongoUrl()));
        if (mongoDBLogProperty.getLog()){
            builder.addCommandListener(new CustomMongoDriverLogger(mongoDBLogProperty.getFormat()));
        }
        this.mongoClient = MongoClients.create(builder.build());
        sqlExecute.setMongoClient(this.mongoClient);
        this.sqlExecute = sqlExecute;
        return sqlExecute;
    }

    @Bean("mongo")
    @ConditionalOnMissingBean
    @DependsOn("sqlExecute")
    public MongoClient mongo(){
        MongoClientCache.mongoClient = this.mongoClient;
        return this.mongoClient;
    }

    @Bean("mongoPlusMapMapper")
    public MongoPlusMapMapper mongoPlusMapMapper(SqlExecute sqlExecute){
        return new MongoPlusMapMapper(sqlExecute);
    }

    @Bean("mongoTransactionalAspect")
    @Deprecated
    public MongoTransactionalAspect mongoTransactionalAspect(){
        return new MongoTransactionalAspect(this.mongoClient);
    }
}
