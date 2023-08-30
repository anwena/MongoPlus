package com.anwen.mongo.config;

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
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoClientSettingsBuilderCustomizer;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

import javax.annotation.PostConstruct;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@EnableConfigurationProperties(value = {MongoDBConnectProperty.class, MongoDBLogProperty.class})
public class MongoPlusConfiguration extends MongoAutoConfiguration {

    private final MongoDBLogProperty mongoDBLogProperty;

    final
    MongoDBConnectProperty mongoDBConnectProperty;

    private MongoClient mongoClient;

    private SqlExecute sqlExecute;

    @Override
    public MongoClient mongo(ObjectProvider<MongoClientSettingsBuilderCustomizer> builderCustomizers, MongoClientSettings settings) {
        return this.mongoClient;
    }

    public MongoPlusConfiguration(MongoDBConnectProperty mongoDBConnectProperty,MongoDBLogProperty mongoDBLogProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoDBLogProperty = mongoDBLogProperty;
    }

    @Bean
    @ConditionalOnMissingBean
    @PostConstruct
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
        // 发布自定义事件通知其他类，sqlOperation已完成初始化
        this.sqlExecute = sqlExecute;
        return sqlExecute;
    }
    @Bean
    public MongoPlusMapMapper mongoPlusMapMapper(SqlExecute sqlExecute){
        return new MongoPlusMapMapper(sqlExecute);
    }

    @Bean
    public MongoTransactionalAspect mongoTransactionalAspect(){
        return new MongoTransactionalAspect(this.mongoClient);
    }
}
