package com.anwen.mongo.config;

import com.anwen.mongo.event.SqlOperationInitializedEvent;
import com.anwen.mongo.log.CustomMongoDriverLogger;
import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.utils.UrlJoint;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfigurationExcludeFilter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration;
import org.springframework.boot.autoconfigure.mongo.MongoClientSettingsBuilderCustomizer;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.*;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Configuration
@Log4j2
@EnableConfigurationProperties(MongoDBConnectProperty.class)
public class MongoDBJDBCConfiguration extends MongoAutoConfiguration{

    @Override
    public MongoClient mongo(ObjectProvider<MongoClientSettingsBuilderCustomizer> builderCustomizers, MongoClientSettings settings) {
        return null;
    }

    /**
     * 自定义事件
     * @author JiaChaoYang
     * @date 2023/6/26/026 22:06
    */ 
    @Resource
    private ApplicationEventPublisher eventPublisher;

    final
    MongoDBConnectProperty mongoDBConnectProperty;

    public MongoDBJDBCConfiguration(MongoDBConnectProperty mongoDBConnectProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
    }

    @Bean
    @ConditionalOnMissingBean
    @PostConstruct
    public SqlOperation<?> sqlOperation() {
        CustomMongoDriverLogger customLogger = new CustomMongoDriverLogger();
        SqlOperation<?> sqlOperation = new SqlOperation<>();
        sqlOperation.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlOperation.setBaseProperty(mongoDBConnectProperty);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        MongoClientSettings settings = MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(urlJoint.jointMongoUrl()))
                .addCommandListener(customLogger)
                .build();
        sqlOperation.setMongoClient(MongoClients.create(settings));
        // 发布自定义事件通知其他类，sqlOperation已完成初始化
        eventPublisher.publishEvent(new SqlOperationInitializedEvent(sqlOperation));
        return sqlOperation;
    }
}
