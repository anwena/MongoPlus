package com.anwen.mongo.config;

import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.utils.UrlJoint;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Configuration
@Slf4j
@EnableConfigurationProperties(MongoDBConnectProperty.class)
public class MongoDBJDBCConfiguration {

    final
    MongoDBConnectProperty mongoDBConnectProperty;

    public MongoDBJDBCConfiguration(MongoDBConnectProperty mongoDBConnectProperty) {
        this.mongoDBConnectProperty = mongoDBConnectProperty;
    }

    @Bean
    @ConditionalOnMissingBean
    public SqlOperation<?> sqlOperation() {
        SqlOperation<?> sqlOperation = new SqlOperation<>();
        sqlOperation.setSlaveDataSources(mongoDBConnectProperty.getSlaveDataSource());
        sqlOperation.setBaseProperty(mongoDBConnectProperty);
        UrlJoint urlJoint = new UrlJoint(mongoDBConnectProperty);
        try (MongoClient mongoClient = MongoClients.create(urlJoint.jointMongoUrl())) {
            sqlOperation.setMongoClient(mongoClient);
            log.info("Connected successfully to server.");
        }
        return sqlOperation;
    }

}
