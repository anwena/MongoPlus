package com.anwen.mongo.transactional;

import com.anwen.mongo.config.property.MongoDBConnectProperty;
import com.anwen.mongo.config.property.MongoTransactionProperty;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.MongoClient;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.DependsOn;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionManager;

/**
 * 事务配置类
 *
 * @author JiaChaoYang
 **/
@DependsOn("sqlExecute")
@EnableConfigurationProperties(value = MongoTransactionProperty.class)
public class MongoTransactionManagerConfig {

    private final MongoClient mongoClient;

    private final MongoDBConnectProperty mongoDBConnectProperty;

    private final MongoTransactionProperty mongoTransactionProperty;

    public MongoTransactionManagerConfig(MongoClient mongo,MongoDBConnectProperty mongoDBConnectProperty,MongoTransactionProperty mongoTransactionProperty) {
        this.mongoClient = mongo;
        this.mongoDBConnectProperty = mongoDBConnectProperty;
        this.mongoTransactionProperty = mongoTransactionProperty;
    }

    @Bean("mongoPlusTransactionalManager")
    @ConditionalOnMissingBean(TransactionManager.class)
    public PlatformTransactionManager mongoPlusTransactionalManager(){
        //确保开启副本集
        return StringUtils.isNotBlank(mongoDBConnectProperty.getReplicaSet()) && mongoTransactionProperty.getTransaction() ? new MongoPlusTransactionalManager(mongoClient) : null;
    }

}
