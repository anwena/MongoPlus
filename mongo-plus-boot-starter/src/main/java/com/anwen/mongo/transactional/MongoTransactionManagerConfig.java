package com.anwen.mongo.transactional;

import com.anwen.mongo.config.MongoDBConnectProperty;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.client.MongoClient;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.DependsOn;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

/**
 * 事务配置类
 *
 * @author JiaChaoYang
 **/
@EnableTransactionManagement
@DependsOn("sqlExecute")
public class MongoTransactionManagerConfig {

    private final MongoClient mongoClient;

    private final MongoDBConnectProperty mongoDBConnectProperty;;

    public MongoTransactionManagerConfig(MongoClient mongo,MongoDBConnectProperty mongoDBConnectProperty) {
        this.mongoClient = mongo;
        this.mongoDBConnectProperty = mongoDBConnectProperty;
    }

    @Bean("mongoPlusTransactionalManager")
    @ConditionalOnMissingBean(TransactionManager.class)
    public MongoPlusTransactionalManager mongoPlusTransactionalManager(){
        //确保开启副本集
        return StringUtils.isNotBlank(mongoDBConnectProperty.getReplicaSet()) ? new MongoPlusTransactionalManager(mongoClient) : null;
    }

}
