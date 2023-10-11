package com.anwen.mongo.transactional;

import com.mongodb.client.MongoClient;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
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
public class MongoTransactionManagerAutoConfiguration {

    private final MongoClient mongoClient;

    public MongoTransactionManagerAutoConfiguration(MongoClient mongo) {
        this.mongoClient = mongo;
    }

    @Bean("mongoPlusTransactionalManager")
    @ConditionalOnMissingBean(TransactionManager.class)
    @ConditionalOnProperty(name = "mongo-plus.spring.transaction",havingValue = "true")
    public PlatformTransactionManager mongoPlusTransactionalManager(){
        return new MongoPlusTransactionalManager(mongoClient);
    }

}
