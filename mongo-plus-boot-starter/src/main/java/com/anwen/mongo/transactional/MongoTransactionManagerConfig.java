package com.anwen.mongo.transactional;

import com.mongodb.client.MongoClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.DependsOn;
import org.springframework.transaction.PlatformTransactionManager;
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

    public MongoTransactionManagerConfig(MongoClient mongo) {
        this.mongoClient = mongo;
    }

    @Bean
    public PlatformTransactionManager platformTransactionManager(){
        return new MongoTransactionalManager(mongoClient);
    }

}
