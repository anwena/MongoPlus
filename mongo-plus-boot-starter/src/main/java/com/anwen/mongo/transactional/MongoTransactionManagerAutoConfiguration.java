package com.anwen.mongo.transactional;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionManager;

/**
 * 事务配置类
 *
 * @author JiaChaoYang
 **/
public class MongoTransactionManagerAutoConfiguration {

    @Bean("mongoPlusTransactionalManager")
    @ConditionalOnMissingBean(TransactionManager.class)
    @ConditionalOnProperty(name = "mongo-plus.spring.transaction",havingValue = "true")
    public PlatformTransactionManager mongoPlusTransactionalManager(){
        return new MongoPlusTransactionalManager();
    }

}
