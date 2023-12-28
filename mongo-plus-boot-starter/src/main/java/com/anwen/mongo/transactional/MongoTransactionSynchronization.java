package com.anwen.mongo.transactional;

import org.springframework.transaction.support.TransactionSynchronization;

/**
 * 事务同步
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2023-12-27 11:28
 **/
public class MongoTransactionSynchronization implements TransactionSynchronization {

    private final MongoPlusTransactionalManager mongoPlusTransactionalManager;

    public MongoTransactionSynchronization(MongoPlusTransactionalManager mongoPlusTransactionalManager) {
        this.mongoPlusTransactionalManager = mongoPlusTransactionalManager;
    }

    @Override
    public void afterCommit() {
//        TransactionSynchronization.super.afterCommit();
        mongoPlusTransactionalManager.doCommit();
    }

    @Override
    public void afterCompletion(int status) {
        TransactionSynchronization.super.afterCompletion(status);
    }
}
