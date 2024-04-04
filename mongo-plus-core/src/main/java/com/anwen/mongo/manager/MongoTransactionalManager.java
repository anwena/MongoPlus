package com.anwen.mongo.manager;

import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.context.MongoTransactionStatus;
import com.anwen.mongo.factory.MongoClientFactory;
import com.mongodb.ClientSessionOptions;
import com.mongodb.MongoException;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Mongo事务管理
 *
 * @author JiaChaoYang
 **/
public class MongoTransactionalManager {

    private static final Logger logger = LoggerFactory.getLogger(MongoTransactionalManager.class);

    /**
     * 事务开启
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
     */
    public static void startTransaction() {
        MongoClientFactory mongoClientFactory = MongoClientFactory.getInstance();
        if (mongoClientFactory == null){
            throw new MongoException("Please initialize MongoClientFactory first");
        }
        startTransaction(mongoClientFactory.getMongoClient());
    }

    public static void startTransaction(MongoClient mongoClient) {
        //获取线程中的session
        ClientSession session = MongoTransactionContext.getClientSessionContext();
        if (session == null) {
            session = mongoClient.startSession(ClientSessionOptions.builder().causallyConsistent(true).build());
            session.startTransaction();
            MongoTransactionStatus status = new MongoTransactionStatus(session);
            MongoTransactionContext.setTransactionStatus(status);
        }
        // 每个被切到的方法都引用加一
        MongoTransactionContext.getMongoTransactionStatus().incrementReference();
        if (logger.isDebugEnabled()) {
            logger.debug("Mongo transaction created, Thread:{}, session hashcode:{}", Thread.currentThread().getName(), session.hashCode());
        }
    }

    /**
     * 事务提交
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
     */
    public static void commitTransaction() {
        MongoTransactionStatus status = MongoTransactionContext.getMongoTransactionStatus();
        if (status == null) {
            logger.warn("no session to commit.");
            return;
        }
        status.decrementReference();
        if (status.readyCommit()) {
            ClientSession clientSession = status.getClientSession();
            if (clientSession.hasActiveTransaction()){
                clientSession.commitTransaction();
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Mongo transaction committed, Thread:{}, session hashcode:{}", Thread.currentThread().getName(), status.getClientSession().hashCode());
        }
    }

    /**
     * 事务回滚
     * @author JiaChaoYang
     * @date 2023/7/30 18:16
     */
    public static void rollbackTransaction() {
        MongoTransactionStatus status = MongoTransactionContext.getMongoTransactionStatus();
        if (status == null) {
            logger.warn("no session to rollback.");
            return;
        }
        // 清空计数器
        status.clearReference();
        ClientSession clientSession = status.getClientSession();
        if (clientSession.hasActiveTransaction()){
            clientSession.abortTransaction();
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Mongo transaction rolled back, Thread:{}, session hashcode:{}", Thread.currentThread().getName(), status.getClientSession().hashCode());
        }
    }

    public static void closeSession() {
        MongoTransactionStatus status = MongoTransactionContext.getMongoTransactionStatus();
        if (status == null) {
            logger.warn("no session to rollback.");
            return;
        }
        if (status.readyClose()) {
            try {
                ClientSession clientSession = status.getClientSession();
                if (clientSession.hasActiveTransaction()){
                    clientSession.close();
                }
            } finally {
                // 确保清理线程变量时不会被打断
                MongoTransactionContext.clear();
            }
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Mongo transaction closed, Thread:{}, session hashcode:{}", Thread.currentThread().getName(), status.getClientSession().hashCode());
        }
    }

}
