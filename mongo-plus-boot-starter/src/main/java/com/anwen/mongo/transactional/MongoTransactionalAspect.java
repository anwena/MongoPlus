package com.anwen.mongo.transactional;

import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.context.MongoTransactionStatus;
import com.mongodb.ClientSessionOptions;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author JiaChaoYang
 **/
@Aspect
public class MongoTransactionalAspect {

    private static final Logger logger = LoggerFactory.getLogger(MongoTransactionalAspect.class);

    public MongoTransactionalAspect(MongoClient mongoClient) {
        this.mongoClient = mongoClient;
    }

    private final MongoClient mongoClient;

    @Around("@annotation(com.anwen.mongo.annotation.transactional.MongoTransactional)")
    public Object manageTransaction(ProceedingJoinPoint joinPoint) throws Throwable {
        startTransaction();
        try {
            Object proceed = joinPoint.proceed();
            commitTransaction();
            return proceed;
        } catch (Exception e) {
            rollbackTransaction();
            throw e;
        } finally {
            closeSession();
        }
    }

    /**
     * 事务开启
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
    */
    private void startTransaction() {
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
    private void commitTransaction() {
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
    private void rollbackTransaction() {
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

    private void closeSession() {
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
