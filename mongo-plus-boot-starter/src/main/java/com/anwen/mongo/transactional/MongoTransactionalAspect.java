package com.anwen.mongo.transactional;

import com.mongodb.ClientSessionOptions;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;
import lombok.extern.log4j.Log4j2;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;

/**
 * @author JiaChaoYang
 **/
@Aspect
@Log4j2
public class MongoTransactionalAspect {

    private final MongoClient mongoClient;

    public MongoTransactionalAspect(MongoClient mongoClient) {
        this.mongoClient = mongoClient;
    }

    private ClientSession session;

    @Around("@annotation(com.anwen.mongo.annotation.transactional.MongoTransactional)")
    public Object manageTransaction(ProceedingJoinPoint joinPoint) throws Throwable {
        startTransaction();
        try {
            Object proceed = joinPoint.proceed();
            commitTransaction();
            return proceed;
        } catch (Exception e) {
            rollbackTransaction();
            throw new RuntimeException(e);
        }finally {
            closeSession();
        }
    }

    /**
     * 事务开启
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
    */
    private void startTransaction() {
        session = mongoClient.startSession(ClientSessionOptions.builder().causallyConsistent(true).build());
        session.startTransaction();
    }

    /**
     * 事务提交
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
    */
    private void commitTransaction() {
        session.commitTransaction();
    }

    /**
     * 事务回滚
     * @author JiaChaoYang
     * @date 2023/7/30 18:16
    */
    private void rollbackTransaction() {
        session.abortTransaction();
    }

    private void closeSession() {
        session.close();
    }
}
