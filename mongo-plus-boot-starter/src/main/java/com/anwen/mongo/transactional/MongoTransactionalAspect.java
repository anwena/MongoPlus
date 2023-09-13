package com.anwen.mongo.transactional;

import com.alibaba.fastjson.JSON;
import com.mongodb.ClientSessionOptions;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.bson.BsonDocument;

/**
 * @author JiaChaoYang
 **/
@Aspect
@Deprecated
public class MongoTransactionalAspect {

    public MongoTransactionalAspect(MongoClient mongoClient) {
        this.mongoClient = mongoClient;
    }

    private final MongoClient mongoClient;

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
        this.session = mongoClient.startSession(ClientSessionOptions.builder().causallyConsistent(true).build());
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
