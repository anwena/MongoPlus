package com.anwen.mongo.transactional;

import com.anwen.mongo.context.MongoTransactionContext;
import com.mongodb.ClientSessionOptions;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;

/**
 * @author JiaChaoYang
 **/
@Aspect
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
            MongoTransactionContext.clear();
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
        ClientSession clientSession = MongoTransactionContext.getClientSessionContext();
        if (clientSession == null) {
            this.session = mongoClient.startSession(ClientSessionOptions.builder().causallyConsistent(true).build());
            session.startTransaction();
            MongoTransactionContext.setClientSessionContext(session);
        }
        System.out.println("开启事务了。。。");
    }

    /**
     * 事务提交
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
    */
    private void commitTransaction() {
        session.commitTransaction();
        System.out.println("提交事务了。。。");
    }

    /**
     * 事务回滚
     * @author JiaChaoYang
     * @date 2023/7/30 18:16
    */
    private void rollbackTransaction() {
        session.abortTransaction();
        System.out.println("回滚事务了。。。");
    }

    private void closeSession() {
        session.close();
        System.out.println("关闭事务了。。。");
    }
}
