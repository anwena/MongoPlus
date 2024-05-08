package com.anwen.mongo.transactional;

import com.anwen.mongo.annotation.transactional.MongoTransactional;
import com.anwen.mongo.manager.MongoTransactionalManager;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.annotation.Order;

/**
 * AOP操作，实现声明式事务
 *
 * @author JiaChaoYang
 **/
@Aspect
@Order(1)
public class MongoTransactionalAspect {

    @Pointcut("@annotation(com.anwen.mongo.annotation.transactional.MongoTransactional)")
    private void markMongoTransactional() {
    }

    @Around(value = "markMongoTransactional() && @annotation(mongoTransactional)")
    public Object manageTransaction(ProceedingJoinPoint joinPoint, MongoTransactional mongoTransactional) throws Throwable {
        System.out.println(mongoTransactional);
        MongoTransactionalManager.startTransaction(mongoTransactional);
        try {
            Object proceed = joinPoint.proceed();
            MongoTransactionalManager.commitTransaction();
            return proceed;
        } catch (Exception e) {
            MongoTransactionalManager.rollbackTransaction();
            throw e;
        } finally {
            MongoTransactionalManager.closeSession();
        }
    }
}
