package com.anwen.mongo.transactional;

import com.anwen.mongo.annotation.transactional.MongoTransactional;
import com.anwen.mongo.manager.MongoTransactionalManager;
import com.anwen.mongo.toolkit.ArrayUtils;
import com.anwen.mongo.toolkit.ClassTypeUtil;
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

        MongoTransactionalManager.startTransaction(mongoTransactional);
        try {
            Object proceed = joinPoint.proceed();
            MongoTransactionalManager.commitTransaction();
            return proceed;
        } catch (Exception e) {
            Class<? extends Exception> eClass = e.getClass();
            boolean finish = doRollBack(mongoTransactional, eClass);
            if (!finish) {
                finish = doUnRollBack(mongoTransactional, eClass);
            }
            if (!finish) {
                MongoTransactionalManager.rollbackTransaction();
            }
            throw e;
        } finally {
            MongoTransactionalManager.closeSession();
        }

    }

    private static boolean doUnRollBack(MongoTransactional mongoTransactional, Class<? extends Exception> eClass) {

        Class<? extends Throwable>[] noRollBackList = mongoTransactional.noRollbackFor();
        if (ArrayUtils.isEmpty(noRollBackList)) {
            return false;
        }
        for (Class<? extends Throwable> eType : noRollBackList) {
            if (ClassTypeUtil.isTargetClass(eType,eClass)) {
                MongoTransactionalManager.commitTransaction();
                return true;
            }
        }
        return false;

    }

    private static boolean doRollBack(MongoTransactional mongoTransactional, Class<? extends Exception> eClass) {

        Class<? extends Throwable>[] rollBackList = mongoTransactional.rollbackFor();
        if (ArrayUtils.isEmpty(rollBackList)) {
            return false;
        }
        for (Class<? extends Throwable> eType : rollBackList) {
            if (ClassTypeUtil.isTargetClass(eType,eClass)) {
                MongoTransactionalManager.rollbackTransaction();
                return true;
            }
        }
        return false;

    }

}
