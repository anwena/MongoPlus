package com.anwen.mongo.transactional;

import cn.hutool.json.JSONUtil;
import com.mongodb.client.ClientSession;
import lombok.extern.log4j.Log4j2;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.bson.BsonDocument;

/**
 * @author JiaChaoYang
 **/
@Aspect
@Log4j2
public class MongoTransactionalAspect {
    public MongoTransactionalAspect(ClientSession session) {
        this.session = session;
    }

    private final ClientSession session;

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
        BsonDocument document = session.getServerSession().getIdentifier();
        System.out.println("开启时候的session："+ JSONUtil.toJsonStr(document));
        session.startTransaction();
    }

    /**
     * 事务提交
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
    */
    private void commitTransaction() {
        BsonDocument document = session.getServerSession().getIdentifier();
        System.out.println("提交时候的session："+ JSONUtil.toJsonStr(document));
        session.commitTransaction();
    }

    /**
     * 事务回滚
     * @author JiaChaoYang
     * @date 2023/7/30 18:16
    */
    private void rollbackTransaction() {
        BsonDocument document = session.getServerSession().getIdentifier();
        System.out.println("回滚时候的session："+ JSONUtil.toJsonStr(document));
        session.abortTransaction();
    }

    private void closeSession() {
        BsonDocument document = session.getServerSession().getIdentifier();
        System.out.println("关闭时候的session："+ JSONUtil.toJsonStr(document));
        session.close();
    }
}
