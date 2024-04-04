package com.anwen.mongo.transactional;

import com.anwen.mongo.context.MongoTransactionSpring;
import com.mongodb.ClientSessionOptions;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionException;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;
import org.springframework.transaction.support.DefaultTransactionStatus;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.Objects;

/**
 * 自定义事务管理器
 * 不再维护使用Spring的注解控制事务，请使用MongoPlus提供的事务注解，继续使用可能导致多数据源无法回滚。单数据源依然不受影响
 * @author JiaChaoYang
 **/
@Deprecated
public class MongoPlusTransactionalManager extends AbstractPlatformTransactionManager {

    Logger logger = LoggerFactory.getLogger(MongoPlusTransactionalManager.class);

    private final MongoClient mongo;

    public MongoPlusTransactionalManager(MongoClient mongo) {
        this.mongo = mongo;
    }

    @Override
    protected Object doGetTransaction() throws TransactionException {
        return mongo.startSession(ClientSessionOptions.builder().causallyConsistent(true).build());
    }

    @Override
    protected void doBegin(Object transaction, TransactionDefinition definition) throws TransactionException {
        ClientSession clientSession = (ClientSession) transaction;
        TransactionSynchronizationManager.bindResource(Objects.requireNonNull(definition.getName()),clientSession);
        clientSession.startTransaction();
        MongoTransactionSpring.setResources(TransactionSynchronizationManager.getResourceMap());
        MongoTransactionSpring.setCurrentTransactionName(definition.getName());
        if (logger.isDebugEnabled()){
            logger.debug("begin transaction -> name: {} , sessionId: {}",definition.getName(), clientSession.getServerSession().getIdentifier());
        }
    }

    @Override
    protected void doCommit(DefaultTransactionStatus status) throws TransactionException {
        ClientSession clientSession = (ClientSession) status.getTransaction();
        if (clientSession.hasActiveTransaction()) {
            clientSession.commitTransaction();
            clientSession.close();
        }
        MongoTransactionSpring.clear();
        if (logger.isDebugEnabled()){
            logger.debug("commit transaction -> sessionId: {}",clientSession.getServerSession().getIdentifier());
        }
    }

    @Override
    protected void doRollback(DefaultTransactionStatus status) throws TransactionException {
        ClientSession clientSession = (ClientSession) status.getTransaction();
        if (clientSession.hasActiveTransaction()) {
            clientSession.abortTransaction();
            clientSession.close();
        }
        MongoTransactionSpring.clear();
        if (logger.isDebugEnabled()){
            logger.debug("rollback transaction");
        }
    }
}
