package com.anwen.mongo.manager;

import com.anwen.mongo.annotation.transactional.MongoReadPreference;
import com.anwen.mongo.annotation.transactional.MongoTransactional;
import com.anwen.mongo.context.MongoTransactionContext;
import com.anwen.mongo.context.MongoTransactionStatus;
import com.anwen.mongo.domain.InitMongoPlusException;
import com.anwen.mongo.enums.ReadConcernEnum;
import com.anwen.mongo.enums.ReadPreferenceEnum;
import com.anwen.mongo.enums.WriteConcernEnum;
import com.anwen.mongo.factory.MongoClientFactory;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;
import com.mongodb.*;
import com.mongodb.client.ClientSession;
import com.mongodb.client.MongoClient;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * Mongo事务管理
 *
 * @author JiaChaoYang
 **/
public class MongoTransactionalManager {

    private static final Log log = LogFactory.getLog(MongoTransactionalManager.class);

    public static MongoClient getMongoClient() {

        MongoClientFactory mongoClientFactory = MongoClientFactory.getInstance();
        if (mongoClientFactory == null) {
            throw new InitMongoPlusException("Please initialize MongoClientFactory first");
        }
        return mongoClientFactory.getMongoClient();

    }

    /**
     * 事务开启
     *
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
     */
    public static void startTransaction() {
        startTransaction(getMongoClient());
    }

    public static void startTransaction(MongoClient mongoClient) {
        startTransaction(mongoClient, null);
    }

    public static void startTransaction(MongoTransactional transactional) {
        startTransaction(getMongoClient(), transactional);
    }

    public static void startTransaction(MongoClient mongoClient, MongoTransactional transactional) {

        //获取线程中的session
        ClientSession session = MongoTransactionContext.getClientSessionContext();
        if (session == null) {
            ClientSessionOptions.Builder builder = ClientSessionOptions.builder();
            builder.causallyConsistent(true);
            if (Objects.nonNull(transactional)) {
                config(transactional, builder);
            }
            session = mongoClient.startSession(builder.build());
            session.startTransaction();
            MongoTransactionStatus status = new MongoTransactionStatus(session);
            MongoTransactionContext.setTransactionStatus(status);
        }
        // 每个被切到的方法都引用加一
        MongoTransactionContext.getMongoTransactionStatus().incrementReference();
        if (log.isDebugEnabled()) {
            log.debug("Mongo transaction created, Thread:{}, session hashcode:{}", Thread.currentThread().getName(), session.hashCode());
        }

    }

    /**
     * 存在事务注解则进行自定义配置
     *
     * @param transactional 事务注解
     * @param builder       回话配置对象
     */
    private static void config(MongoTransactional transactional, ClientSessionOptions.Builder builder) {

        builder.causallyConsistent(transactional.causallyConsistent())
                .snapshot(transactional.snapshot());
        ReadConcern readConcern = buildReadConcern(transactional.readConcern());
        WriteConcern writeConcern = buildWriteConcern(transactional.writeConcern());
        ReadPreference readPreference = buildReadPreference(transactional.preference());
        TransactionOptions.Builder tsPBuilder = TransactionOptions.builder();
        if (transactional.maxCommitTimeMS() > 0) {
            tsPBuilder.maxCommitTime(transactional.maxCommitTimeMS(), transactional.timeUnit());
        }
        Optional.ofNullable(readConcern).ifPresent(tsPBuilder::readConcern);
        Optional.ofNullable(writeConcern).ifPresent(tsPBuilder::writeConcern);
        Optional.ofNullable(readPreference).ifPresent(tsPBuilder::readPreference);
        builder.defaultTransactionOptions(tsPBuilder.build());

    }

    /**
     * ReadPreference 是一个枚举类型，用于指定数据从不同节点读取的偏好。
     *
     * @param preferences 配置参数
     * @return 选择的读取偏好设置
     * @author loser
     */
    private static ReadPreference buildReadPreference(MongoReadPreference[] preferences) {

        if (Objects.isNull(preferences) || preferences.length != 1) {
            return null;
        }
        MongoReadPreference preference = preferences[0];
        ReadPreferenceEnum preferenceEnum = preference.preferenceEnum();
        long maxStaleness = preference.maxStaleness();
        TimeUnit timeUnit = preference.timeUnit();
        if (maxStaleness > 0) {
            return getReadPreference(preferenceEnum, maxStaleness, timeUnit);
        } else {
            return getReadPreference(preferenceEnum);
        }

    }

    /**
     * 不带超时时间
     *
     * @author loser
     */
    private static ReadPreference getReadPreference(ReadPreferenceEnum preferenceEnum) {

        switch (preferenceEnum) {
            case PRIMARY:
                return ReadPreference.primary();
            case PRIMARY_PREFERRED:
                return ReadPreference.primaryPreferred();
            case SECONDARY:
                return ReadPreference.secondary();
            case SECONDARY_PREFERRED:
                return ReadPreference.secondaryPreferred();
            case NEAREST:
                return ReadPreference.nearest();
            default:
                return null;
        }

    }

    /**
     * 带超时时间
     *
     * @author loser
     */
    private static ReadPreference getReadPreference(ReadPreferenceEnum preferenceEnum, long maxStaleness, TimeUnit timeUnit) {

        switch (preferenceEnum) {
            case PRIMARY:
                return ReadPreference.primary();
            case PRIMARY_PREFERRED:
                return ReadPreference.primaryPreferred(maxStaleness, timeUnit);
            case SECONDARY:
                return ReadPreference.secondary(maxStaleness, timeUnit);
            case SECONDARY_PREFERRED:
                return ReadPreference.secondaryPreferred(maxStaleness, timeUnit);
            case NEAREST:
                return ReadPreference.nearest(maxStaleness, timeUnit);
            default:
                return null;
        }

    }

    /**
     * WriteConcern 用于定义写入操作的安全等级。 WriteConcern 决定了一个写入操作需要被多少个节点确认才算成功。这可以用于保证数据的一致性和可靠性，在面对事务性操作时尤其重要
     *
     * @param writeConcernEnum 写入操作的安全等级枚举
     * @return 写入操作的安全等级
     * @author loser
     */
    private static WriteConcern buildWriteConcern(WriteConcernEnum writeConcernEnum) {

        if (Objects.isNull(writeConcernEnum)) {
            return null;
        }
        switch (writeConcernEnum) {
            case ACKNOWLEDGED:
                return WriteConcern.ACKNOWLEDGED;
            case UNACKNOWLEDGED:
                return WriteConcern.UNACKNOWLEDGED;
            case MAJORITY:
                return WriteConcern.MAJORITY;
            case W1:
                return WriteConcern.W1;
            case W2:
                return WriteConcern.W2;
            case W3:
                return WriteConcern.W3;
            case JOURNALED:
                return WriteConcern.JOURNALED;
            default:
                return null;
        }

    }

    /**
     * readConcern 用于定义读取操作的一致性和隔离级别。 readConcern 可以在每个操作或每个会话的基础上进行设置。对于事务，必须设置 readConcern 为 majority 以确保事务期间的读取能反映在数据的大多数副本上的最新数据
     *
     * @param readConcernEnum 读取配置枚举
     * @return 读取配置
     * @author loser
     */
    private static ReadConcern buildReadConcern(ReadConcernEnum readConcernEnum) {

        if (Objects.isNull(readConcernEnum)) {
            return null;
        }
        switch (readConcernEnum) {
            case DEFAULT:
                return ReadConcern.DEFAULT;
            case LOCAL:
                return ReadConcern.LOCAL;
            case MAJORITY:
                return ReadConcern.MAJORITY;
            case LINEARIZABLE:
                return ReadConcern.LINEARIZABLE;
            case AVAILABLE:
                return ReadConcern.AVAILABLE;
            case SNAPSHOT:
                return ReadConcern.SNAPSHOT;
            default:
                return null;
        }

    }


    /**
     * 事务提交
     *
     * @author JiaChaoYang
     * @date 2023/7/30 18:15
     */
    public static void commitTransaction() {
        MongoTransactionStatus status = MongoTransactionContext.getMongoTransactionStatus();
        if (status == null) {
            log.warn("no session to commit.");
            return;
        }
        status.decrementReference();
        if (status.readyCommit()) {
            ClientSession clientSession = status.getClientSession();
            if (clientSession.hasActiveTransaction()) {
                clientSession.commitTransaction();
            }
        }
        if (log.isDebugEnabled()) {
            log.debug("Mongo transaction committed, Thread:{}, session hashcode:{}", Thread.currentThread().getName(), status.getClientSession().hashCode());
        }
    }

    /**
     * 事务回滚
     *
     * @author JiaChaoYang
     * @date 2023/7/30 18:16
     */
    public static void rollbackTransaction() {
        MongoTransactionStatus status = MongoTransactionContext.getMongoTransactionStatus();
        if (status == null) {
            log.warn("no session to rollback.");
            return;
        }
        // 清空计数器
        status.clearReference();
        ClientSession clientSession = status.getClientSession();
        if (clientSession.hasActiveTransaction()) {
            clientSession.abortTransaction();
        }
        if (log.isDebugEnabled()) {
            log.debug("Mongo transaction rolled back, Thread:{}, session hashcode:{}", Thread.currentThread().getName(), status.getClientSession().hashCode());
        }
    }

    public static void closeSession() {
        MongoTransactionStatus status = MongoTransactionContext.getMongoTransactionStatus();
        if (status == null) {
            log.warn("no session to rollback.");
            return;
        }
        if (status.readyClose()) {
            try {
                ClientSession clientSession = status.getClientSession();
                if (clientSession.hasActiveTransaction()) {
                    clientSession.close();
                }
            } finally {
                // 确保清理线程变量时不会被打断
                MongoTransactionContext.clear();
            }
        }
        if (log.isDebugEnabled()) {
            log.debug("Mongo transaction closed, Thread:{}, session hashcode:{}", Thread.currentThread().getName(), status.getClientSession().hashCode());
        }
    }

}
