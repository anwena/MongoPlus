package com.anwen.mongo.context;

import com.mongodb.client.ClientSession;

/**
 * * @description TODO
 * * @author songguoxiang@yunquna.com
 * * @date 2023/09/17 17:42
 */
public class MongoTransactionContext {
    private static final ThreadLocal<MongoTransactionStatus> threadLocalHeaderMap = new ThreadLocal<>();

    public MongoTransactionContext() {
    }

    public static ClientSession getClientSessionContext() {
        MongoTransactionStatus status = getMongoTransactionStatus();
        if (status == null) {
            return null;
        }
        return status.getClientSession();
    }

    public static MongoTransactionStatus getMongoTransactionStatus() {
        return threadLocalHeaderMap.get();
    }

    public static void setTransactionStatus(MongoTransactionStatus mongoTransactionStatus) {
        threadLocalHeaderMap.set(mongoTransactionStatus);
    }

    public static void clear() {
        threadLocalHeaderMap.remove();
    }
}
