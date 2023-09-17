package com.anwen.mongo.context;

import com.mongodb.client.ClientSession;

/**
 * * @description TODO
 * * @author songguoxiang@yunquna.com
 * * @date 2023/09/17 17:42
 */
public class MongoTransactionContext {
    private static ThreadLocal<ClientSession> threadLocalHeaderMap = new ThreadLocal();

    public MongoTransactionContext() {
    }

    public static ClientSession getClientSessionContext() {
        return threadLocalHeaderMap.get();
    }

    public static void setClientSessionContext(ClientSession clientSession) {
        threadLocalHeaderMap.set(clientSession);
    }

    public static void clear() {
        threadLocalHeaderMap.set(null);
    }
}
