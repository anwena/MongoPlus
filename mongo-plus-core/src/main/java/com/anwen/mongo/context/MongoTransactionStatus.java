package com.anwen.mongo.context;

import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.mongodb.client.ClientSession;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class MongoTransactionStatus {

    /**
     * 持有的session
     */
    private final Map<String,ClientSession> clientSessionMap = new ConcurrentHashMap<>();

    /**
     * 引用嵌套计数器，表示当前事务方法的第几层
     */
    private long referenceCount;

    public MongoTransactionStatus(ClientSession clientSession) {
        this.clientSessionMap.put(DataSourceNameCache.getDataSource(),clientSession);
        this.referenceCount = 0;
    }

    public ClientSession getClientSession() {
        return this.clientSessionMap.get(DataSourceNameCache.getDataSource());
    }

    public void incrementReference() {
        this.referenceCount++;
    }

    public void decrementReference() {
        this.referenceCount--;
    }

    public void clearReference() {
        this.referenceCount = 0;
    }

    public boolean readyCommit() {
        return this.referenceCount == 0;
    }

    public boolean readyClose() {
        return this.referenceCount <= 0;
    }
}
