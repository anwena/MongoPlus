package com.anwen.mongo.context;

import com.mongodb.client.ClientSession;

public class MongoTransactionStatus {

    /**
     * 持有的session
     */
    private final ClientSession clientSession;

    /**
     * 引用嵌套计数器，表示当前事务方法的第几层
     */
    private long referenceCount;

    public MongoTransactionStatus(ClientSession clientSession) {
        this.clientSession = clientSession;
        this.referenceCount = 0;
    }

    public ClientSession getClientSession() {
        return this.clientSession;
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
