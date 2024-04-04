package com.anwen.mongo.transactional;

import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.mongodb.ClientSessionOptions;
import com.mongodb.client.ClientSession;

/**
 * Mongo事务
 * 声明式事务请使用{@link com.anwen.mongo.manager.MongoTransactionalManager}
 * @author JiaChaoYang
 **/
@Deprecated
public class MongoTransactionalOperate {

    /**
     * 创建一个session
     *
     * @author JiaChaoYang
     * @date 2023/9/10 16:44
     */
    public static ClientSession createTransaction(){
        return MongoPlusClientCache.mongoPlusClient.getMongoClient().startSession(ClientSessionOptions.builder().causallyConsistent(true).build());
    }

    /**
     * 创建并开启一个事务
     * @author JiaChaoYang
     * @date 2023/9/10 16:47
    */
    public static ClientSession startTransaction(String dataSourceName){
        ClientSession clientSession = MongoPlusClientCache.mongoPlusClient.getMongoClient().startSession(ClientSessionOptions.builder().causallyConsistent(true).build());
        clientSession.startTransaction();
        return clientSession;
    }

    /**
     * 开启一个事务
     * @author JiaChaoYang
     * @date 2023/9/10 16:46
    */
    public static void startTransaction(ClientSession clientSession){
        clientSession.startTransaction();
    }

    /**
     * 提交一个事务
     * @author JiaChaoYang
     * @date 2023/9/10 16:48
    */
    public static void commitTransaction(ClientSession clientSession){
        clientSession.commitTransaction();
    }

    /**
     * 提交并关闭一个事务
     * @author JiaChaoYang
     * @date 2023/9/10 16:50
    */
    public static void commitAndCloseTransaction(ClientSession clientSession){
        clientSession.commitTransaction();
        closeTransaction(clientSession);
    }

    /**
     * 回滚一个事务
     * @author JiaChaoYang
     * @date 2023/9/10 16:49
    */
    public static void rollbackTransaction(ClientSession clientSession){
        clientSession.abortTransaction();
    }

    /**
     * 回滚并关闭一个事务
     * @author JiaChaoYang
     * @date 2023/9/10 16:49
     */
    public static void rollbackAndCloseTransaction(ClientSession clientSession){
        clientSession.abortTransaction();
        closeTransaction(clientSession);
    }

    /**
     * 关闭事务
     * @author JiaChaoYang
     * @date 2023/9/10 16:49
    */
    public static void closeTransaction(ClientSession clientSession){
        clientSession.close();
    }

}
