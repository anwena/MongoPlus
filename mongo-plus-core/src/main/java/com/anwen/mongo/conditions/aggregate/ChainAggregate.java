package com.anwen.mongo.conditions.aggregate;

import com.mongodb.client.ClientSession;

import java.util.List;

public interface ChainAggregate<T> {

    /**
     * 获取列表 返回T类型的List
     * @return {@link List<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:13
     */
    List<T> list();

    /**
     * 获取列表 返回T类型的List
     * @return {@link List<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:13
     */
    @Deprecated
    List<T> list(ClientSession clientSession);

}
