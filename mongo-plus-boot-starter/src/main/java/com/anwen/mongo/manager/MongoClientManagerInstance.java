package com.anwen.mongo.manager;

import org.springframework.beans.factory.DisposableBean;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-01-05 15:31
 **/
public class MongoClientManagerInstance extends MongoClientManager implements DisposableBean {
    @Override
    public void destroy() throws Exception {
        mongoClientMap.forEach((k,v) -> v.close());
    }
}
