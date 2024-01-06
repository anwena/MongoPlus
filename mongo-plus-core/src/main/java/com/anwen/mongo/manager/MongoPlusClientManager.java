package com.anwen.mongo.manager;

import java.util.HashMap;
import java.util.Map;

/**
 * MongoClient管理器
 *
 * @author JiaChaoYang
 * @project mongo-plus
 * @date 2024-01-05 14:04
 **/
public class MongoPlusClientManager {

    /**
     * 封装后的Client
     * @author JiaChaoYang
     * @date 2024/1/6 1:34
    */
    protected final Map<String,MongoPlusClient> mongoPlusClientMap = new HashMap<>();

    public void setMongoPlusClient(String key,MongoPlusClient mongoPlusClient){
        mongoPlusClientMap.put(key,mongoPlusClient);
    }

    public MongoPlusClient getMongoPlusClient(String key){
        return mongoPlusClientMap.get(key);
    }

}