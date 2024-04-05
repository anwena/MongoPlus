package com.anwen.mongo.factory;

import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.constant.DataSourceConstant;
import com.mongodb.client.MongoClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * MongoClient工厂
 *
 * @author JiaChaoYang
 **/
public class MongoClientFactory implements AutoCloseable {

    private final Logger logger = LoggerFactory.getLogger(MongoClientFactory.class);

    private final Map<String , MongoClient> mongoClientMap = new ConcurrentHashMap<>();

    private static MongoClientFactory mongoClientFactory;

    private MongoClientFactory() {
    }

    private MongoClientFactory(MongoClient mongoClient){
        new MongoClientFactory(DataSourceConstant.DEFAULT_DATASOURCE,mongoClient);
    }

    private MongoClientFactory(String ds,MongoClient mongoClient){
        mongoClientMap.put(ds,mongoClient);
    }

    public static MongoClientFactory getInstance(String ds,MongoClient mongoClient){
        if (mongoClientFactory == null){
            mongoClientFactory = new MongoClientFactory();
        }
        mongoClientFactory.addMongoClient(ds,mongoClient);
        return mongoClientFactory;
    }

    public static MongoClientFactory getInstance(MongoClient mongoClient){
        return getInstance(DataSourceConstant.DEFAULT_DATASOURCE,mongoClient);
    }

    public static MongoClientFactory getInstance(Map<String,MongoClient> mongoClientMap){
        mongoClientFactory = new MongoClientFactory();
        mongoClientFactory.mongoClientMap.putAll(mongoClientMap);
        return mongoClientFactory;
    }

    /**
     * 获取工厂实例，可能为null(spring-boot-starter项目已经初始化好，如果使用{@link com.anwen.mongo.config.Configuration}构建，可能会为空)
     * @author JiaChaoYang
     * @date 2024/4/5 0:01
    */
    public static MongoClientFactory getInstance(){
        return mongoClientFactory;
    }

    public void addMongoClient(String ds,MongoClient mongoClient){
        mongoClientMap.put(ds,mongoClient);
    }

    public MongoClient getMongoClient(String ds){
        return mongoClientMap.get(ds);
    }

    public MongoClient getMongoClient(){
        return getMongoClient(DataSourceNameCache.getDataSource());
    }

    @Override
    public void close() throws Exception {
        if (logger.isDebugEnabled()){
            logger.debug("Destroy data source connection client");
        }
        System.out.println("Destroy data source connection client");
        mongoClientMap.forEach((ds,mongoClient) -> mongoClient.close());
    }
}
