package com.anwen.mongo.manager;

import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.factory.MongoClientFactory;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.toolkit.MongoUtil;
import com.mongodb.client.MongoClient;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.stream.Collectors;

/**
 * 多数据源处理器，可以通过该类
 * @author anwen
 * @date 2024/7/9 下午3:51
 */
public class DataSourceManager {

    private final MongoPlusClient mongoPlusClient;

    private final CollectionNameConvert collectionNameConvert;

    private final MongoClientFactory mongoClientFactory;

    public DataSourceManager(MongoPlusClient mongoPlusClient,
                             CollectionNameConvert collectionNameConvert,
                             MongoClientFactory mongoClientFactory){
        this.mongoPlusClient = mongoPlusClient;
        this.collectionNameConvert = collectionNameConvert;
        this.mongoClientFactory = mongoClientFactory;
    }

    /**
     * 将一个临时的数据源添加到MongoPlusClient中
     * @param dsName 数据源名称
     * @param baseProperty 数据源配置
     * @author anwen
     * @date 2024/7/9 下午4:02
     */
    public void appendTempDataSource(String dsName, BaseProperty baseProperty,boolean isOverride){
        MongoClient mongoClient = MongoUtil.getMongo(dsName,baseProperty);
        Boolean containsMongoClient = mongoClientFactory.containsMongoClient(dsName);
        if (!containsMongoClient || isOverride) {
            mongoClientFactory.addMongoClient(dsName, mongoClient);
            mongoPlusClient.getCollectionManagerMap().put(dsName,new LinkedHashMap<String, CollectionManager>(){{
                Arrays.stream(baseProperty.getDatabase().split(",")).collect(Collectors.toList()).forEach(db -> put(db,new CollectionManager(mongoClient,collectionNameConvert,db)));
            }});
        }
    }

    /**
     * 切换为指定数据源
     * <p style='color: red'>需要手动的去清除缓存</p>
     * <p style='color: red'>不推荐使用</p>
     * @param dsName 数据源名称
     * @author anwen
     * @date 2024/7/9 下午4:46
     */
    public void changeDataSource(String dsName){
        DataSourceNameCache.setDataSource(dsName);
    }

    /**
     * 清除上下文中的数据源缓存
     * <p style='color: red'>不推荐使用</p>
     * @author anwen
     * @date 2024/7/9 下午4:46
     */
    public void clearDataSource(){
        DataSourceNameCache.clear();
    }

    /**
     * 获取当前线程的数据源
     * @return {@link String}
     * @author anwen
     * @date 2024/7/9 下午4:59
     */
    public String currentDataSourceName(){
        return DataSourceNameCache.getDataSource();
    }

}
