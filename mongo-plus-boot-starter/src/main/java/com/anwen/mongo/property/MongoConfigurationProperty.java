package com.anwen.mongo.property;

import com.anwen.mongo.cache.PropertyCache;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author JiaChaoYang
 **/
@ConfigurationProperties(prefix = "mongo-plus.configuration")
public class MongoConfigurationProperty {

    /**
     * 字段配置
     * @author JiaChaoYang
     * @date 2023/11/14 22:16
    */
    private MongoDBFieldProperty field;

    /**
     * 集合配置
     * @author JiaChaoYang
     * @date 2023/11/14 22:16
    */
    private MongoDBCollectionProperty collection;

    public MongoDBFieldProperty getField() {
        return field;
    }

    public void setField(MongoDBFieldProperty field) {
        System.out.println("进来外部的构造函数了");
        PropertyCache.mapUnderscoreToCamelCase = field.getMapUnderscoreToCamelCase();
        this.field = field;
    }

    public MongoDBCollectionProperty getCollection() {
        return collection;
    }

    public void setCollection(MongoDBCollectionProperty collection) {
        this.collection = collection;
    }
}
