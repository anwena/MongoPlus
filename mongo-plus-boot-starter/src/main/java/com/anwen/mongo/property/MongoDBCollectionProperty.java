package com.anwen.mongo.property;

import com.anwen.mongo.enums.CollectionNameConvertEnum;
import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "mongo-plus.configuration.collection")
public class MongoDBCollectionProperty {

    /**
     * collection名称映射策略
     */
    private CollectionNameConvertEnum mappingStrategy = CollectionNameConvertEnum.ALL_CHAR_LOWERCASE;


    public CollectionNameConvertEnum getMappingStrategy() {
        return mappingStrategy;
    }

    public void setMappingStrategy(CollectionNameConvertEnum mappingStrategy) {
        this.mappingStrategy = mappingStrategy;
    }

}
