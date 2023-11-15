package com.anwen.mongo.property;

import com.anwen.mongo.enums.CollectionNameConvertEnum;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

public class MongoDBCollectionProperty {

    /**
     * collection名称映射策略
     */
    private String mappingStrategy = "all_char_lowercase";


    public CollectionNameConvertEnum getMappingStrategy() {
        return CollectionNameConvertEnum.getConvert(mappingStrategy);
    }

    public void setMappingStrategy(String mappingStrategy) {
        this.mappingStrategy = mappingStrategy;
    }

}
