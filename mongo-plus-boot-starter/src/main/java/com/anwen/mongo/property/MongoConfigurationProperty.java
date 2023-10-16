package com.anwen.mongo.property;

import com.anwen.mongo.cache.PropertyCache;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author JiaChaoYang
 **/
@ConfigurationProperties(prefix = "mongo-plus.configuration")
public class MongoConfigurationProperty {
    
    /**
     * 下划线转驼峰
     * @author JiaChaoYang
     * @date 2023/10/12 0:09
    */ 
    private Boolean mapUnderscoreToCamelCase = false;

    public Boolean getMapUnderscoreToCamelCase() {
        return mapUnderscoreToCamelCase;
    }

    public void setMapUnderscoreToCamelCase(Boolean mapUnderscoreToCamelCase) {
        PropertyCache.mapUnderscoreToCamelCase = mapUnderscoreToCamelCase;
        this.mapUnderscoreToCamelCase = mapUnderscoreToCamelCase;
    }
}
