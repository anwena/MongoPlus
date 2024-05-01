package com.anwen.mongo.property;

import com.anwen.mongo.cache.global.PropertyCache;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author JiaChaoYang
 **/
@ConfigurationProperties(prefix = "mongo-plus.configuration.field")
public class MongoDBFieldProperty {

    /**
     * 下划线转驼峰
     * @author JiaChaoYang
     * @date 2023/10/12 0:09
     */
    private Boolean mapUnderscoreToCamelCase = false;

    /**
     * 是否忽略null属性
     * @author JiaChaoYang
     * @date 2024/5/1 下午1:30
     */
    private Boolean ignoringNull = true;

    public Boolean getIgnoringNull() {
        return ignoringNull;
    }

    public void setIgnoringNull(Boolean ignoringNull) {
        PropertyCache.ignoringNull = ignoringNull;
        this.ignoringNull = ignoringNull;
    }

    public Boolean getMapUnderscoreToCamelCase() {
        return mapUnderscoreToCamelCase;
    }

    public void setMapUnderscoreToCamelCase(Boolean mapUnderscoreToCamelCase) {
        PropertyCache.mapUnderscoreToCamelCase = mapUnderscoreToCamelCase;
        this.mapUnderscoreToCamelCase = mapUnderscoreToCamelCase;
    }

}
