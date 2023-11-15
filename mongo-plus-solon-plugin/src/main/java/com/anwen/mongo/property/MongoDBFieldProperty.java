package com.anwen.mongo.property;

import com.anwen.mongo.cache.PropertyCache;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

/**
 * @author JiaChaoYang
 **/
@Inject(value = "${mongo-plus.configuration.field}",required = false,autoRefreshed = true)
@Configuration
public class MongoDBFieldProperty {

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
