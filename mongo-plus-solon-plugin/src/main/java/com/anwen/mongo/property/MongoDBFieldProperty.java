package com.anwen.mongo.property;

import com.anwen.mongo.cache.PropertyCache;

/**
 * @author JiaChaoYang
 **/
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
        System.out.println("进来内部的构造函数了");
        PropertyCache.mapUnderscoreToCamelCase = mapUnderscoreToCamelCase;
        this.mapUnderscoreToCamelCase = mapUnderscoreToCamelCase;
    }

}
