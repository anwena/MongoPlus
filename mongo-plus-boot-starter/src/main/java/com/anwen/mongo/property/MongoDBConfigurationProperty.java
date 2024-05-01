package com.anwen.mongo.property;

import com.anwen.mongo.cache.global.PropertyCache;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * configuration属性配置
 *
 * @author JiaChaoYang
 **/
@ConfigurationProperties(prefix = "mongo-plus.configuration")
public class MongoDBConfigurationProperty {

    /**
     * banner打印
     * @author JiaChaoYang
     * @date 2024/1/26 21:58
    */
    private Boolean banner = true;

    /**
     * 存放自增id的集合
     * @author JiaChaoYang
     * @date 2024/5/1 下午8:55
     */
    private String autoIdCollectionName = "counters";

    public String getAutoIdCollectionName() {
        return autoIdCollectionName;
    }

    public void setAutoIdCollectionName(String autoIdCollectionName) {
        PropertyCache.autoIdCollectionName = autoIdCollectionName;
        this.autoIdCollectionName = autoIdCollectionName;
    }

    public Boolean getBanner() {
        return banner;
    }

    public void setBanner(Boolean banner) {
        this.banner = banner;
    }
}
