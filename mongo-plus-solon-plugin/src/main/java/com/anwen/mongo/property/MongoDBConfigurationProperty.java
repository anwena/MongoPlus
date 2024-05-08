package com.anwen.mongo.property;

import com.anwen.mongo.cache.global.PropertyCache;

/**
 * configuration属性配置
 *
 * @author JiaChaoYang
 **/
public class MongoDBConfigurationProperty {

    /**
     * banner打印
     * @author JiaChaoYang
     * @date 2024/1/26 21:58
    */
    private Boolean banner = true;

    private Boolean ikun = false;

    /**
     * 存放自增id的集合
     * @author JiaChaoYang
     * @date 2024/5/1 下午8:55
     */
    private String autoIdCollectionName;

    public String getAutoIdCollectionName() {
        return autoIdCollectionName;
    }

    public void setAutoIdCollectionName(String autoIdCollectionName) {
        PropertyCache.autoIdCollectionName = autoIdCollectionName;
        this.autoIdCollectionName = autoIdCollectionName;
    }

    public Boolean getIkun() {
        return ikun;
    }

    public void setIkun(Boolean ikun) {
        PropertyCache.ikun = ikun;
        this.ikun = ikun;
    }

    public Boolean getBanner() {
        return banner;
    }

    public void setBanner(Boolean banner) {
        this.banner = banner;
    }
}
