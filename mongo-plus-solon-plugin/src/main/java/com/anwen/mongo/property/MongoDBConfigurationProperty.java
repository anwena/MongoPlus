package com.anwen.mongo.property;

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

    public Boolean getBanner() {
        return banner;
    }

    public void setBanner(Boolean banner) {
        this.banner = banner;
    }
}
