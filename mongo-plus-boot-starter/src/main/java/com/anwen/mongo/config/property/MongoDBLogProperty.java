package com.anwen.mongo.config.property;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author JiaChaoYang
 * 日志属性
 * @since 2023-06-07 23:07
 **/
@ConfigurationProperties(prefix = "mongo-plus")
public class MongoDBLogProperty {

    /**
     * 是否开启日志
     * @author: JiaChaoYang
     * @date: 2023/6/7 23:08
     **/
    private Boolean log = false;

    /**
     * 是否关闭格式化sql
     * @author JiaChaoYang
     * @date 2023/8/29 0:52
    */
    private Boolean format = false;

    public Boolean getLog() {
        return this.log;
    }

    public Boolean getFormat() {
        return this.format;
    }

    public void setLog(final Boolean log) {
        this.log = log;
    }

    public void setFormat(final Boolean format) {
        this.format = format;
    }

    public MongoDBLogProperty(final Boolean log, final Boolean format) {
        this.log = log;
        this.format = format;
    }

    public MongoDBLogProperty() {
    }

}
