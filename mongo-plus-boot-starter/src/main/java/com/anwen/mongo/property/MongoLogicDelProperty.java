package com.anwen.mongo.property;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * 逻辑删除配置
 *
 * @author loser
 * @date 2024/4/28
 */
@ConfigurationProperties(prefix = "mongo-plus.configuration.logic")
public class MongoLogicDelProperty {

    /**
     * 是否开启逻辑删除功能
     */
    private Boolean open = false;

    /**
     * 逻辑删除全局属性名
     */
    private String logicDeleteField;

    /**
     * 逻辑删除全局值（默认 1、表示已删除）
     */
    private String logicDeleteValue = "1";

    /**
     * 逻辑未删除全局值（默认 0、表示未删除）
     */
    private String logicNotDeleteValue = "0";

    public Boolean getOpen() {
        return open;
    }

    public void setOpen(Boolean open) {
        this.open = open;
    }

    public String getLogicDeleteField() {
        return logicDeleteField;
    }

    public void setLogicDeleteField(String logicDeleteField) {
        this.logicDeleteField = logicDeleteField;
    }

    public String getLogicDeleteValue() {
        return logicDeleteValue;
    }

    public void setLogicDeleteValue(String logicDeleteValue) {
        this.logicDeleteValue = logicDeleteValue;
    }

    public String getLogicNotDeleteValue() {
        return logicNotDeleteValue;
    }

    public void setLogicNotDeleteValue(String logicNotDeleteValue) {
        this.logicNotDeleteValue = logicNotDeleteValue;
    }
}
