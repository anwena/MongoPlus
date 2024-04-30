package com.anwen.mongo.model;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-02-23 10:42
 **/
public class LogicProperty {

    /**
     * 是否开启逻辑删除功能
     */
    private Boolean open = false;

    /**
     * 是否开启基于拦截器的逻辑删除字段填充（建议方案：使用数据库默认字段 > 其次是手动设置 > 框架自带拦截器 > 自定义拦截器）
     */
    private Boolean autoFill = false;

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

    public void setAutoFill(Boolean autoFill) {
        this.autoFill = autoFill;
    }

    public Boolean getAutoFill() {
        return autoFill;
    }

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
