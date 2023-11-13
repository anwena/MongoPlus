package com.anwen.mongo.model;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 分组依据字段
 * @date 2023-11-13 16:32
 **/
public class GroupField {

    /**
     * 分组依据字段
     * @author JiaChaoYang
     * @date 2023/11/13 16:34
    */
    private String groupField;

    /**
     * 引用字段
     * @author JiaChaoYang
     * @date 2023/11/13 16:34
    */
    private String field;

    public String getGroupField() {
        return groupField;
    }

    public void setGroupField(String groupField) {
        this.groupField = groupField;
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

    public GroupField(String groupField, String field) {
        this.groupField = groupField;
        this.field = field;
    }

    public GroupField() {
    }
}
