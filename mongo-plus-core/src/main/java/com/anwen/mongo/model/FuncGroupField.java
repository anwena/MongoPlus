package com.anwen.mongo.model;

import com.anwen.mongo.support.SFunction;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 分组依据字段，函数式
 * @date 2023-11-13 16:36
 **/
public class FuncGroupField<T> {

    /**
     * 分组依据字段
     * @author JiaChaoYang
     * @date 2023/11/13 16:34
     */
    private SFunction<T,Object> groupField;

    /**
     * 引用字段
     * @author JiaChaoYang
     * @date 2023/11/13 16:34
     */
    private SFunction<T,Object> field;

    public String getGroupField() {
        return groupField.getFieldNameLine();
    }

    public void setGroupField(SFunction<T, Object> groupField) {
        this.groupField = groupField;
    }

    public String getField() {
        return field.getFieldNameLine();
    }

    public void setField(SFunction<T, Object> field) {
        this.field = field;
    }

    public FuncGroupField(SFunction<T, Object> groupField, SFunction<T, Object> field) {
        this.groupField = groupField;
        this.field = field;
    }

    public FuncGroupField() {
    }
}
