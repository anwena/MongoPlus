package com.anwen.mongo.enums;

/**
 * 显隐字段枚举
 *
 * @author JiaChaoYang
 **/
public enum ProjectionEnum {

    //隐藏
    NONE(0),

    //显示
    DISPLAY(1),

    ;

    private final Integer value;

    public Integer getValue() {
        return value;
    }

    ProjectionEnum(Integer value) {
        this.value = value;
    }
}
