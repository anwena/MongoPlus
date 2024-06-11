package com.anwen.mongo.enums;

/**
 * 排序枚举
 * @author JiaChaoYang
 * @date 2023-02-19 13:25
 */
public enum OrderEnum {

    ASC(1),

    DESC(-1)

    ;

    private final Integer value;

    public Integer getValue() {
        return value;
    }

    OrderEnum(Integer value) {
        this.value = value;
    }
}
