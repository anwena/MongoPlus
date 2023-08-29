package com.anwen.mongo.enums;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description 0 and 1 or
 * @date 2023-07-16 19:08
 **/
public enum LogicTypeEnum {

    AND(0),

    OR(1),

    NOR(2),

    NOT(3),

    ELEMMATCH(4);

    private final Integer key;

    public Integer getKey() {
        return key;
    }

    LogicTypeEnum(Integer key) {
        this.key = key;
    }
}
