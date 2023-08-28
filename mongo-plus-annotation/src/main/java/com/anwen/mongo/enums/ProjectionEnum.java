package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 显隐字段枚举
 *
 * @author JiaChaoYang
 **/
@Getter
@AllArgsConstructor
public enum ProjectionEnum {

    //隐藏
    NONE(0),

    //显示
    DISPLAY(1),

    ;

    private final Integer value;

}
