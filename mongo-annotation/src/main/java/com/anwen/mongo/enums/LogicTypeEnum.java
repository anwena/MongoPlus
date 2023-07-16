package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description 0 and 1 or
 * @date 2023-07-16 19:08
 **/
@Getter
@AllArgsConstructor
public enum LogicTypeEnum {

    AND(0),

    OR(1);

    private final Integer key;

}
