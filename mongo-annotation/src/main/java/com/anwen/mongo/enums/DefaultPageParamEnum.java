package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-02-23 11:00
 **/
@Getter
@AllArgsConstructor
public enum DefaultPageParamEnum {

    PAGE_NUM(1),

    PAGE_SIZE(10);

    private final Integer num;
}
