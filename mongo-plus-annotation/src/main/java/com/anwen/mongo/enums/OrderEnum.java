package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @Description: 排序枚举
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.enums
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-19 13:25
 * @Version: 1.0
 */
@Getter
@AllArgsConstructor
public enum OrderEnum {

    ORDER_BY(1),

    ORDER_BY_DESC(-1)

    ;

    private final Integer flag;

}
