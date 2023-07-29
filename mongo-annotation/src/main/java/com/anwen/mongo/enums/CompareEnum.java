package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 0查询，1修改
 * @author JiaChaoYang
 * @date 2023/7/9 22:06
*/
@Getter
@AllArgsConstructor
public enum CompareEnum {

    QUERY(0),

    UPDATE(1);

    private final int key;

}
