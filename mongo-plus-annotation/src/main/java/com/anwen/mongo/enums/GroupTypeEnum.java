package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * group条件枚举
 * @author JiaChaoYang
 * @date 2023/8/17 20:01
*/
@Getter
@AllArgsConstructor
public enum GroupTypeEnum {
    // 计算指定字段的总和
    SUM("sum"),

    // 计算指定字段的平均值
    AVG("avg"),

    // 查找指定字段的最小值
    MIN("min"),

    // 查找指定字段的最大值
    MAX("max"),

    // 获取指定字段在分组中的第一个文档的值
    FIRST("first"),

    // 获取指定字段在分组中的最后一个文档的值
    LAST("last"),

    // 将指定字段的值添加到数组中
    PUSH("push"),

    // 将指定字段的唯一值添加到数组中
    ADD_TO_SET("addToSet"),

    // 计算指定字段非空值的个数
    COUNT("count");

    private final String operator;

}
