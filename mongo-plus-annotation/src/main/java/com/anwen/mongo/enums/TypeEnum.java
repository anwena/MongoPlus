package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-18 00:04
 **/
@Getter
@AllArgsConstructor
public enum TypeEnum {

    // 双精度浮点数
    DOUBLE(1),
    // 字符串
    STRING(2),
    // 内嵌文档
    OBJECT(3),
    // 数组
    ARRAY(4),
    // 二进制数据
    BINARY_DATA(5),
    // 已废弃的类型
    UNDEFINED(6),
    // ObjectId
    OBJECT_ID(7),
    // 布尔值
    BOOLEAN(8),
    // 日期时间
    DATE(9),
    // 空值
    NULL(10),
    // 正则表达式
    REGEX(11),
    // JavaScript代码
    JAVASCRIPT_CODE(12),
    // 符号
    SYMBOL(13),
    // 带作用域的JavaScript代码
    JAVASCRIPT_CODE_WITH_SCOPE(14),
    // 32位整数
    INT32(15),
    // 时间戳
    TIMESTAMP(16),
    // 64位整数
    INT64(17),
    // 最小键
    MIN_KEY(18),
    // 最大键
    MAX_KEY(19);

    private final int typeCode;

}
