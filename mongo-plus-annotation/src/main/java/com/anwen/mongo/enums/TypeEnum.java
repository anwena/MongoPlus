package com.anwen.mongo.enums;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-18 00:04
 **/
public enum TypeEnum {

    //不是真正的 BSON 类型。用于表示文档结束
    END_OF_DOCUMENT(0x00),
    // 双精度浮点数
    DOUBLE(0x01),
    // 字符串
    STRING(0x02),
    // 内嵌文档
    OBJECT(0x03),
    // 数组
    ARRAY(0x04),
    // 二进制数据
    BINARY_DATA(0x05),
    // 已废弃的类型
    UNDEFINED(0x06),
    // ObjectId
    OBJECT_ID(0x07),
    // 布尔值
    BOOLEAN(0x08),
    // 日期时间
    DATE(0x09),
    // 空值
    NULL(0x0a),
    // 正则表达式
    REGEX(0x0b),
    // JavaScript代码
    JAVASCRIPT_CODE(0x0d),
    // 符号
    SYMBOL(0x0e),
    // 带作用域的JavaScript代码
    JAVASCRIPT_CODE_WITH_SCOPE(0x0f),
    // 32位整数
    INT32(0x10),
    // 时间戳
    TIMESTAMP(0x11),
    // 64位整数
    INT64(0x12),
    // 128位浮点数
    DECIMAL128(0x13),
    // 最小键
    MIN_KEY(0xff),
    // 最大键
    MAX_KEY(0x7f);

    private final int typeCode;

    public int getTypeCode() {
        return typeCode;
    }

    TypeEnum(int typeCode) {
        this.typeCode = typeCode;
    }
}
