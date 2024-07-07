package com.anwen.mongo.enums;

/**
 * 脱敏类型枚举
 * @author anwen
 * @date 2024/6/28 下午5:06
 */
public enum DesensitizationTypeEnum {

    /**
     * 自定义
     * @date 2024/6/28 下午3:53
     */
    CUSTOM,

    /**
     * 用户id
     * @date 2024/6/28 下午3:54
     */
    USER_ID,

    /**
     * 中文名
     * @date 2024/6/28 下午3:54
     */
    CHINESE_NAME,

    /**
     * 身份证号
     * @date 2024/6/28 下午3:54
     */
    ID_CARD,

    /**
     * 座机号
     * @date 2024/6/28 下午3:54
     */
    FIXED_PHONE,

    /**
     * 手机号
     * @date 2024/6/28 下午3:54
     */
    MOBILE_PHONE,

    /**
     * 地址
     * @date 2024/6/28 下午3:55
     */
    ADDRESS,

    /**
     * 电子邮件
     * @date 2024/6/28 下午3:55
     */
    EMAIL,

    /**
     * 密码
     * @date 2024/6/28 下午3:55
     */
    PASSWORD,

    /**
     * 中国大陆车牌，包含普通车辆、新能源车辆
     * @date 2024/6/28 下午3:55
     */
    CAR_LICENSE,

    /**
     * 银行卡
     * @date 2024/6/28 下午3:55
     */
    BANK_CARD,

    /**
     * IPv4地址
     * @date 2024/6/28 下午4:21
     */
    IPV4,

    /**
     * IPv6地址
     * @date 2024/6/28 下午4:21
     */
    IPV6,

    /**
     * 定义了一个first_mask的规则，只显示第一个字符。
     * @date 2024/6/28 下午4:21
     */
    FIRST_MASK,

    /**
     * 清空为null
     * @date 2024/6/28 下午4:21
     */
    CLEAR_TO_NULL,

    /**
     * 清空为""
     * @date 2024/6/28 下午4:21
     */
    CLEAR_TO_EMPTY,

    ;

}
