package com.anwen.mongo.enums;

/**
 * 偏好设置节点选择策略
 *
 * @author loser
 * @date 2024/5/8
 */
public enum ReadPreferenceEnum {

    /**
     * 只选择主节点
     */
    PRIMARY,

    /**
     * 优先选择主节点，如果不可用则选择从节点
     */
    PRIMARY_PREFERRED,

    /**
     * 只选择从节点
     */
    SECONDARY,

    /**
     * 优先选择从节点，如果从节点不可用则选择主节点
     */
    SECONDARY_PREFERRED,

    /**
     * 选择最近的节点（ping mongodb 服务器哪个最近就用哪个）
     */
    NEAREST
}
