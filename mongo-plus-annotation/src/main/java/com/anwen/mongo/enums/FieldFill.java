package com.anwen.mongo.enums;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 字段填充策略枚举类
 * @date 2023-11-21 11:14
 * @since quote from: MyBatisPlus
 **/
public enum FieldFill {
    /**
     * 默认不处理
     */
    DEFAULT,
    /**
     * 插入时填充字段
     */
    INSERT,
    /**
     * 更新时填充字段
     */
    UPDATE,
    /**
     * 插入和更新时填充字段
     */
    INSERT_UPDATE
}
