package com.anwen.mongo.conditions.interfaces.condition;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author JiaChaoYang
 * 排序配置
 * @since 2023-02-10 11:57
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Order {

    /**
     * 排序类型 1升序  -1降序
     * @since 2023/2/10 11:59
    */
    private Integer type;

    /**
     * 字段名
     * @since 2023/2/10 11:59
    */
    private String column;

}
