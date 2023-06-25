package com.anwen.mongo.sql.interfaces;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 构建条件对象
 * @author JiaChaoYang
 * @since 2023/2/14 14:13
*/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CompareCondition {

    /**
     * 条件
     * @since 2023/2/10 10:16
     */
    private String condition;

    /**
     * 字段
     * @since 2023/2/10 10:16
    */
    private String column;

    /**
     * 值
     * @since 2023/2/10 10:16
    */
    private Object value;

    /**
     * 类型 0查询，1修改
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:49
    */
    private Integer type;

}
