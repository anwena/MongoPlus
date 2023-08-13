package com.anwen.mongo.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 分组操作
 *
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BaseGroupAggregate extends BasePipeline {
    /**
     * 返回字段名
     * @author JiaChaoYang
     * @date 2023/8/13 23:53
    */
    /**
     * 返回字段名
     * @author JiaChaoYang
     * @date 2023/8/13 23:53
     */
    private String field;

    /**
     * 累计操作符
     * @author JiaChaoYang
     * @date 2023/8/13 23:53
     */
    private String accumulator;

    /**
     * 表达式
     * @author JiaChaoYang
     * @date 2023/8/13 23:54
     */
    private String expression;

}