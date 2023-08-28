package com.anwen.mongo.conditions.accumulator;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 累加器结果集
 *
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Accumulator {

    /**
     * 结果映射字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:22
    */
    private String resultMappingField;

    /**
     * 条件
     * @author JiaChaoYang
     * @date 2023/8/17 20:11
    */
    private String condition;

    /**
     * 字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:11
    */
    private String field;

    public Accumulator(String condition, String field) {
        this.condition = condition;
        this.field = field;
        this.resultMappingField = field;
    }
}
