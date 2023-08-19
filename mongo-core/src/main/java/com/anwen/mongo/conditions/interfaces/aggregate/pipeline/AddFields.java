package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * addFields函数类
 *
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AddFields {

    /**
     * 结果映射字段
     * @author JiaChaoYang
     * @date 2023/8/20 0:12
    */
    private String resultMappingField;

    /**
     * 字段值、取值字段
     * @author JiaChaoYang
     * @date 2023/8/20 0:12
    */
    private String field;

}
