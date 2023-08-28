package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * replaceRoot类
 *
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReplaceRoot {

    /**
     * 结果映射字段
     * @author JiaChaoYang
     * @date 2023/8/20 1:54
    */
    private String resultMappingField;

    /**
     * 指定字段
     * @author JiaChaoYang
     * @date 2023/8/20 1:54
    */
    private String field;

}
