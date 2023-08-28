package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * let操作符
 *
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Let {

    /**
     * 变量
     * @author JiaChaoYang
     * @date 2023/8/21 22:09
    */
    private String variable;

    /**
     * 值
     * @author JiaChaoYang
     * @date 2023/8/21 22:09
    */
    private String value;

}
