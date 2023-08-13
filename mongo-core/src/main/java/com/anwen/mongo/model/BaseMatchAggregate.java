package com.anwen.mongo.model;

import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Condition;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.nio.channels.Pipe;
import java.util.List;

/**
 * @author JiaChaoYang
 **/
@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BaseMatchAggregate extends Pipeline {

    /**
     * 条件集合
     * @author JiaChaoYang
     * @date 2023/8/13 18:41
    */
    private List<CompareCondition> compareConditionList;

}
