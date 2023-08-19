package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

import java.util.List;

/**
 * match策略实现类
 * @author JiaChaoYang
 **/
public class ConcretePipelineMatch implements PipelineStrategy {

    private final List<CompareCondition> compareList;

    public ConcretePipelineMatch(List<CompareCondition> compareList) {
        this.compareList = compareList;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return BuildCondition.buildQueryCondition(compareList);
    }
}
