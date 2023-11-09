package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.anwen.mongo.toolkit.CollUtil;
import com.mongodb.BasicDBObject;

import java.util.List;

/**
 * match策略实现类
 * @author JiaChaoYang
 **/
public class MatchConcretePipeline implements PipelineStrategy {

    private final List<CompareCondition> compareList;

    private List<BasicDBObject> basicDBObjectList;

    public MatchConcretePipeline(List<CompareCondition> compareList) {
        this.compareList = compareList;
    }

    public MatchConcretePipeline(List<CompareCondition> compareList, List<BasicDBObject> basicDBObjectList) {
        this.compareList = compareList;
        this.basicDBObjectList = basicDBObjectList;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return BuildCondition.buildQueryCondition(compareList);
    }
}
