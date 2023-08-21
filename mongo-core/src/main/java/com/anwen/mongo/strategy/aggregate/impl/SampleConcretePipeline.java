package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

/**
 * sample策略实现类
 *
 * @author JiaChaoYang
 **/
public class SampleConcretePipeline implements PipelineStrategy {

    private final Long size;

    public SampleConcretePipeline(Long size) {
        this.size = size;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return BuildCondition.buildSample(size);
    }
}
