package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.enums.AggregateTypeEnum;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

/**
 * skip策略实现类
 *
 * @author JiaChaoYang
 **/
public class SkipConcretePipeline implements PipelineStrategy {

    private final Long skip;

    public SkipConcretePipeline(Long skip) {
        this.skip = skip;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return new BasicDBObject(AggregateTypeEnum.SKIP.getType(),skip);
    }
}
