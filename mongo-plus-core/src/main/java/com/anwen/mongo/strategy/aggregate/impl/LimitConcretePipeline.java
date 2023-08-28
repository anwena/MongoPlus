package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.enums.AggregateTypeEnum;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

/**
 * limit策略实现类
 *
 * @author JiaChaoYang
 **/
public class LimitConcretePipeline implements PipelineStrategy {

    private final Long limit;

    public LimitConcretePipeline(Long limit) {
        this.limit = limit;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return new BasicDBObject(AggregateTypeEnum.LIMIT.getType(),limit);
    }
}
