package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

/**
 * lookup策略实现类
 *
 * @author JiaChaoYang
 **/
public class LookupConcretePipeline implements PipelineStrategy {

    private final BasicDBObject basicDBObject;

    public LookupConcretePipeline(BasicDBObject basicDBObject) {
        this.basicDBObject = basicDBObject;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return basicDBObject;
    }
}
