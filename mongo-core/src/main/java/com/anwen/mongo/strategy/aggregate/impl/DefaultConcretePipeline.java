package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

/**
 * 默认实现策略
 *
 * @author JiaChaoYang
 **/
public class DefaultConcretePipeline implements PipelineStrategy {

    private final BasicDBObject basicDBObject;

    public DefaultConcretePipeline(BasicDBObject basicDBObject) {
        this.basicDBObject = basicDBObject;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return basicDBObject;
    }
}
