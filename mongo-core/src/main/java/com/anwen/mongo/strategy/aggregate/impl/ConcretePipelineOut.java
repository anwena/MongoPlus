package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

/**
 * out策略实现类
 *
 * @author JiaChaoYang
 **/
public class ConcretePipelineOut implements PipelineStrategy {

    private final String db;

    private final String coll;

    public ConcretePipelineOut(String db, String coll) {
        this.db = db;
        this.coll = coll;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return BuildCondition.buildOut(db,coll);
    }
}
