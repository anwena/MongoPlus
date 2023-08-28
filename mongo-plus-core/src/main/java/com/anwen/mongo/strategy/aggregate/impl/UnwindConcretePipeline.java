package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

/**
 * unwind策略实现类
 *
 * @author JiaChaoYang
 **/
public class UnwindConcretePipeline implements PipelineStrategy {

    private final String field;

    private final Boolean preserveNullAndEmptyArrays;

    public UnwindConcretePipeline(String field, Boolean preserveNullAndEmptyArrays) {
        this.field = field;
        this.preserveNullAndEmptyArrays = preserveNullAndEmptyArrays;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return BuildCondition.buildUnwind(preserveNullAndEmptyArrays,field);
    }
}
