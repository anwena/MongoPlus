package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * group条件构造实现
 *
 * @author JiaChaoYang
 **/
public class ConcretePipelineGroup implements PipelineStrategy {

    private final List<Accumulator> accumulatorList;

    private final String _id;

    public ConcretePipelineGroup(String _id,Accumulator accumulator) {
        this._id = _id;
        this.accumulatorList = Collections.singletonList(accumulator);
    }

    public ConcretePipelineGroup(String _id,Accumulator... accumulators) {
        this._id = _id;
        this.accumulatorList = new ArrayList<>(Arrays.asList(accumulators));
    }

    public ConcretePipelineGroup(String _id , List<Accumulator> accumulatorList) {
        this._id = _id;
        this.accumulatorList = accumulatorList;
    }

    public ConcretePipelineGroup(String _id, String resultMappingField, String operator, String field) {
        this._id = _id;
        this.accumulatorList = Collections.singletonList(new Accumulator(resultMappingField, operator, field));
    }

    @Override
    public BasicDBObject buildAggregate() {
        BasicDBObject basicDBObject = BuildCondition.buildGroup(accumulatorList);
        basicDBObject.put(SqlOperationConstant._ID,_id);
        return basicDBObject;
    }
}
