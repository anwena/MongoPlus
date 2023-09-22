package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.constant.SqlOperationConstant;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.toolkit.StringUtils;
import com.mongodb.BasicDBObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * group策略实现类
 *
 * @author JiaChaoYang
 **/
public class GroupConcretePipeline implements PipelineStrategy {

    private List<Accumulator> accumulatorList;

    private String _id;

    private List<Accumulator> _idAccumulator;

    public GroupConcretePipeline(String _id, Accumulator accumulator) {
        this._id = _id;
        this.accumulatorList = Collections.singletonList(accumulator);
    }

    public GroupConcretePipeline(String _id, Accumulator... accumulators) {
        this._id = _id;
        this.accumulatorList = new ArrayList<>(Arrays.asList(accumulators));
    }

    public GroupConcretePipeline(String _id , List<Accumulator> accumulatorList) {
        this._id = _id;
        this.accumulatorList = accumulatorList;
    }

    public GroupConcretePipeline(String _id, String resultMappingField, String operator, String field) {
        this._id = _id;
        this.accumulatorList = Collections.singletonList(new Accumulator(resultMappingField, operator, field));
    }

    public GroupConcretePipeline(String _id){
        this._id = _id;
    }

    public GroupConcretePipeline(List<Accumulator> _idAccumulator){
        this._idAccumulator = _idAccumulator;
    }

    @Override
    public BasicDBObject buildAggregate() {
        BasicDBObject basicDBObject = new BasicDBObject();
        if (CollUtil.isNotEmpty(accumulatorList)){
            basicDBObject = BuildCondition.buildGroup(accumulatorList);
        }
        if (StringUtils.isNotBlank(_id)) {
            basicDBObject.put(SqlOperationConstant._ID, "$" + _id);
        }
        if (null != _idAccumulator && !_idAccumulator.isEmpty()){
            basicDBObject.put(SqlOperationConstant._ID,BuildCondition.buildGroup(_idAccumulator));
        }
        return basicDBObject;
    }
}
