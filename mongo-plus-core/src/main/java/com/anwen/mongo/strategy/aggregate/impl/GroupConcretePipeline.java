package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.build.GroupBuilder;
import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.model.GroupField;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
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

    private List<GroupField> _idList;

    public GroupConcretePipeline(String _id, Accumulator accumulator) {
        this._id = _id;
        this.accumulatorList = Collections.singletonList(accumulator);
    }

    public GroupConcretePipeline(List<GroupField> _idList, Accumulator accumulator,boolean multiple) {
        this._idList = _idList;
        this.accumulatorList = Collections.singletonList(accumulator);
    }

    public GroupConcretePipeline(String _id, Accumulator... accumulators) {
        this._id = _id;
        this.accumulatorList = new ArrayList<>(Arrays.asList(accumulators));
    }

    public GroupConcretePipeline(List<GroupField> _idList,boolean multiple,Accumulator... accumulators) {
        this._idList = _idList;
        this.accumulatorList = new ArrayList<>(Arrays.asList(accumulators));
    }

    public GroupConcretePipeline(String _id , List<Accumulator> accumulatorList) {
        this._id = _id;
        this.accumulatorList = accumulatorList;
    }

    public GroupConcretePipeline(List<GroupField> _idList , List<Accumulator> accumulatorList) {
        this._idList = _idList;
        this.accumulatorList = accumulatorList;
    }

    public GroupConcretePipeline(String _id, String resultMappingField, String operator, Object field) {
        this._id = _id;
        this.accumulatorList = Collections.singletonList(new Accumulator(resultMappingField, operator, field));
    }

    public GroupConcretePipeline(List<GroupField> _idList , String resultMappingField, String operator, Object field,boolean multiple) {
        this._idList = _idList;
        this.accumulatorList = Collections.singletonList(new Accumulator(resultMappingField, operator, field));
    }

    public GroupConcretePipeline(String _id){
        this._id = _id;
    }

    public GroupConcretePipeline(List<GroupField> _idList,boolean multiple){
        this._idList = _idList;
    }

    public GroupConcretePipeline(List<Accumulator> _idAccumulator){
        this._idAccumulator = _idAccumulator;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return new GroupBuilder()
                .withAccumulatorList(accumulatorList)
                .withId(_id)
                .withIdList(_idList)
                .withIdAccumulator(_idAccumulator)
                .build();
    }
}
