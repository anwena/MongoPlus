package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.interfaces.aggregate.pipeline.AddFields;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * addField策略实现类
 *
 * @author JiaChaoYang
 **/
public class ConcretePipelineAddFields implements PipelineStrategy {

    private final List<AddFields> addFieldsList;

    public ConcretePipelineAddFields(AddFields... addFields) {
        this.addFieldsList = new ArrayList<>(Arrays.asList(addFields));
    }

    public ConcretePipelineAddFields(List<AddFields> addFieldsList) {
        this.addFieldsList = addFieldsList;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return BuildCondition.buildAddFields(addFieldsList);
    }
}
