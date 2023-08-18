package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.conditions.accumulator.Accumulator;
import com.anwen.mongo.conditions.accumulator.AccumulatorInterface;
import com.anwen.mongo.conditions.interfaces.aggregate.Aggregate;
import com.anwen.mongo.conditions.interfaces.aggregate.project.Projection;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.enums.AggregateTypeEnum;
import com.anwen.mongo.enums.GroupTypeEnum;
import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.strategy.aggregate.impl.ConcretePipelineGroup;
import com.anwen.mongo.strategy.aggregate.impl.ConcretePipelineMatch;
import com.anwen.mongo.support.SFunction;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class AggregateChainWrapper<T,Children> implements Aggregate<T,Children>, AccumulatorInterface<T> {

    @Getter
    List<BaseAggregate> baseAggregateList = new ArrayList<>();

    protected final Children typedThis = (Children) this;

    @Override
    public Children match(QueryChainWrapper<T, ?> queryChainWrapper) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.MATCH.getType(),new ConcretePipelineMatch(queryChainWrapper.getCompareList())));
        return typedThis;
    }

    @Override
    public Children project(Projection... projection) {
        return null;
    }

    @Override
    public Children sort(Order... orders) {
        return null;
    }

    @Override
    public Children limit(long l) {
        return null;
    }

    @Override
    public Children skip(long s) {
        return null;
    }

    @Override
    public Children group(SFunction<T, Object> _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(),new ConcretePipelineGroup(_id.getFieldNameLine(),accumulator)));
        return typedThis;
    }

    @Override
    public Children group(String _id, Accumulator accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id,accumulator)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(),new ConcretePipelineGroup(_id.getFieldNameLine(),accumulator)));
        return typedThis;
    }

    @Override
    public Children group(String _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(),new ConcretePipelineGroup(_id,accumulator)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(),accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(String _id, List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id,accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, String resultMappingField, String operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(),resultMappingField,operator,field)));
        return typedThis;
    }

    @Override
    public Children group(String _id, String resultMappingField, String operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id,resultMappingField,operator,field)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, String resultMappingField, GroupTypeEnum operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(),resultMappingField,operator.getOperator(),field)));
        return typedThis;
    }

    @Override
    public Children group(String _id, String resultMappingField, GroupTypeEnum operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id,resultMappingField,operator.getOperator(),field)));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(),resultMappingField.getFieldNameLine(),operator,field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(String _id, SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id,resultMappingField.getFieldNameLine(),operator,field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(String _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id,resultMappingField.getFieldNameLine(),operator.getOperator(),field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> _id, SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new ConcretePipelineGroup(_id.getFieldNameLine(),resultMappingField.getFieldNameLine(),operator.getOperator(),field.getFieldNameLine())));
        return typedThis;
    }

    @Override
    public Children lookup() {
        return null;
    }

    @Override
    public Children addFields() {
        return null;
    }

    @Override
    public Children out() {
        return null;
    }
}
