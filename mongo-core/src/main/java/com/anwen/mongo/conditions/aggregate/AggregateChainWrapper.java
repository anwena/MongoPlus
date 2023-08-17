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
import com.anwen.mongo.model.BaseGroupAggregate;
import com.anwen.mongo.model.BaseMatchAggregate;
import com.anwen.mongo.support.SFunction;

import java.util.ArrayList;
import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class AggregateChainWrapper<T,Children> implements Aggregate<T,Children>, AccumulatorInterface<T> {

    List<BaseAggregate> baseAggregateList = new ArrayList<>();

    protected final Children typedThis = (Children) this;

    @Override
    public Children match(QueryChainWrapper<T, ?> queryChainWrapper) {
        baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.MATCH.getType(),new BaseMatchAggregate(queryChainWrapper.getCompareList())));
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
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(),new BaseGroupAggregate(accumulator)));
        return typedThis;
    }

    @Override
    public Children group(String _id, Accumulator... accumulator) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(),new BaseGroupAggregate(accumulator)));
        return typedThis;
    }

    @Override
    public Children group(List<Accumulator> accumulatorList) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(),new BaseGroupAggregate(accumulatorList)));
        return typedThis;
    }

    @Override
    public Children group(String resultMappingField, String operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new BaseGroupAggregate(new Accumulator(resultMappingField,operator,field))));
        return typedThis;
    }

    @Override
    public Children group(String resultMappingField, GroupTypeEnum operator, String field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new BaseGroupAggregate(new Accumulator(resultMappingField,operator.getOperator(),field))));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> resultMappingField, String operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new BaseGroupAggregate(new Accumulator(resultMappingField.getFieldNameLine(),operator,field.getFieldNameLine()))));
        return typedThis;
    }

    @Override
    public Children group(SFunction<T, Object> resultMappingField, GroupTypeEnum operator, SFunction<T, Object> field) {
        this.baseAggregateList.add(new BaseAggregate(AggregateTypeEnum.GROUP.getType(), new BaseGroupAggregate(new Accumulator(resultMappingField.getFieldNameLine(),operator.getOperator(),field.getFieldNameLine()))));
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
