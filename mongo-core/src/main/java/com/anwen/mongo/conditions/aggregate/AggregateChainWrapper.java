package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.conditions.interfaces.aggregate.Aggregate;
import com.anwen.mongo.conditions.interfaces.aggregate.project.Projection;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.enums.AggregateTypeEnum;
import com.anwen.mongo.model.BaseAggregate;
import com.anwen.mongo.model.BaseMatchAggregate;
import com.anwen.mongo.model.Pipeline;

import java.util.ArrayList;
import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class AggregateChainWrapper<T,Children> implements Aggregate<T,Children> {

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
    public Children group() {
        return null;
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
