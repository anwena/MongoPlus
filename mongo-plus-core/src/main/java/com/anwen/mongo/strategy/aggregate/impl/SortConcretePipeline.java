package com.anwen.mongo.strategy.aggregate.impl;

import com.anwen.mongo.conditions.MongoPlusBasicDBObject;
import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.strategy.aggregate.PipelineStrategy;
import com.mongodb.BasicDBObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * sort策略实现类
 *
 * @author JiaChaoYang
 **/
public class SortConcretePipeline implements PipelineStrategy {

    private final List<Order> orderList;

    public SortConcretePipeline(Order... orders) {
        this.orderList = new ArrayList<>(Arrays.asList(orders));
    }

    public SortConcretePipeline(List<Order> orderList) {
        this.orderList = orderList;
    }

    @Override
    public BasicDBObject buildAggregate() {
        return new MongoPlusBasicDBObject(){{
            orderList.forEach(order -> put(order.getColumn(),order.getType()));
        }};
    }
}
