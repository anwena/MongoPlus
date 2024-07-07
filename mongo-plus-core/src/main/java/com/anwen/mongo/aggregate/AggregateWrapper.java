package com.anwen.mongo.aggregate;

import com.anwen.mongo.handlers.condition.AbstractConditionHandler;

/**
 * 便捷的创建管道构造器
 * @author anwen
 * @date 2024/6/25 下午8:29
 */
public class AggregateWrapper extends LambdaAggregateWrapper<AggregateWrapper> {

    public AggregateWrapper(AbstractConditionHandler conditionHandler) {
        super(conditionHandler);
    }

    public AggregateWrapper() {
    }
}
