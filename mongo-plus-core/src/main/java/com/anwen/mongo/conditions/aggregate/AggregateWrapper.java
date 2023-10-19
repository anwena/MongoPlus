package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.toolkit.ChainWrappers;

/**
 * 管道条件构造
 *
 * @author JiaChaoYang
 **/
public class AggregateWrapper<T> extends AggregateChainWrapper<T, AggregateWrapper<T>> {

    /**
     * 链式调用
     * @author JiaChaoYang
     * @date 2023/8/12 2:14
     */
    public AggregateChainWrapper<T, AggregateWrapper<T>> lambdaAggregate(){
        return ChainWrappers.lambdaAggregateChain();
    }

}
