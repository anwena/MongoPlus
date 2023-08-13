package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.toolkit.ChainWrappers;

/**
 * 管道条件构造
 *
 * @author JiaChaoYang
 **/
public class AggregateWrapper<T> extends QueryChainWrapper<T, AggregateWrapper<T>> {

    /**
     * 链式调用
     * @author JiaChaoYang
     * @date 2023/8/12 2:14
     */
    public QueryChainWrapper<T, AggregateWrapper<T>> lambdaQuery(){
        return ChainWrappers.lambdaAggregateChain();
    }

}
