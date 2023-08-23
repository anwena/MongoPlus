package com.anwen.mongo.conditions.inject.aggregate;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.toolkit.ChainWrappers;

import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class InjectAggregateWrapper extends AggregateChainWrapper<Map<String,Object>, InjectAggregateWrapper> {

    /**
     * 链式调用
     * @author JiaChaoYang
     * @date 2023/8/23 21:09
    */
    public AggregateChainWrapper<Map<String,Object>, InjectAggregateWrapper> lambdaQuery(){
        return ChainWrappers.lambdaAggregateChainInject();
    }

}
