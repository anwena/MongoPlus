package com.anwen.mongo.conditions.query;

import com.anwen.mongo.toolkit.ChainWrappers;

/**
 * @author JiaChaoYang
 **/
public class QueryWrapper<T> extends QueryChainWrapper<T, QueryWrapper<T>> {
    
    /**
     * 链式调用
     * @author JiaChaoYang
     * @date 2023/8/12 2:14
    */ 
    public QueryChainWrapper<T, QueryWrapper<T>> lambdaQuery(){
        return ChainWrappers.lambdaQueryChain();
    }
}
