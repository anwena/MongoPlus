package com.anwen.mongo.conditions.query;

import com.anwen.mongo.conditions.AbstractChainWrapper;
import com.anwen.mongo.toolkit.ChainWrappers;

/**
 * @author JiaChaoYang
 **/
public class Wrapper<T> extends AbstractChainWrapper<T,Wrapper<T>> {
    
    /**
     * 链式调用
     * @author JiaChaoYang
     * @date 2023/8/12 2:14
    */ 
    public QueryChainWrapper<T, Wrapper<T>> lambdaQuery(){
        return ChainWrappers.lambdaQueryChain();
    }
}
