package com.anwen.mongo.conditions.inject.query;

import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.toolkit.ChainWrappers;

import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class InjectQueryWrapper extends QueryChainWrapper<Map<String,Object>, InjectQueryWrapper> {

    /**
     * 链式调用
     * @author JiaChaoYang
     * @date 2023/8/12 2:14
     */
    public QueryChainWrapper<Map<String,Object>, InjectQueryWrapper> lambdaQuery(){
        return ChainWrappers.lambdaQueryChainInject();
    }
}
