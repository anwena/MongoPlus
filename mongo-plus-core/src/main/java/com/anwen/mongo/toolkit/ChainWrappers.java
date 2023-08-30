package com.anwen.mongo.toolkit;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.aggregate.AggregateWrapper;
import com.anwen.mongo.conditions.aggregate.LambdaAggregateChainWrapper;
import com.anwen.mongo.conditions.inject.aggregate.InjectAggregateWrapper;
import com.anwen.mongo.conditions.inject.query.InjectWrapper;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.query.Wrapper;
import com.anwen.mongo.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.execute.SqlExecute;

import java.util.Map;

/**
 * 快速构建链式调用
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:27
*/ 
public final class ChainWrappers {

    public static <T> LambdaQueryChainWrapper<T> lambdaQueryChain(SqlExecute sqlExecute){
        return new LambdaQueryChainWrapper<>(sqlExecute);
    }

    public static <T> LambdaAggregateChainWrapper<T> lambdaAggregateChain(SqlExecute sqlExecute){
        return new LambdaAggregateChainWrapper<>(sqlExecute);
    }

    public static <T> LambdaUpdateChainWrapper<T> lambdaUpdateChain(SqlExecute sqlExecute){
        return new LambdaUpdateChainWrapper<>(sqlExecute);
    }

    public static <T> QueryChainWrapper<T, Wrapper<T>> lambdaQueryChain(){
        return new QueryChainWrapper<>();
    }

    public static QueryChainWrapper<Map<String,Object>, InjectWrapper> lambdaQueryChainInject(){
        return new QueryChainWrapper<>();
    }

    public static <T> AggregateChainWrapper<T, AggregateWrapper<T>> lambdaAggregateChain(){
        return new AggregateChainWrapper<>();
    }

    public static AggregateChainWrapper<Map<String,Object>, InjectAggregateWrapper> lambdaAggregateChainInject(){
        return new AggregateChainWrapper<>();
    }

}
