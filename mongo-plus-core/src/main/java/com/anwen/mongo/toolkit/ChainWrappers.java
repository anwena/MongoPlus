package com.anwen.mongo.toolkit;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.aggregate.AggregateWrapper;
import com.anwen.mongo.conditions.aggregate.LambdaAggregateChainWrapper;
import com.anwen.mongo.conditions.inject.aggregate.InjectAggregateWrapper;
import com.anwen.mongo.conditions.inject.query.InjectQueryWrapper;
import com.anwen.mongo.conditions.inject.update.InjectUpdateWrapper;
import com.anwen.mongo.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.conditions.query.QueryWrapper;
import com.anwen.mongo.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.conditions.update.UpdateChainWrapper;
import com.anwen.mongo.conditions.update.UpdateWrapper;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.execute.SqlExecute;

import java.util.Map;

/**
 * 快速构建链式调用
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:27
*/ 
public final class ChainWrappers {

    public static <T> LambdaQueryChainWrapper<T> lambdaQueryChain(SqlExecute sqlExecute, ExecutorFactory factory, Class<T> clazz,String dataSourceName){
        return new LambdaQueryChainWrapper<>(sqlExecute,factory, clazz,dataSourceName);
    }

    public static <T> LambdaAggregateChainWrapper<T> lambdaAggregateChain(SqlExecute sqlExecute,ExecutorFactory factory,Class<T> clazz,String dataSourceName){
        return new LambdaAggregateChainWrapper<>(sqlExecute,factory,clazz,dataSourceName);
    }

    public static <T> LambdaUpdateChainWrapper<T> lambdaUpdateChain(SqlExecute sqlExecute,ExecutorFactory factory,Class<T> clazz,String dataSourceName){
        return new LambdaUpdateChainWrapper<>(sqlExecute,factory,clazz,dataSourceName);
    }

    public static <T> UpdateChainWrapper<T, UpdateWrapper<T>> lambdaUpdateChain(){
        return new UpdateChainWrapper<>();
    }

    public static <T> UpdateChainWrapper<Map<String,Object>, InjectUpdateWrapper> lambdaUpdateChainInject(){
        return new UpdateChainWrapper<>();
    }

    public static <T> QueryChainWrapper<T, QueryWrapper<T>> lambdaQueryChain(){
        return new QueryChainWrapper<>();
    }

    public static QueryChainWrapper<Map<String,Object>, InjectQueryWrapper> lambdaQueryChainInject(){
        return new QueryChainWrapper<>();
    }

    public static <T> AggregateChainWrapper<T, AggregateWrapper<T>> lambdaAggregateChain(){
        return new AggregateChainWrapper<>();
    }

    public static AggregateChainWrapper<Map<String,Object>, InjectAggregateWrapper> lambdaAggregateChainInject(){
        return new AggregateChainWrapper<>();
    }

}
