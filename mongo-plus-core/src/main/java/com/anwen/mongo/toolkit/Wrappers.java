package com.anwen.mongo.toolkit;

import com.anwen.mongo.aggregate.AggregateWrapper;
import com.anwen.mongo.conditions.query.QueryWrapper;
import com.anwen.mongo.conditions.update.UpdateWrapper;

/**
 * 快捷获取条件构造器
 *
 * @author anwen
 * @date 2024/6/26 下午1:39
 */
public class Wrappers {

    /**
     * 获取条件构造器
     * @author anwen
     * @date 2024/6/26 下午1:41
     */
    public static <T> QueryWrapper<T> lambdaQuery(){
        return new QueryWrapper<>();
    }

    /**
     * 获取修改条件构造器
     * @author anwen
     * @date 2024/6/26 下午1:42
     */
    public static <T> UpdateWrapper<T> lambdaUpdate(){
        return new UpdateWrapper<>();
    }

    /**
     * 获取聚合条件构造器
     * @author anwen
     * @date 2024/6/26 下午1:42
     */
    public static AggregateWrapper lambdaAggregate(){
        return new AggregateWrapper();
    }

}
