package com.anwen.mongo.aggregate;

import com.anwen.mongo.mapper.BaseMapper;

import java.util.List;

/**
 * @author anwen
 * @date 2024/6/19 下午11:37
 */
public class LambdaAggregateChainWrapper<T> extends LambdaAggregateWrapper<LambdaAggregateChainWrapper<T>> implements ChainAggregate<T> {

    private final BaseMapper baseMapper;

    private final Class<T> clazz;

    public LambdaAggregateChainWrapper(BaseMapper baseMapper, Class<T> clazz) {
        this.baseMapper = baseMapper;
        this.clazz = clazz;
    }

    @Override
    public List<T> list() {
        return list(clazz);
    }

    @Override
    public <R> List<R> list(Class<R> rClazz) {
        return baseMapper.aggregateList(this,clazz,rClazz);
    }
}
