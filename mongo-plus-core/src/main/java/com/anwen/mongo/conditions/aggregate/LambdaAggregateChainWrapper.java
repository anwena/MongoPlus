package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.mapper.BaseMapper;

import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainWrapper<T> extends AggregateChainWrapper<T,LambdaAggregateChainWrapper<T>> implements ChainAggregate<T> {

    private final BaseMapper baseMapper;

    private final Class<T> clazz;

    public LambdaAggregateChainWrapper(BaseMapper baseMapper, Class<T> clazz) {
        this.baseMapper = baseMapper;
        this.clazz = clazz;
    }

    @Override
    public List<T> list() {
        return baseMapper.aggregateList(this,clazz);
    }
}
