package com.anwen.mongo.conditions.update;

import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.mapper.BaseMapper;

public class LambdaUpdateChainWrapper<T> extends UpdateChainWrapper<T,LambdaUpdateChainWrapper<T>> implements ChainUpdate {

    private final BaseMapper baseMapper;

    private final Class<T> clazz;

    public LambdaUpdateChainWrapper(BaseMapper baseMapper, Class<T> clazz) {
        this.baseMapper = baseMapper;
        this.clazz = clazz;
    }

    @Override
    public boolean update(){
        return baseMapper.update(this,clazz);
    }

    @Override
    public boolean remove() {
        return baseMapper.remove(this,clazz);
    }

}
