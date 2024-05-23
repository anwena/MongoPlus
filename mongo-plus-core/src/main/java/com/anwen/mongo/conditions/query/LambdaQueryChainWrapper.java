package com.anwen.mongo.conditions.query;

import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

import java.util.List;

/**
 * 查询实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:11
*/
public class LambdaQueryChainWrapper<T> extends QueryChainWrapper<T,LambdaQueryChainWrapper<T>> implements ChainQuery<T> {

    private final BaseMapper baseMapper;

    private final Class<T> clazz;

    public LambdaQueryChainWrapper(BaseMapper baseMapper, Class<T> clazz){
        this.baseMapper = baseMapper;
        this.clazz = clazz;
    }

    @Override
    public List<T> list() {
        return baseMapper.list(this,clazz);
    }

    @Override
    public T one() {
        return baseMapper.one(this,clazz);
    }

    @Override
    @Deprecated
    public T limitOne() {
        return baseMapper.limitOne(this,clazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return baseMapper.page(this,pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return baseMapper.page(this,pageNum,pageSize,clazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam, Integer recentPageNum) {
        return baseMapper.page(this, pageParam.getPageNum(), pageParam.getPageSize(), recentPageNum, clazz);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize, Integer recentPageNum) {
        return baseMapper.page(this, pageNum, pageSize, recentPageNum, clazz);
    }

    @Override
    public long count() {
        return baseMapper.count(this,clazz);
    }

}
