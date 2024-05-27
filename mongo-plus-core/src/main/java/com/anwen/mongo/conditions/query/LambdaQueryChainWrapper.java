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
        return list(clazz);
    }

    @Override
    public <R> List<R> list(Class<R> rClazz) {
        return baseMapper.list(this,clazz,rClazz);
    }

    @Override
    public T one() {
        return one(clazz);
    }

    @Override
    public <R> R one(Class<R> rClazz) {
        return baseMapper.one(this,clazz,rClazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return page(pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public <R> PageResult<R> page(PageParam pageParam, Class<R> rClazz) {
        return baseMapper.page(this,pageParam.getPageNum(),pageParam.getPageSize(),clazz,rClazz);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return page(pageNum,pageSize,clazz);
    }

    @Override
    public <R> PageResult<R> page(Integer pageNum, Integer pageSize, Class<R> rClazz) {
        return baseMapper.page(this, pageNum, pageSize, clazz, rClazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam, Integer recentPageNum) {
        return page(pageParam.getPageNum(), pageParam.getPageSize(), recentPageNum, clazz);
    }

    @Override
    public <R> PageResult<R> page(PageParam pageParam, Integer recentPageNum, Class<R> rClazz) {
        return baseMapper.page(this, pageParam.getPageNum(), pageParam.getPageSize(), recentPageNum, clazz, rClazz);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize, Integer recentPageNum) {
        return page(pageNum, pageSize, recentPageNum, clazz);
    }

    @Override
    public <R> PageResult<R> page(Integer pageNum, Integer pageSize, Integer recentPageNum, Class<R> rClazz) {
        return baseMapper.page(this, pageNum, pageSize, recentPageNum, clazz, rClazz);
    }

    @Override
    public long count() {
        return baseMapper.count(this,clazz);
    }

}
