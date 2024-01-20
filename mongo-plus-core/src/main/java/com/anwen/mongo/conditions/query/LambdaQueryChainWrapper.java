package com.anwen.mongo.conditions.query;

import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

import java.util.List;

/**
 * 查询实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:11
*/
public class LambdaQueryChainWrapper<T> extends QueryChainWrapper<T,LambdaQueryChainWrapper<T>> implements ChainQuery<T> {

    private final ExecutorFactory factory;

    private final Class<T> clazz;

    private final String database;

    public LambdaQueryChainWrapper(ExecutorFactory factory, Class<T> clazz,String database){
        this.factory = factory;
        this.clazz = clazz;
        this.database = database;
    }

    @Override
    public List<T> list() {
        return factory.getExecute(database).list(getCompareList(), getOrderList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T one() {
        return factory.getExecute(database).one(getCompareList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T limitOne() {
        return factory.getExecute(database).limitOne(getCompareList(),getProjectionList(),getBasicDBObjectList(),getOrderList(),clazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return factory.getExecute(database).page(getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return factory.getExecute(database).page(getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize,clazz);
    }

    @Override
    public long count() {
        return factory.getExecute(database).count(getCompareList(),clazz);
    }

}
