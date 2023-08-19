package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.execute.SqlOperation;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainWrapper<T> extends AggregateChainWrapper<T,LambdaAggregateChainWrapper<T>> implements ChainAggregate<T> {

    private final SqlOperation<T> sqlOperation;

    public LambdaAggregateChainWrapper(Class<T> clazz , SqlOperation<T> sqlOperation) {
        this.sqlOperation = sqlOperation;
        T tClass;
        try {
            tClass = clazz.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
        sqlOperation.init(tClass.getClass());
    }

    @Override
    public List<T> list() {
        return sqlOperation.doAggregateList(super.baseAggregateList);
    }

    @Override
    public <E> List<E> list(Class<E> clazz) {
        return sqlOperation.doAggregateList(super.baseAggregateList,clazz);
    }

    @Override
    public T one() {
        return null;
    }

    @Override
    public T limitOne() {
        return null;
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return null;
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return null;
    }

    @Override
    public long count() {
        return 0;
    }
}
