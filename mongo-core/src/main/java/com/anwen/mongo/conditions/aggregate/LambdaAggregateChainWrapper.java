package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.execute.SqlOperation;

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
        return sqlOperation.doAggregateList(super.baseAggregateList,super.getBasicDBObjectList());
    }

    @Override
    public <E> List<E> list(Class<E> clazz) {
        return sqlOperation.doAggregateList(super.baseAggregateList,super.getBasicDBObjectList(),clazz);
    }
}
