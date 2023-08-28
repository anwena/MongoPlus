package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.execute.SqlOperation;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainWrapper<T> extends AggregateChainWrapper<T,LambdaAggregateChainWrapper<T>> implements ChainAggregate<T> {

    private final SqlOperation sqlOperation;

    public LambdaAggregateChainWrapper(SqlOperation sqlOperation) {
        this.sqlOperation = sqlOperation;
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
