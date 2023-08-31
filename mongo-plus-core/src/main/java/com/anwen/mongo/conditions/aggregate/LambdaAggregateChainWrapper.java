package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.execute.SqlExecute;

import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainWrapper<T> extends AggregateChainWrapper<T,LambdaAggregateChainWrapper<T>> implements ChainAggregate<T> {

    private final SqlExecute sqlExecute;

    public LambdaAggregateChainWrapper(SqlExecute sqlExecute) {
        this.sqlExecute = sqlExecute;
    }

    @Override
    public List<T> list() {
        return sqlExecute.doAggregateList(super.baseAggregateList,super.getBasicDBObjectList(),super.getOptionsBasicDBObject());
    }

    @Override
    public <E> List<E> list(Class<E> clazz) {
        return sqlExecute.doAggregateList(super.baseAggregateList,super.getBasicDBObjectList(),super.getOptionsBasicDBObject(),clazz);
    }
}
