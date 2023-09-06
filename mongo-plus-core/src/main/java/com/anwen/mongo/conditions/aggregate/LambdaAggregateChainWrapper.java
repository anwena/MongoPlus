package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.execute.SqlExecute;

import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainWrapper<T> extends AggregateChainWrapper<T,LambdaAggregateChainWrapper<T>> implements ChainAggregate<T> {

    private final SqlExecute sqlExecute;

    private final Class<T> clazz;

    public LambdaAggregateChainWrapper(SqlExecute sqlExecute,Class<T> clazz) {
        this.sqlExecute = sqlExecute;
        this.clazz = clazz;
    }

    @Override
    public List<T> list() {
        return sqlExecute.doAggregateList(super.baseAggregateList,super.getBasicDBObjectList(),super.getOptionsBasicDBObject(),clazz);
    }
}
