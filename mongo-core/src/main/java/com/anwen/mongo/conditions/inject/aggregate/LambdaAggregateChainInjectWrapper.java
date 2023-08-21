package com.anwen.mongo.conditions.inject.aggregate;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.execute.SqlOperation;

import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainInjectWrapper extends AggregateChainWrapper<Map<String,Object>, LambdaAggregateChainInjectWrapper> implements ChainInjectAggregate {

    private final SqlOperation<Map<String,Object>> sqlOperation;

    public LambdaAggregateChainInjectWrapper(SqlOperation<Map<String,Object>> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    @Override
    public <E> List<E> list(String collectionName,Class<E> clazz) {
        return sqlOperation.doAggregateList(collectionName,super.getBaseAggregateList(),super.getBasicDBObjectList(),clazz);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName) {
        return sqlOperation.doAggregateList(collectionName,super.getBaseAggregateList(),super.getBasicDBObjectList());
    }
}
