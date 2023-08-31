package com.anwen.mongo.conditions.inject.aggregate;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.execute.SqlExecute;

import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainInjectWrapper extends AggregateChainWrapper<Map<String,Object>, LambdaAggregateChainInjectWrapper> implements ChainInjectAggregate {

    private final SqlExecute sqlExecute;

    public LambdaAggregateChainInjectWrapper(SqlExecute sqlExecute) {
        this.sqlExecute = sqlExecute;
    }

    @Override
    public <E> List<E> list(String collectionName,Class<E> clazz) {
        return sqlExecute.doAggregateList(collectionName,super.getBaseAggregateList(),super.getBasicDBObjectList(),super.getOptionsBasicDBObject(),clazz);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName) {
        return sqlExecute.doAggregateList(collectionName,super.getBaseAggregateList(),super.getBasicDBObjectList(),super.getOptionsBasicDBObject());
    }
}
