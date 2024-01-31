package com.anwen.mongo.conditions.inject.aggregate;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.execute.ExecutorFactory;

import java.util.List;
import java.util.Map;

import static com.anwen.mongo.toolkit.StringPool.EMPTY;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainInjectWrapper extends AggregateChainWrapper<Map<String,Object>, LambdaAggregateChainInjectWrapper> implements ChainInjectAggregate {

    private final ExecutorFactory factory;

    public LambdaAggregateChainInjectWrapper(ExecutorFactory factory) {
        this.factory = factory;
    }

    @Override
    public <E> List<E> list(String collectionName,Class<E> clazz) {
        return list(EMPTY,collectionName,clazz);
    }

    @Override
    public <E> List<E> list(String database, String collectionName, Class<E> clazz) {
        return factory.getInjectExecute(database).aggregateList(collectionName,super.getBaseAggregateList(),super.getBasicDBObjectList(),super.getOptionsBasicDBObject(),clazz);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName) {
        return list(EMPTY,collectionName);
    }

    @Override
    public List<Map<String, Object>> list(String database, String collectionName) {
        return factory.getInjectExecute(database).aggregateList(collectionName,super.getBaseAggregateList(),super.getBasicDBObjectList(),super.getOptionsBasicDBObject());
    }
}
