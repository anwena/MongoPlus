package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.execute.ExecutorFactory;

import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainWrapper<T> extends AggregateChainWrapper<T,LambdaAggregateChainWrapper<T>> implements ChainAggregate<T> {

    private final ExecutorFactory factory;

    private final String database;

    private final Class<T> clazz;

    public LambdaAggregateChainWrapper(ExecutorFactory factory,Class<T> clazz,String database) {
        this.factory = factory;
        this.clazz = clazz;
        this.database = database;
    }

    @Override
    public List<T> list() {
        return factory.getExecute(database).aggregateList(this,clazz);
    }
}
