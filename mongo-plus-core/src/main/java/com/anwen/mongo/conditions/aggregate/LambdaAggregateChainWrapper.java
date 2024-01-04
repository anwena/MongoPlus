package com.anwen.mongo.conditions.aggregate;

import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.execute.SqlExecute;
import com.mongodb.client.ClientSession;

import java.util.List;

/**
 * @author JiaChaoYang
 **/
public class LambdaAggregateChainWrapper<T> extends AggregateChainWrapper<T,LambdaAggregateChainWrapper<T>> implements ChainAggregate<T> {

    private final SqlExecute sqlExecute;

    private final ExecutorFactory factory;
    
    private final String dataSourceName;

    private final Class<T> clazz;

    public LambdaAggregateChainWrapper(SqlExecute sqlExecute,ExecutorFactory factory,Class<T> clazz,String dataSourceName) {
        this.sqlExecute = sqlExecute;
        this.factory = factory;
        this.clazz = clazz;
        this.dataSourceName = dataSourceName;
    }

    @Override
    public List<T> list() {
        return factory.getExecute(dataSourceName).aggregateList(super.baseAggregateList,super.getBasicDBObjectList(),super.getOptionsBasicDBObject(),clazz);
    }

    @Override
    public List<T> list(ClientSession clientSession) {
        return sqlExecute.doAggregateList(clientSession,super.baseAggregateList,super.getBasicDBObjectList(),super.getOptionsBasicDBObject(),clazz);
    }
}
