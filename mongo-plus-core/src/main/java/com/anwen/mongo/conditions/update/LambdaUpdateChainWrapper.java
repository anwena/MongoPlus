package com.anwen.mongo.conditions.update;

import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.execute.SqlExecute;
import com.mongodb.client.ClientSession;

import java.util.ArrayList;
import java.util.List;

public class LambdaUpdateChainWrapper<T> extends UpdateChainWrapper<T,LambdaUpdateChainWrapper<T>> implements ChainUpdate {

    private final SqlExecute sqlExecute;

    private final ExecutorFactory factory;

    private final Class<T> clazz;

    private final String database;

    public LambdaUpdateChainWrapper(SqlExecute sqlExecute,ExecutorFactory factory,Class<T> clazz,String database) {
        this.sqlExecute = sqlExecute;
        this.factory = factory;
        this.clazz = clazz;
        this.database = database;
    }

    @Override
    public boolean update(){
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(getCompareList());
        compareConditionList.addAll(getUpdateCompareList());
        return factory.getExecute(database).update(compareConditionList,clazz);
    }

    @Override
    public boolean update(ClientSession clientSession) {
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(getCompareList());
        compareConditionList.addAll(getUpdateCompareList());
        return sqlExecute.doUpdate(clientSession,compareConditionList,clazz);
    }

    @Override
    public boolean remove() {
        return factory.getExecute(database).remove(getCompareList(),clazz);
    }

    @Override
    public boolean remove(ClientSession clientSession) {
        return sqlExecute.doRemove(clientSession,getCompareList(),clazz);
    }

}
