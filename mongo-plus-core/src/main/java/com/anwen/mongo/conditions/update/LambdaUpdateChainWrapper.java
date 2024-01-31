package com.anwen.mongo.conditions.update;

import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.execute.ExecutorFactory;

import java.util.ArrayList;
import java.util.List;

public class LambdaUpdateChainWrapper<T> extends UpdateChainWrapper<T,LambdaUpdateChainWrapper<T>> implements ChainUpdate {

    private final ExecutorFactory factory;

    private final Class<T> clazz;

    private final String database;

    public LambdaUpdateChainWrapper(ExecutorFactory factory,Class<T> clazz,String database) {
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
    public boolean remove() {
        return factory.getExecute(database).remove(getCompareList(),clazz);
    }

}
