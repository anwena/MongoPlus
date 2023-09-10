package com.anwen.mongo.conditions.update;

import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.execute.SqlExecute;
import com.mongodb.client.ClientSession;

import java.util.ArrayList;
import java.util.List;

public class LambdaUpdateChainWrapper<T> extends UpdateChainWrapper<T,LambdaUpdateChainWrapper<T>> implements ChainUpdate {

    private final SqlExecute sqlExecute;

    private final Class<T> clazz;

    public LambdaUpdateChainWrapper(SqlExecute sqlExecute,Class<T> clazz) {
        this.sqlExecute = sqlExecute;
        this.clazz = clazz;
    }

    @Override
    public boolean update(){
        return update(null);
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
        return sqlExecute.doRemove(getCompareList(),clazz);
    }

    @Override
    public boolean remove(ClientSession clientSession) {
        return sqlExecute.doRemove(clientSession,getCompareList(),clazz);
    }

}
