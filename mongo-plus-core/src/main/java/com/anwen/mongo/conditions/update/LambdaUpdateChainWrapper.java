package com.anwen.mongo.conditions.update;

import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.execute.SqlExecute;

import java.util.ArrayList;
import java.util.List;

public class LambdaUpdateChainWrapper<T> extends UpdateChainWrapper<T,LambdaUpdateChainWrapper<T>> implements ChainUpdate {

    private final SqlExecute sqlExecute;

    public LambdaUpdateChainWrapper(SqlExecute sqlExecute) {
        this.sqlExecute = sqlExecute;
    }

    @Override
    public boolean update(){
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(getCompareList());
        compareConditionList.addAll(getUpdateCompareList());
        return sqlExecute.doUpdate(compareConditionList);
    }

    @Override
    public boolean remove() {
        return sqlExecute.doRemove(getCompareList());
    }

}
