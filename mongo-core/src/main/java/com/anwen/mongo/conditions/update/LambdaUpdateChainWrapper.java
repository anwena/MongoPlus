package com.anwen.mongo.conditions.update;

import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.sql.SqlOperation;

import java.util.ArrayList;
import java.util.List;

public class LambdaUpdateChainWrapper<T> extends UpdateChainWrapper<T,LambdaUpdateChainWrapper<T>> implements ChainUpdate {

    private final SqlOperation<T> sqlOperation;

    public LambdaUpdateChainWrapper(SqlOperation<T> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    @Override
    public boolean update(){
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(getCompareList());
        compareConditionList.addAll(getUpdateCompareList());
        return sqlOperation.doUpdate(compareConditionList);
    }

    @Override
    public boolean remove() {
        return sqlOperation.doRemove(getCompareList());
    }

}
