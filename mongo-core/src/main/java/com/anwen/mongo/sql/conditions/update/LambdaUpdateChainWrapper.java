package com.anwen.mongo.sql.conditions.update;

import com.anwen.mongo.sql.SqlOperation;

public class LambdaUpdateChainWrapper<T> extends UpdateChainWrapper<T,LambdaUpdateChainWrapper<T>> implements ChainUpdate<T> {

    private final SqlOperation<T> sqlOperation;

    public LambdaUpdateChainWrapper(SqlOperation<T> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    @Override
    public boolean update(){
        return sqlOperation.doUpdate(getCompareList());
    }

    @Override
    public boolean remove() {
        return sqlOperation.doRemove(getCompareList());
    }

}
