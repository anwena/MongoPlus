package com.anwen.mongo.sql.inject.update;

import com.anwen.mongo.enums.CpmpareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.conditions.interfaces.Inject.InjectUpdate;
import com.anwen.mongo.sql.interfaces.CompareCondition;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-23 22:23
 **/
@Getter
public class LambdaUpdateChainInjectWrapper extends AbstractChainWrapper<String, LambdaUpdateChainInjectWrapper> implements InjectUpdate<LambdaUpdateChainInjectWrapper> {

    private final List<CompareCondition> updateCompareList = new ArrayList<>();

    private final SqlOperation<Map<String,Object>> sqlOperation;

    public LambdaUpdateChainInjectWrapper(SqlOperation<Map<String,Object>> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    @Override
    public LambdaUpdateChainInjectWrapper set(boolean condition, String column, Object value) {
        return condition ? set(column,value) : typedThis;
    }

    @Override
    public LambdaUpdateChainInjectWrapper set(String column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    private LambdaUpdateChainInjectWrapper getBaseUpdateCompare(String column, Object value){
        updateCompareList.add(new CompareCondition(new Throwable().getStackTrace()[1].getMethodName(), column,value, CpmpareEnum.UPDATE.getKey(), LogicTypeEnum.AND.getKey()));
        return this;
    }

    public boolean update(String collectionName){
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(getCompareList());
        compareConditionList.addAll(getUpdateCompareList());
        return sqlOperation.doUpdate(collectionName,compareConditionList);
    }

    public boolean remove(String collectionName) {
        return sqlOperation.doRemove(collectionName,getCompareList());
    }
}
