package com.anwen.mongo.conditions.inject.update;

import com.anwen.mongo.conditions.AbstractChainWrapper;
import com.anwen.mongo.conditions.interfaces.Inject.InjectUpdate;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.enums.CompareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.execute.SqlOperation;
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

    private final SqlOperation sqlOperation;

    public LambdaUpdateChainInjectWrapper(SqlOperation sqlOperation) {
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
        updateCompareList.add(CompareCondition.builder().condition(new Throwable().getStackTrace()[1].getMethodName()).column(column).value(value).type(CompareEnum.UPDATE.getKey()).logicType(LogicTypeEnum.AND.getKey()).build());
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
