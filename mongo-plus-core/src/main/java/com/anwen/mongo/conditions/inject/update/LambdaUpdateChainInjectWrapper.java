package com.anwen.mongo.conditions.inject.update;

import com.anwen.mongo.conditions.AbstractChainWrapper;
import com.anwen.mongo.conditions.interfaces.Inject.InjectUpdate;
import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.enums.CompareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.execute.ExecutorFactory;

import java.util.ArrayList;
import java.util.List;

import static com.anwen.mongo.toolkit.StringPool.EMPTY;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-23 22:23
 **/
public class LambdaUpdateChainInjectWrapper extends AbstractChainWrapper<String, LambdaUpdateChainInjectWrapper> implements InjectUpdate<LambdaUpdateChainInjectWrapper> {

    private final List<CompareCondition> updateCompareList = new ArrayList<>();

    private final ExecutorFactory factory;

    public LambdaUpdateChainInjectWrapper(ExecutorFactory factory) {
        this.factory = factory;
    }

    @Override
    public LambdaUpdateChainInjectWrapper set(boolean condition, String column, Object value) {
        return condition ? set(column,value) : this;
    }

    @Override
    public LambdaUpdateChainInjectWrapper push(String column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    @Override
    public LambdaUpdateChainInjectWrapper push(boolean condition, String column, Object value) {
        return condition ? push(column,value) : this;
    }

    @Override
    public LambdaUpdateChainInjectWrapper push(String column, Object... value) {
        for (Object o : value) {
            getBaseUpdateCompare(column,o);
        }
        return this;
    }

    @Override
    public LambdaUpdateChainInjectWrapper push(boolean condition, String column, Object... value) {
        return condition ? push(column,value) : this;
    }

    @Override
    public LambdaUpdateChainInjectWrapper push(String column, List<?> value) {
        for (Object o : value) {
            getBaseUpdateCompare(column,o);
        }
        return this;
    }

    @Override
    public LambdaUpdateChainInjectWrapper push(boolean condition, String column, List<?> value) {
        return condition ? push(column,value) : this;
    }

    @Override
    public LambdaUpdateChainInjectWrapper set(String column, Object value) {
        return getBaseUpdateCompare(column,value);
    }

    private LambdaUpdateChainInjectWrapper getBaseUpdateCompare(String column, Object value){
        updateCompareList.add(CompareCondition.builder().condition(new Throwable().getStackTrace()[1].getMethodName()).column(column).value(value).build());
        return this;
    }

    public boolean update(String collectionName){
        return update(EMPTY,collectionName);
    }

    public boolean update(String database,String collectionName){
        List<CompareCondition> compareConditionList = new ArrayList<>();
        compareConditionList.addAll(getCompareList());
        compareConditionList.addAll(getUpdateCompareList());
        return factory.getInjectExecute(database).update(collectionName,compareConditionList);
    }

    public boolean remove(String collectionName) {
        return remove(EMPTY,collectionName);
    }

    public boolean remove(String database,String collectionName) {
        return factory.getInjectExecute(database).remove(collectionName,getCompareList());
    }

    public List<CompareCondition> getUpdateCompareList() {
        return updateCompareList;
    }

}
