package com.anwen.mongo.sql.query;

import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.conditions.interfaces.Query;
import com.anwen.mongo.sql.support.SFunction;

public class QueryChainWrapper<T,Children extends AbstractChainWrapper<T,Children>> extends AbstractChainWrapper<T,Children> implements Query<T,Children> {
    @Override
    public Children select(SFunction<T, Object> column) {
        return null;
    }

    @Override
    public Children or(boolean condition, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return condition ? or(lambdaQueryChainWrapper) : typedThis;
    }

    @Override
    public Children or(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return getBaseOrCondition(lambdaQueryChainWrapper.getCompareList());
    }

    @Override
    public Children or(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? or(column,value) : typedThis;
    }

    @Override
    public Children and(boolean condition, LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return condition ? and(lambdaQueryChainWrapper) : typedThis;
    }

    @Override
    public Children and(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper) {
        return getBaseAndCondition(lambdaQueryChainWrapper.getCompareList());
    }

    @Override
    public Children or(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value, LogicTypeEnum.OR.getKey());
    }

    @Override
    public Children or(boolean condition, String column, Object value) {
        return condition ? or(column,value) : typedThis;
    }

    @Override
    public Children or(String column, Object value) {
        return getBaseCondition(column,value,LogicTypeEnum.OR.getKey());
    }

}
