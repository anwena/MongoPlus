package com.anwen.mongo.sql.conditions.query;

import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.sql.interfaces.CompareCondition;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

/**
 * 查询实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:11
*/
public class LambdaQueryChainWrapper<T> extends AbstractChainWrapper<T,LambdaQueryChainWrapper<T>> implements ChainQuery<T> {

    private final SqlOperation<T> sqlOperation;

    public LambdaQueryChainWrapper(Class<T> clazz , SqlOperation<T> sqlOperation) {
        this.sqlOperation = sqlOperation;
        T tClass;
        try {
            tClass = clazz.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
        sqlOperation.init(tClass.getClass());
    }

    /*@Override
    public List<T> list(LambdaAbstractWrapperChainWrapper<T> lambdaQueryMongoWrapper) {
        sqlOperation.init(getEClass());
        return sqlOperation.doList(lambdaQueryMongoWrapper.getQueryCondition().getCompareList(), lambdaQueryMongoWrapper.getQueryCondition().getOrderList());
    }*/
    @Override
    public List<T> list() {
        return sqlOperation.doList(getCompareList(), getOrderList());
    }

    @Override
    public T one() {
        return sqlOperation.doOne(getCompareList());
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return sqlOperation.doPage(getCompareList(),getOrderList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return sqlOperation.doPage(getCompareList(),getOrderList(),pageNum,pageSize);
    }
}
