package com.anwen.mongo.sql.conditions.query;

import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.sql.interfaces.CompareCondition;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;

/**
 * 查询实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:11
*/
public class LambdaQueryChainWrapper<T> extends AbstractChainWrapper<T,LambdaQueryChainWrapper<T>> implements ChainQuery<T> {

    private SqlOperation<T> sqlOperation;

    public LambdaQueryChainWrapper(SqlOperation<T> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    public <T> Class<T> getEClass() {

        //get the Class object of this own class
        Class<? extends LambdaQueryChainWrapper> thisClass = this.getClass();

        //get the Type Object of supper class
        Type superClassType = thisClass.getGenericSuperclass();
        ParameterizedType pt = (ParameterizedType) superClassType;

        //get the Generic Type array
        Type[] genTypeArr = pt.getActualTypeArguments();
        Type genType = genTypeArr[0];
        if (!(genType instanceof Class)) {
            return (Class<T>) Object.class;
        }

        return (Class<T>) genType;
    }

    /*@Override
    public List<T> list(LambdaAbstractWrapperChainWrapper<T> lambdaQueryMongoWrapper) {
        sqlOperation.init(getEClass());
        return sqlOperation.doList(lambdaQueryMongoWrapper.getQueryCondition().getCompareList(), lambdaQueryMongoWrapper.getQueryCondition().getOrderList());
    }*/
    @Override
    public List<T> list() {
        sqlOperation.init(getEClass());
        return sqlOperation.doList(getCompareList(), getOrderList());
    }

    @Override
    public T one() {
        sqlOperation.init(getEClass());
        return sqlOperation.doOne(getCompareList());
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        sqlOperation.init(getEClass());
        return sqlOperation.doPage(getCompareList(),getOrderList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        sqlOperation.init(getEClass());
        return sqlOperation.doPage(getCompareList(),getOrderList(),pageNum,pageSize);
    }
}
