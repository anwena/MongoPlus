package com.anwen.mongo.conditions.query;

import com.anwen.mongo.execute.SqlOperation;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

/**
 * 查询实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:11
*/
public class LambdaQueryChainWrapper<T> extends QueryChainWrapper<T,LambdaQueryChainWrapper<T>> implements ChainQuery<T> {

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

    public LambdaQueryChainWrapper(SqlOperation<T> sqlOperation){
        this.sqlOperation = sqlOperation;
    }

    @Override
    public List<T> list() {
        return sqlOperation.doList(getCompareList(), getOrderList(),getProjectionList());
    }


    @Override
    public T one() {
        return sqlOperation.doOne(getCompareList(),getProjectionList());
    }

    @Override
    public T limitOne() {
        return sqlOperation.doLimitOne(getCompareList(),getProjectionList());
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return sqlOperation.doPage(getCompareList(),getOrderList(),getProjectionList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return sqlOperation.doPage(getCompareList(),getOrderList(),getProjectionList(),pageNum,pageSize);
    }

    @Override
    public long count() {
        return sqlOperation.doCount(getCompareList());
    }
}
