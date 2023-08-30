package com.anwen.mongo.conditions.query;

import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

import java.util.List;

/**
 * 查询实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:11
*/
public class LambdaQueryChainWrapper<T> extends QueryChainWrapper<T,LambdaQueryChainWrapper<T>> implements ChainQuery<T> {

    private final SqlExecute sqlExecute;

    public LambdaQueryChainWrapper(SqlExecute sqlExecute){
        this.sqlExecute = sqlExecute;
    }

    @Override
    public List<T> list() {
        return sqlExecute.doList(getCompareList(), getOrderList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    public T one() {
        return sqlExecute.doOne(getCompareList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    public T limitOne() {
        return sqlExecute.doLimitOne(getCompareList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return sqlExecute.doPage(getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize);
    }

    @Override
    public long count() {
        return sqlExecute.doCount(getCompareList());
    }
}
