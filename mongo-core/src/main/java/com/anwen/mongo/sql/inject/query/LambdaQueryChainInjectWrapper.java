package com.anwen.mongo.sql.inject.query;

import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.sql.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;
import com.anwen.mongo.sql.query.QueryChainWrapper;

import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-21 00:22
 **/
public class LambdaQueryChainInjectWrapper extends QueryChainWrapper<Map<String,Object>, LambdaQueryChainInjectWrapper> implements ChainInject {

    private final SqlOperation<Map<String,Object>> sqlOperation;

    public LambdaQueryChainInjectWrapper(SqlOperation<Map<String, Object>> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    @Override
    public List<Map<String, Object>> list(String collection) {
        return sqlOperation.doList(collection,getCompareList(),getOrderList(),getProjectionList());
    }

    @Override
    public Map<String, Object> one(String collectionName) {
        return sqlOperation.doOne(collectionName,getCompareList(),getProjectionList());
    }

    @Override
    public Map<String, Object> limitOne(String collectionName) {
        return sqlOperation.doLimitOne(collectionName,getCompareList(),getProjectionList());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam) {
        return sqlOperation.doPage(collectionName,getCompareList(),getOrderList(),getProjectionList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize) {
        return sqlOperation.doPage(collectionName,getCompareList(),getOrderList(),getProjectionList(),pageNum,pageSize);
    }

    @Override
    public long count(String collectionName) {
        return sqlOperation.doCount(collectionName,getCompareList());
    }

    @Override
    public long count(String collectionName, LambdaQueryChainInjectWrapper lambdaQueryChainInjectWrapper) {
        return sqlOperation.doCount(collectionName,lambdaQueryChainInjectWrapper.getCompareList());
    }
}
