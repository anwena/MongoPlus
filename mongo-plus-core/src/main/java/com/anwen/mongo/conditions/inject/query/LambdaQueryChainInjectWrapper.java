package com.anwen.mongo.conditions.inject.query;

import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-21 00:22
 **/
public class LambdaQueryChainInjectWrapper extends QueryChainWrapper<Map<String,Object>, LambdaQueryChainInjectWrapper> implements ChainInject {

    private final SqlExecute sqlExecute;

    public LambdaQueryChainInjectWrapper(SqlExecute sqlExecute) {
        this.sqlExecute = sqlExecute;
    }

    @Override
    public List<Map<String, Object>> list(String collection) {
        return sqlExecute.doList(collection,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    public Map<String, Object> limitOne(String collectionName) {
        return sqlExecute.doLimitOne(collectionName,getCompareList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam) {
        return sqlExecute.doPage(collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize);
    }

    @Override
    public Map<String, Object> one(String collectionName) {
        return sqlExecute.doOne(collectionName,getCompareList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    public long count(String collectionName) {
        return sqlExecute.doCount(collectionName,getCompareList());
    }
}