package com.anwen.mongo.conditions.inject.query;

import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.toolkit.StringPool;
import com.mongodb.client.ClientSession;

import java.util.List;
import java.util.Map;

import static com.anwen.mongo.toolkit.StringPool.EMPTY;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-21 00:22
 **/
public class LambdaQueryChainInjectWrapper extends QueryChainWrapper<Map<String,Object>, LambdaQueryChainInjectWrapper> implements ChainInject {

    private final SqlExecute sqlExecute;

    private final ExecutorFactory factory;

    public LambdaQueryChainInjectWrapper(SqlExecute sqlExecute, ExecutorFactory factory) {
        this.sqlExecute = sqlExecute;
        this.factory = factory;
    }

    @Override
    public List<Map<String, Object>> list(String collectionName) {
        return list(EMPTY,collectionName);
    }

    @Override
    public List<Map<String, Object>> list(String database, String collectionName) {
        return factory.getInjectExecute(database).list(collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    @Deprecated
    public List<Map<String, Object>> list(ClientSession clientSession, String collectionName) {
        return sqlExecute.doList(clientSession,collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    public Map<String, Object> limitOne(String collectionName) {
        return limitOne(EMPTY,collectionName);
    }

    @Override
    public Map<String, Object> limitOne(String database, String collectionName) {
        return factory.getInjectExecute(database).limitOne(collectionName,getCompareList(),getProjectionList(),getBasicDBObjectList(),getOrderList());
    }

    @Override
    @Deprecated
    public Map<String, Object> limitOne(ClientSession clientSession, String collectionName) {
        return sqlExecute.doLimitOne(clientSession,collectionName,getCompareList(),getProjectionList(),getBasicDBObjectList(),getOrderList());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam) {
        return page(EMPTY,collectionName,pageParam);
    }

    @Override
    public PageResult<Map<String, Object>> page(String database, String collectionName, PageParam pageParam) {
        return factory.getInjectExecute(database).page(collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    @Deprecated
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, PageParam pageParam) {
        return sqlExecute.doPage(clientSession,collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize) {
        return page(EMPTY,collectionName,pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(String database, String collectionName, Integer pageNum, Integer pageSize) {
        return factory.getInjectExecute(database).page(collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize);
    }

    @Override
    @Deprecated
    public PageResult<Map<String, Object>> page(ClientSession clientSession, String collectionName, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(clientSession,collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize);
    }

    @Override
    public Map<String, Object> one(String collectionName) {
        return one(EMPTY,collectionName);
    }

    @Override
    public Map<String, Object> one(String database, String collectionName) {
        return factory.getInjectExecute(database).one(collectionName,getCompareList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    @Deprecated
    public Map<String, Object> one(ClientSession clientSession, String collectionName) {
        return sqlExecute.doOne(clientSession,collectionName,getCompareList(),getProjectionList(),getBasicDBObjectList());
    }

    @Override
    public long count(String collectionName) {
        return count(EMPTY,collectionName);
    }

    @Override
    public long count(String database, String collectionName) {
        return factory.getInjectExecute(database).count(collectionName,getCompareList());
    }

    @Override
    @Deprecated
    public long count(ClientSession clientSession, String collectionName) {
        return sqlExecute.doCount(clientSession,collectionName,getCompareList());
    }
}
