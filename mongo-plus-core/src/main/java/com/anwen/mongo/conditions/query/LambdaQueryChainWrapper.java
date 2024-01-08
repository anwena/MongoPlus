package com.anwen.mongo.conditions.query;

import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.ClientSession;

import java.util.List;

/**
 * 查询实现
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:11
*/
public class LambdaQueryChainWrapper<T> extends QueryChainWrapper<T,LambdaQueryChainWrapper<T>> implements ChainQuery<T> {

    private final SqlExecute sqlExecute;

    private final ExecutorFactory factory;

    private final Class<T> clazz;

    private final String database;

    public LambdaQueryChainWrapper(SqlExecute sqlExecute,ExecutorFactory factory, Class<T> clazz,String database){
        this.sqlExecute = sqlExecute;
        this.factory = factory;
        this.clazz = clazz;
        this.database = database;
    }

    @Override
    public List<T> list() {
        return factory.getExecute(database).list(getCompareList(), getOrderList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public List<T> list(ClientSession clientSession) {
        return sqlExecute.doList(clientSession,getCompareList(), getOrderList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T one() {
        return factory.getExecute(database).one(getCompareList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T one(ClientSession clientSession) {
        return sqlExecute.doOne(clientSession,getCompareList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T limitOne() {
        return factory.getExecute(database).limitOne(getCompareList(),getProjectionList(),getBasicDBObjectList(),getOrderList(),clazz);
    }

    @Override
    public T limitOne(ClientSession clientSession) {
        return sqlExecute.doLimitOne(clientSession,getCompareList(),getProjectionList(),getBasicDBObjectList(),getOrderList(),clazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return factory.getExecute(database).page(getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public PageResult<T> page(ClientSession clientSession, PageParam pageParam) {
        return sqlExecute.doPage(clientSession,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return factory.getExecute(database).page(getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize,clazz);
    }

    @Override
    public PageResult<T> page(ClientSession clientSession, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(clientSession,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize,clazz);
    }

    @Override
    public long count() {
        return factory.getExecute(database).count(getCompareList(),clazz);
    }

    @Override
    public long count(ClientSession clientSession) {
        return sqlExecute.doCount(clientSession,getCompareList(),clazz);
    }
}
