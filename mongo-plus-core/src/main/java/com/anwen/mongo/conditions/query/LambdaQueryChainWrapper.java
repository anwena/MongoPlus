package com.anwen.mongo.conditions.query;

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

    private final Class<T> clazz;

    public LambdaQueryChainWrapper(SqlExecute sqlExecute, Class<T> clazz){
        this.sqlExecute = sqlExecute;
        this.clazz = clazz;
    }

    @Override
    public List<T> list() {
        return sqlExecute.doList(getCompareList(), getOrderList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public List<T> list(ClientSession clientSession) {
        return sqlExecute.doList(clientSession,getCompareList(), getOrderList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T one() {
        return sqlExecute.doOne(getCompareList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T one(ClientSession clientSession) {
        return sqlExecute.doOne(clientSession,getCompareList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T limitOne() {
        return sqlExecute.doLimitOne(getCompareList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public T limitOne(ClientSession clientSession) {
        return sqlExecute.doLimitOne(clientSession,getCompareList(),getProjectionList(),getBasicDBObjectList(),clazz);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return sqlExecute.doPage(getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public PageResult<T> page(ClientSession clientSession, PageParam pageParam) {
        return sqlExecute.doPage(clientSession,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize(),clazz);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize,clazz);
    }

    @Override
    public PageResult<T> page(ClientSession clientSession, Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(clientSession,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize,clazz);
    }

    @Override
    public long count() {
        return sqlExecute.doCount(getCompareList(),clazz);
    }

    @Override
    public long count(ClientSession clientSession) {
        return sqlExecute.doCount(clientSession,getCompareList(),clazz);
    }
}
