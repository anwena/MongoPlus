package com.anwen.mongo.conditions.inject.query;

import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.execute.ExecutorFactory;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

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

    private final BaseMapper baseMapper;

    public LambdaQueryChainInjectWrapper(BaseMapper baseMapper) {
        this.baseMapper = baseMapper;
    }

    @Override
    public List<Map<String, Object>> list(String collectionName) {
        return list(EMPTY,collectionName);
    }

    @Override
    public List<Map<String, Object>> list(String database, String collectionName) {
//        return factory.getInjectExecute(database).list(collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList());
        return null;
    }

    @Override
    public Map<String, Object> limitOne(String collectionName) {
        return limitOne(EMPTY,collectionName);
    }

    @Override
    public Map<String, Object> limitOne(String database, String collectionName) {
//        return factory.getInjectExecute(database).limitOne(collectionName,getCompareList(),getProjectionList(),getBasicDBObjectList(),getOrderList());
        return null;
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam) {
        return page(EMPTY,collectionName,pageParam);
    }

    @Override
    public PageResult<Map<String, Object>> page(String database, String collectionName, PageParam pageParam) {
//        return factory.getInjectExecute(database).page(collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageParam.getPageNum(),pageParam.getPageSize());
        return null;
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize) {
        return page(EMPTY,collectionName,pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(String database, String collectionName, Integer pageNum, Integer pageSize) {
//        return factory.getInjectExecute(database).page(collectionName,getCompareList(),getOrderList(),getProjectionList(),getBasicDBObjectList(),pageNum,pageSize);
        return null;
    }

    @Override
    public Map<String, Object> one(String collectionName) {
        return one(EMPTY,collectionName);
    }

    @Override
    public Map<String, Object> one(String database, String collectionName) {
//        return factory.getInjectExecute(database).one(collectionName,getCompareList(),getProjectionList(),getBasicDBObjectList());
        return null;
    }

    @Override
    public long count(String collectionName) {
        return count(EMPTY,collectionName);
    }

    @Override
    public long count(String database, String collectionName) {
//        return factory.getInjectExecute(database).count(collectionName,getCompareList());
        return 0;
    }
}
