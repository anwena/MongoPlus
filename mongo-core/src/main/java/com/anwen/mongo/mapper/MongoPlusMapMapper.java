package com.anwen.mongo.mapper;

import com.anwen.mongo.conditions.inject.query.LambdaQueryChainInjectWrapper;
import com.anwen.mongo.conditions.inject.update.LambdaUpdateChainInjectWrapper;
import com.anwen.mongo.conditions.interfaces.Inject.InjectQuery;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.sql.SqlOperation;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 **/
public class MongoPlusMapMapper implements InjectQuery {
    private final SqlOperation<Map<String,Object>> sqlOperation;

    public MongoPlusMapMapper(SqlOperation<Map<String, Object>> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    public LambdaQueryChainInjectWrapper lambdaQuery(){
        return new LambdaQueryChainInjectWrapper(sqlOperation);
    }

    public LambdaUpdateChainInjectWrapper lambdaUpdate(){
        return new LambdaUpdateChainInjectWrapper(sqlOperation);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName) {
        return sqlOperation.doList(collectionName);
    }

    @Override
    public List<Map<String, Object>> list(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return sqlOperation.doList(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList());
    }

    @Override
    public Map<String, Object> one(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return sqlOperation.doOne(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList());
    }

    @Override
    public Map<String, Object> limitOne(String collectionName, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlOperation.doLimitOne(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList());
    }


    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam) {
        return sqlOperation.doPage(collectionName,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return sqlOperation.doPage(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize) {
        return sqlOperation.doPage(collectionName,pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize, QueryChainWrapper<Map<String, Object>, ?> queryChainWrapper) {
        return sqlOperation.doPage(collectionName,queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),pageNum,pageSize);
    }

    @Override
    public Map<String, Object> getById(String collectionName , Serializable id) {
        return sqlOperation.doGetById(collectionName,id);
    }

    @Override
    public List<Map<String, Object>> getByIds(String collectionName , Collection<Serializable> ids) {
        return sqlOperation.doGetByIds(collectionName,ids);
    }

    @Override
    public Boolean save(String collectionName, Map<String, Object> entityMap) {
        return sqlOperation.doSave(collectionName,entityMap);
    }

    @Override
    public Boolean saveBatch(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlOperation.doSaveBatch(collectionName,entityMapList);
    }

    @Override
    public Boolean saveOrUpdate(String collectionName, Map<String, Object> entityMap) {
        return sqlOperation.doSaveOrUpdate(collectionName,entityMap);
    }

    @Override
    public Boolean saveOrUpdateBatch(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlOperation.doSaveOrUpdateBatch(collectionName,entityMapList);
    }

    @Override
    public Boolean updateById(String collectionName, Map<String, Object> entityMap) {
        return sqlOperation.doUpdateById(collectionName,entityMap);
    }

    @Override
    public Boolean updateBatchByIds(String collectionName, Collection<Map<String, Object>> entityMapList) {
        return sqlOperation.doUpdateBatchByIds(collectionName,entityMapList);
    }

    @Override
    public Boolean updateByColumn(String collectionName, Map<String, Object> entityMap, String column) {
        return sqlOperation.doUpdateByColumn(collectionName,entityMap,column);
    }

    @Override
    public Boolean removeById(String collectionName, Serializable id) {
        return sqlOperation.doRemoveById(collectionName,id);
    }

    @Override
    public Boolean removeByColumn(String collectionName, String column, String value) {
        return sqlOperation.doRemoveByColumn(collectionName,column,value);
    }

    @Override
    public Boolean removeBatchByIds(String collectionName, Collection<Serializable> idList) {
        return sqlOperation.doRemoveBatchByIds(collectionName,idList);
    }

    @Override
    public long count(String collectionName, QueryChainWrapper<Map<String,Object>,?> queryChainWrapper) {
        return sqlOperation.doCount(collectionName,queryChainWrapper.getCompareList());
    }

    @Override
    public long count(String collectionName) {
        return sqlOperation.doCount(collectionName);
    }
}
