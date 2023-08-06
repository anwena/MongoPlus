package com.anwen.mongo.sql.inject.query;

import com.anwen.mongo.sql.SqlOperation;
import com.anwen.mongo.sql.conditions.interfaces.Inject.InjectQuery;
import com.anwen.mongo.sql.inject.update.LambdaUpdateChainInjectWrapper;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @author JiaChaoYang
 * @project mongo
 * @description
 * @date 2023-07-20 21:54
 **/
public class MongoPlusOperate implements InjectQuery {

    private final SqlOperation<Map<String,Object>> sqlOperation;

    public MongoPlusOperate(SqlOperation<Map<String, Object>> sqlOperation) {
        this.sqlOperation = sqlOperation;
    }

    /*@Override
    public void onApplicationEvent(SqlOperationInitializedEvent event) {
        sqlOperation = (SqlOperation<Map<String,Object>>) event.getSqlOperation();
    }*/

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
    public List<Map<String, Object>> list(String collectionName, LambdaQueryChainInjectWrapper lambdaQueryChainInjectWrapper) {
        return sqlOperation.doList(collectionName,lambdaQueryChainInjectWrapper.getCompareList(),lambdaQueryChainInjectWrapper.getOrderList(),lambdaQueryChainInjectWrapper.getProjectionList());
    }


    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam) {
        return sqlOperation.doPage(collectionName,pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, PageParam pageParam, LambdaQueryChainInjectWrapper lambdaQueryChainInjectWrapper) {
        return sqlOperation.doPage(collectionName,lambdaQueryChainInjectWrapper.getCompareList(),lambdaQueryChainInjectWrapper.getOrderList(),lambdaQueryChainInjectWrapper.getProjectionList(),pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize) {
        return sqlOperation.doPage(collectionName,pageNum,pageSize);
    }

    @Override
    public PageResult<Map<String, Object>> page(String collectionName, Integer pageNum, Integer pageSize, LambdaQueryChainInjectWrapper lambdaQueryChainInjectWrapper) {
        return sqlOperation.doPage(collectionName,lambdaQueryChainInjectWrapper.getCompareList(),lambdaQueryChainInjectWrapper.getOrderList(),lambdaQueryChainInjectWrapper.getProjectionList(),pageNum,pageSize);
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
    public long count(String collectionName) {
        return sqlOperation.doCount(collectionName);
    }

}
