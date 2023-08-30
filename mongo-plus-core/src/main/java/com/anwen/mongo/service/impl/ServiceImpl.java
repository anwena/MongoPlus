package com.anwen.mongo.service.impl;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.execute.SqlExecute;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.support.SFunction;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Collection;
import java.util.List;

/**
 * @author JiaChaoYang
 * 接口实现
 * @since 2023-02-09 14:13
 **/
public class ServiceImpl<T> implements IService<T>{

    private SqlExecute sqlExecute;

    public void setSqlOperation(SqlExecute sqlExecute) {
        this.sqlExecute = sqlExecute;
    }

    private Class<T> clazz;

    public void setClazz(Class<?> clazz) {
        this.clazz = (Class<T>) clazz;
    }

    @Override
    public Class<T> getGenericityClazz() {
        if (clazz != null) {
            return clazz;
        }
        Type superClassType = getClass().getGenericSuperclass();
        ParameterizedType pt = (ParameterizedType) superClassType;
        Type genType = pt.getActualTypeArguments()[0];

        if (genType instanceof Class) {
            clazz = (Class<T>) genType;
        } else if (genType instanceof TypeVariable) {
            // 处理泛型类型是 TypeVariable 的情况
            clazz = (Class<T>) Object.class;
        } else {
            throw new IllegalArgumentException("Unsupported generic type: " + genType);
        }
        return clazz;
    }

    @Override
    public Boolean save(T entity) {
        return sqlExecute.doSave(entity);
    }

    @Override
    public Boolean saveBatch(Collection<T> entityList) {
        return sqlExecute.doSaveBatch(entityList);
    }

    @Override
    public Boolean saveOrUpdate(T entity) {
        return sqlExecute.doSaveOrUpdate(entity);
    }

    @Override
    public Boolean saveOrUpdateBatch(Collection<T> entityList) {
        return sqlExecute.doSaveOrUpdateBatch(entityList);
    }

    @Override
    public Boolean updateById(T entity) {
        return sqlExecute.doUpdateById(entity);
    }

    @Override
    public Boolean updateBatchByIds(Collection<T> entityList) {
        return sqlExecute.doUpdateBatchByIds(entityList);
    }

    @Override
    public Boolean updateByColumn(T entity, SFunction<T, Object> column) {
        return sqlExecute.doUpdateByColumn(entity,column);
    }

    @Override
    public Boolean updateByColumn(T entity, String column) {
        return sqlExecute.doUpdateByColumn(entity,column);
    }

    @Override
    public Boolean removeById(Serializable id) {
        return sqlExecute.doRemoveById(id);
    }

    @Override
    public Boolean removeByColumn(SFunction<T, Object> column, String value) {
        return sqlExecute.doRemoveByColumn(column,value);
    }

    @Override
    public Boolean removeByColumn(String column, String value) {
        return sqlExecute.doRemoveByColumn(column,value);
    }

    @Override
    public Boolean removeBatchByIds(Collection<Serializable> idList) {
        return sqlExecute.doRemoveBatchByIds(idList);
    }

    @Override
    public List<T> list() {
        return sqlExecute.doList();
    }

    @Override
    public List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doAggregateList(queryChainWrapper.getBaseAggregateList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public T one(QueryChainWrapper<T,?> queryChainWrapper) {
        return sqlExecute.doOne(queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public T limitOne(QueryChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doLimitOne(queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public List<T> list(QueryChainWrapper<T,?> queryChainWrapper) {
        return sqlExecute.doList(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public List<T> list(AggregateChainWrapper<T,?> queryChainWrapper) {
        return sqlExecute.doAggregateList(queryChainWrapper.getBaseAggregateList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public long count() {
        return sqlExecute.doCount();
    }

    @Override
    public long count(QueryChainWrapper<T, ?> queryChainWrapper) {
        return sqlExecute.doCount(queryChainWrapper.getCompareList());
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize){
        return sqlExecute.doPage(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(), pageNum,pageSize);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return page(pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return sqlExecute.doPage(null,null,null,null,pageNum,pageSize);
    }

    @Override
    public T getById(Serializable id) {
        return sqlExecute.doGetById(id);
    }

    @Override
    public List<T> getByIds(Collection<Serializable> ids) {
        return sqlExecute.doGetByIds(ids);
    }

    @Override
    public SqlExecute getSqlOperation() {
        return sqlExecute;
    }

    public Class<T> getClazz() {
        return clazz;
    }
}
