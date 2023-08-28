package com.anwen.mongo.service.impl;

import com.anwen.mongo.conditions.aggregate.AggregateChainWrapper;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.execute.SqlOperation;
import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.anwen.mongo.service.IService;
import com.anwen.mongo.support.SFunction;
import lombok.Data;

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
@Data
public class ServiceImpl<T> implements IService<T>{

    private SqlOperation sqlOperation;

    public void setSqlOperation(SqlOperation sqlOperation) {
        this.sqlOperation = sqlOperation;
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
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doSave(entity);
    }

    @Override
    public Boolean saveBatch(Collection<T> entityList) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doSaveBatch(entityList);
    }

    @Override
    public Boolean saveOrUpdate(T entity) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doSaveOrUpdate(entity);
    }

    @Override
    public Boolean saveOrUpdateBatch(Collection<T> entityList) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doSaveOrUpdateBatch(entityList);
    }

    @Override
    public Boolean updateById(T entity) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doUpdateById(entity);
    }

    @Override
    public Boolean updateBatchByIds(Collection<T> entityList) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doUpdateBatchByIds(entityList);
    }

    @Override
    public Boolean updateByColumn(T entity, SFunction<T, Object> column) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doUpdateByColumn(entity,column);
    }

    @Override
    public Boolean updateByColumn(T entity, String column) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doUpdateByColumn(entity,column);
    }

    @Override
    public Boolean removeById(Serializable id) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doRemoveById(id);
    }

    @Override
    public Boolean removeByColumn(SFunction<T, Object> column, String value) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doRemoveByColumn(column,value);
    }

    @Override
    public Boolean removeByColumn(String column, String value) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doRemoveByColumn(column,value);
    }

    @Override
    public Boolean removeBatchByIds(Collection<Serializable> idList) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doRemoveBatchByIds(idList);
    }

    @Override
    public List<T> list() {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doList();
    }

    @Override
    public List<T> aggregateList(AggregateChainWrapper<T, ?> queryChainWrapper) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doAggregateList(queryChainWrapper.getBaseAggregateList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public T one(QueryChainWrapper<T,?> queryChainWrapper) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doOne(queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public T limitOne(QueryChainWrapper<T, ?> queryChainWrapper) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doLimitOne(queryChainWrapper.getCompareList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public List<T> list(QueryChainWrapper<T,?> queryChainWrapper) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doList(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public List<T> list(AggregateChainWrapper<T,?> queryChainWrapper) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doAggregateList(queryChainWrapper.getBaseAggregateList(),queryChainWrapper.getBasicDBObjectList());
    }

    @Override
    public long count() {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doCount();
    }

    @Override
    public long count(QueryChainWrapper<T, ?> queryChainWrapper) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doCount(queryChainWrapper.getCompareList());
    }

    @Override
    public PageResult<T> page(QueryChainWrapper<T,?> queryChainWrapper, Integer pageNum, Integer pageSize){
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doPage(queryChainWrapper.getCompareList(),queryChainWrapper.getOrderList(),queryChainWrapper.getProjectionList(),queryChainWrapper.getBasicDBObjectList(), pageNum,pageSize);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return page(pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doPage(null,null,null,null,pageNum,pageSize);
    }

    @Override
    public T getById(Serializable id) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doGetById(id);
    }

    @Override
    public List<T> getByIds(Collection<Serializable> ids) {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation.doGetByIds(ids);
    }

    @Override
    public SqlOperation getSqlOperation() {
        //sqlOperation.setMongoEntity(getEClass());
        return sqlOperation;
    }
}
