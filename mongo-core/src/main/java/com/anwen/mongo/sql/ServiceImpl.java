package com.anwen.mongo.sql;

import com.anwen.mongo.sql.interfaces.Compare;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.query.AbstractChainWrapper;
import com.anwen.mongo.sql.query.LambdaQueryMongoWrapper;
import com.anwen.mongo.sql.support.SFunction;

import javax.annotation.Resource;
import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;

/**
 * @author JiaChaoYang
 * 接口实现
 * @since 2023-02-09 14:13
 **/
public class ServiceImpl<T> implements IService<T> {

    @Resource
    private SqlOperation sqlOperation;

    public <T> Class<T> getEClass() {

        //get the Class object of this own class
        Class<? extends ServiceImpl> thisClass = this.getClass();

        //get the Type Object of supper class
        Type superClassType = thisClass.getGenericSuperclass();
        ParameterizedType pt = (ParameterizedType) superClassType;

        //get the Generic Type array
        Type[] genTypeArr = pt.getActualTypeArguments();
        Type genType = genTypeArr[0];
        if (!(genType instanceof Class)) {
            return (Class<T>) Object.class;
        }

        return (Class<T>) genType;
    }

    @Override
    public Boolean save(T entity) {
        sqlOperation.init(getEClass());
        return sqlOperation.doSave(entity);
    }

    @Override
    public Boolean saveBatch(Collection<T> entityList) {
        sqlOperation.init(getEClass());
        return sqlOperation.doSaveBatch(entityList);
    }

    @Override
    public Boolean saveOrUpdate(T entity) {
        sqlOperation.init(getEClass());
        return sqlOperation.doSaveOrUpdate(entity);
    }

    @Override
    public Boolean saveOrUpdateBatch(Collection<T> entityList) {
        sqlOperation.init(getEClass());
        return sqlOperation.doSaveOrUpdateBatch(entityList);
    }

    @Override
    public Boolean updateById(T entity) {
        sqlOperation.init(getEClass());
        return sqlOperation.doUpdateById(entity);
    }

    @Override
    public Boolean updateBatchByIds(Collection<T> entityList) {
        sqlOperation.init(getEClass());
        return sqlOperation.doUpdateBatchByIds(entityList);
    }

    @Override
    public Boolean updateByColumn(T entity, SFunction<T, Object> column) {
        sqlOperation.init(getEClass());
        return sqlOperation.doUpdateByColumn(entity,column);
    }

    @Override
    public Boolean updateByColumn(T entity, String column) {
        sqlOperation.init(getEClass());
        return sqlOperation.doUpdateByColumn(entity,column);
    }

    @Override
    public Boolean removeById(Serializable id) {
        sqlOperation.init(getEClass());
        return sqlOperation.doRemoveById(id);
    }

    @Override
    public Boolean removeByColumn(SFunction<T, Object> column, String value) {
        sqlOperation.init(getEClass());
        return sqlOperation.doRemoveByColumn(column,value);
    }

    @Override
    public Boolean removeByColumn(String column, String value) {
        sqlOperation.init(getEClass());
        return sqlOperation.doRemoveByColumn(column,value);
    }

    @Override
    public Boolean removeBatchByIds(Collection<Object> idList) {
        sqlOperation.init(getEClass());
        return sqlOperation.doRemoveBatchByIds(idList);
    }

    @Override
    public List<T> list() {
        sqlOperation.init(getEClass());
        return (List<T>) sqlOperation.doList();
    }

    @Override
    public List<T> list(List<Compare> compareList, List<Order> orderList) {
        sqlOperation.init(getEClass());
        return (List<T>) sqlOperation.doList(compareList,orderList);
    }

    @Override
    public T one(List<Compare> compareList, List<Order> orderList) {
        sqlOperation.init(getEClass());
        return sqlOperation.doOne(compareList,orderList);
    }

    @Override
    public T getById(Serializable id) {
        sqlOperation.init(getEClass());
        return sqlOperation.doGetById(id);
    }

    @Override
    public LambdaQueryMongoWrapper<T> lambdaQuery() {
        sqlOperation.init(getEClass());
        return new AbstractChainWrapper<>(getEClass(),this);
    }
}
