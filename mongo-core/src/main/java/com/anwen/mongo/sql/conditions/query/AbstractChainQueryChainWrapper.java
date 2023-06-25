/*
package com.anwen.mongo.sql.query;

import com.anwen.mongo.enums.OrderEnum;
import com.anwen.mongo.sql.ServiceImpl;
import com.anwen.mongo.sql.interfaces.Compare;
import com.anwen.mongo.sql.interfaces.Condition;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;
import com.anwen.mongo.sql.support.SFunction;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

*/
/**
 * @author JiaChaoYang
 * 链式查询 条件实现
 * @since 2023-02-10 10:20
 **//*

public class AbstractChainQueryChainWrapper<T> implements LambdaAbstractWrapperChainWrapper<T> {

    */
/**
     * 构建条件对象
     * @since 2023/2/10 12:00
    *//*

    List<Compare> compareList = new ArrayList<>();

    */
/**
     * 构建排序对象
     * @since 2023/2/10 12:00
    *//*

    List<Order> orderList = new ArrayList<>();

    private final ServiceImpl<T> serviceImpl;

    private final T t;

    public AbstractChainQueryChainWrapper(Class<T> tClass, ServiceImpl<T> serviceImpl) {
        try {
            this.t = tClass.getDeclaredConstructor().newInstance();
            this.serviceImpl = serviceImpl;
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> eq(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? eq(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> eq(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> eq(boolean condition, String column, Object value) {
        return condition ? eq(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> eq(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> ne(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? ne(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> ne(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> ne(boolean condition, String column, Object value) {
        return condition ? ne(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> ne(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> lt(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lt(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> lt(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> lt(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> lt(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> lte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lte(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> lte(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> lte(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> lte(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> gt(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gt(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> gt(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> gt(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> gt(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> gte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gte(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> gte(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> gte(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> gte(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> like(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? like(column,value) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> like(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> like(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> like(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> in(boolean condition, SFunction<T, Object> column, Collection<Object> valueList) {
        return condition ? in(column,valueList) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> in(SFunction<T, Object> column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> in(boolean condition, String column, Collection<Object> valueList) {
        return condition ? in(column,valueList) : this;
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> in(String column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> orderByAsc(SFunction<T, Object> column) {
        return getBaseOrder(OrderEnum.ORDER_BY.getFlag(), column);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> orderByDesc(SFunction<T, Object> column) {
        return getBaseOrder(OrderEnum.ORDER_BY_DESC.getFlag(), column);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> orderByAsc(String column) {
        return getBaseOrder(OrderEnum.ORDER_BY.getFlag(), column);
    }

    @Override
    public LambdaAbstractWrapperChainWrapper<T> orderByDesc(String column) {
        return getBaseOrder(OrderEnum.ORDER_BY_DESC.getFlag(), column);
    }

    @Override
    public PageResult<T> page(Integer pageNum, Integer pageSize) {
        return serviceImpl.page(pageNum,pageSize);
    }

    @Override
    public PageResult<T> page(PageParam pageParam) {
        return page(pageParam.getPageNum(),pageParam.getPageSize());
    }

    @Override
    public List<T> list() {
        return serviceImpl.list(this);
    }

    @Override
    public T one() {
        return serviceImpl.one(this);
    }

    @Override
    public Condition getQueryCondition() {
        return new Condition(compareList,orderList);
    }


    public LambdaAbstractWrapperChainWrapper<T> getBaseOrder(Integer type , String column){
        orderList.add(new Order(type,column));
        return this;
    }

    public LambdaAbstractWrapperChainWrapper<T> getBaseOrder(Integer type , SFunction<T, Object> column){
        orderList.add(new Order(type,column.getFieldNameLine()));
        return this;
    }

    public LambdaAbstractWrapperChainWrapper<T> getBaseCondition(String column, Object value){
        compareList.add(new Compare(new Throwable().getStackTrace()[1].getMethodName(), column,value));
        return this;
    }

    public LambdaAbstractWrapperChainWrapper<T> getBaseCondition(SFunction<T, Object> column, Object value){
        compareList.add(new Compare(new Throwable().getStackTrace()[1].getMethodName(), column.getFieldNameLine(),value));
        return this;
    }
}
*/
