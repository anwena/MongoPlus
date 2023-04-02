package com.anwen.mongo.sql.query;

import com.anwen.mongo.enums.OrderEnum;
import com.anwen.mongo.sql.ServiceImpl;
import com.anwen.mongo.sql.interfaces.Compare;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.support.SFunction;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author JiaChaoYang
 * 条件实现
 * @since 2023-02-10 10:20
 **/
public class AbstractChainWrapper<T> extends ServiceImpl<T> implements LambdaQueryMongoWrapper<T> {

    /**
     * 构建条件对象
     * @since 2023/2/10 12:00
    */
    List<Compare> compareList = new ArrayList<>();

    /**
     * 构建排序对象
     * @since 2023/2/10 12:00
    */
    List<Order> orderList = new ArrayList<>();

    private final ServiceImpl<T> serviceImpl;

    private final T t;

    public AbstractChainWrapper(Class<T> tClass,ServiceImpl<T> serviceImpl) {
        try {
            this.t = tClass.getDeclaredConstructor().newInstance();
            this.serviceImpl = serviceImpl;
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public LambdaQueryMongoWrapper<T> eq(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? eq(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> eq(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> eq(boolean condition, String column, Object value) {
        return condition ? eq(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> eq(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> ne(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? ne(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> ne(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> ne(boolean condition, String column, Object value) {
        return condition ? ne(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> ne(String column,Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> lt(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lt(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> lt(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> lt(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaQueryMongoWrapper<T> lt(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> lte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lte(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> lte(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> lte(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaQueryMongoWrapper<T> lte(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> gt(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gt(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> gt(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> gt(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaQueryMongoWrapper<T> gt(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> gte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gte(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> gte(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> gte(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaQueryMongoWrapper<T> gte(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> like(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? like(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> like(SFunction<T, Object> column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> like(boolean condition, String column, Object value) {
        return null;
    }

    @Override
    public LambdaQueryMongoWrapper<T> like(String column, Object value) {
        return getBaseCondition(column,value);
    }

    @Override
    public LambdaQueryMongoWrapper<T> in(boolean condition, SFunction<T, Object> column, Collection<Object> valueList) {
        return condition ? in(column,valueList) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> in(SFunction<T, Object> column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public LambdaQueryMongoWrapper<T> in(boolean condition, String column, Collection<Object> valueList) {
        return null;
    }

    @Override
    public LambdaQueryMongoWrapper<T> in(String column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public LambdaQueryMongoWrapper<T> orderBy(SFunction<T, Object> column) {
        return getBaseOrder(OrderEnum.ORDER_BY.getFlag(), column);
    }

    @Override
    public LambdaQueryMongoWrapper<T> orderByDesc(SFunction<T, Object> column) {
        return getBaseOrder(OrderEnum.ORDER_BY_DESC.getFlag(), column);
    }

    @Override
    public LambdaQueryMongoWrapper<T> orderBy(String column) {
        return getBaseOrder(OrderEnum.ORDER_BY.getFlag(), column);
    }

    @Override
    public LambdaQueryMongoWrapper<T> orderByDesc(String column) {
        return getBaseOrder(OrderEnum.ORDER_BY_DESC.getFlag(), column);
    }

    @Override
    public List<T> list() {
        return serviceImpl.list(compareList,orderList);
    }

    @Override
    public T one() {
        return serviceImpl.one(compareList,orderList);
    }


    public LambdaQueryMongoWrapper<T> getBaseOrder(Integer type , String column){
        orderList.add(new Order(type,column));
        return this;
    }

    public LambdaQueryMongoWrapper<T> getBaseOrder(Integer type , SFunction<T, Object> column){
        orderList.add(new Order(type,column.getFieldNameLine()));
        return this;
    }

    public LambdaQueryMongoWrapper<T> getBaseCondition(String column, Object value){
        compareList.add(new Compare(new Throwable().getStackTrace()[1].getMethodName(), column,value));
        return this;
    }

    public LambdaQueryMongoWrapper<T> getBaseCondition(SFunction<T, Object> column, Object value){
        compareList.add(new Compare(new Throwable().getStackTrace()[1].getMethodName(), column.getFieldNameLine(),value));
        return this;
    }
}
