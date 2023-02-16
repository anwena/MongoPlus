package com.anwen.mongo.sql.query;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.sql.ServiceImpl;
import com.anwen.mongo.sql.interfaces.Compare;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.support.SFunction;
import com.anwen.mongo.utils.GenericSuperclassUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

    private T t;

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
    public LambdaQueryMongoWrapper<T> ne(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? ne(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> ne(SFunction<T, Object> column, Object value) {
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
    public LambdaQueryMongoWrapper<T> lte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? lte(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> lte(SFunction<T, Object> column, Object value) {
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
    public LambdaQueryMongoWrapper<T> gte(boolean condition, SFunction<T, Object> column, Object value) {
        return condition ? gte(column,value) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> gte(SFunction<T, Object> column, Object value) {
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
    public LambdaQueryMongoWrapper<T> in(boolean condition, SFunction<T, Object> column, Collection<Object> valueList) {
        return condition ? in(column,valueList) : this;
    }

    @Override
    public LambdaQueryMongoWrapper<T> in(SFunction<T, Object> column, Collection<Object> valueList) {
        return getBaseCondition(column,valueList);
    }

    @Override
    public LambdaQueryMongoWrapper<T> orderBy(SFunction<T, Object> column) {
        return getBaseOrder(1,column);
    }

    @Override
    public LambdaQueryMongoWrapper<T> orderByDesc(SFunction<T, Object> column) {
        return getBaseOrder(-1,column);
    }

    @Override
    public List<T> list() {
        return serviceImpl.list(compareList,orderList);
    }

    @Override
    public T one() {
        return serviceImpl.one(compareList,orderList);
    }

    public LambdaQueryMongoWrapper<T> getBaseOrder(Integer type , SFunction<T, Object> column){
        orderList.add(new Order(type,column.getFieldName()));
        return this;
    }


    public LambdaQueryMongoWrapper<T> getBaseCondition(SFunction<T, Object> column, Object value){
        //TODO 第二版本加上注解
        Class<?> aClass = column.getFieldClass();
        compareList.add(new Compare(new Throwable().getStackTrace()[1].getMethodName(), column.getFieldName(),value));
        return this;
    }
}
