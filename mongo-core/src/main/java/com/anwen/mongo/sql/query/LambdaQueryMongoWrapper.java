package com.anwen.mongo.sql.query;

import com.anwen.mongo.sql.IService;
import com.anwen.mongo.sql.support.SFunction;

import java.util.Collection;
import java.util.List;

/**
 * 条件构造接口
 * @author JiaChaoYang
 * @since 2023/2/14 14:18
*/
public interface LambdaQueryMongoWrapper<T> extends IService<T> {

    /**
     * 等于
     **/
    LambdaQueryMongoWrapper<T> eq(boolean condition, SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> eq(SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> ne(boolean condition , SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> ne(SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> lt(boolean condition,SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> lt(SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> lte(boolean condition,SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> lte(SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> gt(boolean condition,SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> gt(SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> gte(boolean condition,SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> gte(SFunction<T,Object> column, Object value);

    LambdaQueryMongoWrapper<T> like(boolean condition,SFunction<T,Object> column,Object value);

    LambdaQueryMongoWrapper<T> like(SFunction<T,Object> column,Object value);

    LambdaQueryMongoWrapper<T> in(boolean condition,SFunction<T,Object> column, Collection<Object> valueList);

    LambdaQueryMongoWrapper<T> in(SFunction<T,Object> column, Collection<Object> valueList);

    LambdaQueryMongoWrapper<T> orderBy(SFunction<T,Object> column);

    LambdaQueryMongoWrapper<T> orderByDesc(SFunction<T,Object> column);

    List<T> list();

    T one();

}
