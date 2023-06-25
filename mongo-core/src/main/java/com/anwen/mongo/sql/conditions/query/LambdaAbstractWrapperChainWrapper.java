/*
package com.anwen.mongo.sql.query;

import com.anwen.mongo.conditions.AbstractChainWrapper;
import com.anwen.mongo.sql.interfaces.Condition;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;
import com.anwen.mongo.sql.support.SFunction;

import java.util.List;

*/
/**
 * 链式查询 条件构造接口
 * @author JiaChaoYang
 * @since 2023/2/14 14:18
*//*

public interface LambdaAbstractWrapperChainWrapper<T> extends AbstractChainWrapper<T> {

    */
/**
     * 正序排序
     * @param column 列名、字段名，lambda方式
     * @return com.anwen.mongo.sql.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
    *//*

    LambdaAbstractWrapperChainWrapper<T> orderByAsc(SFunction<T, Object> column);

    */
/**
     * 倒序排序
     * @param column 列名、字段名，lambda方式
     * @return com.anwen.mongo.sql.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
     *//*

    LambdaAbstractWrapperChainWrapper<T> orderByDesc(SFunction<T,Object> column);

    */
/**
     * 正序排序
     * @param column 列名、字段名
     * @return com.anwen.mongo.sql.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
     *//*

    LambdaAbstractWrapperChainWrapper<T> orderByAsc(String column);

    */
/**
     * 倒序排序
     * @param column 列名、字段名，lambda方式
     * @return com.anwen.mongo.sql.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
     *//*

    LambdaAbstractWrapperChainWrapper<T> orderByDesc(String column);

    */
/**
     * 分页查询，返回PageResult对象
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return com.anwen.mongo.sql.model.PageResult<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
    *//*

    PageResult<T> page(Integer pageNum, Integer pageSize);

    */
/**
     * 分页查询，返回PageResult对象
     * @param pageParam 分页参数对象
     * @return com.anwen.mongo.sql.model.PageResult<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
    *//*

    PageResult<T> page(PageParam pageParam);

    */
/**
     * 查询所有，返回一个集合
     * @author JiaChaoYang
     * @date 2023/6/20/020 23:54
    *//*

    List<T> list();

    */
/**
     * 查询所有，返回单个对象
     * @author JiaChaoYang
     * @date 2023/6/20/020 23:54
    *//*

    T one();

    Condition getQueryCondition();


}
*/
