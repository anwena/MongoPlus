package com.anwen.mongo.sql.conditions.interfaces;

import com.anwen.mongo.sql.query.LambdaQueryChainWrapper;
import com.anwen.mongo.sql.support.SFunction;

public interface Query<T,Children> {

    Children select(SFunction<T,Object> column);

    /**
     * 或者
     * @param condition 判断如果为true，则加入此条件，可做判空，及不为空就加入这个条件
     * @param lambdaQueryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:59
     */
    Children or(boolean condition , LambdaQueryChainWrapper<T> lambdaQueryChainWrapper);

    /**
     * 或者
     * @param lambdaQueryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:44
     */
    Children or(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper);

    /**
     * 或者
     * @param condition 判断如果为true，则加入此条件，可做判空，及不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:59
     */
    Children or(boolean condition , SFunction<T,Object> column,Object value);

    /**
     * 并且 在or中使用
     * @param lambdaQueryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 22:10
     */
    Children and(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper);

    /**
     * 并且 在or中使用
     * @param condition 判断如果为true，则加入此条件，可做判空，及不为空就加入这个条件
     * @param lambdaQueryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 22:11
     */
    Children and(boolean condition,LambdaQueryChainWrapper<T> lambdaQueryChainWrapper);

    /**
     * 或者 单个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:48
     */
    Children or(SFunction<T,Object> column,Object value);

    /**
     * 或者
     * @param condition 判断如果为true，则加入此条件，可做判空，及不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 21:01
     */
    Children or(boolean condition,String column,Object value);

    /**
     * 或者 单个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:50
     */
    Children or(String column , Object value);

}
