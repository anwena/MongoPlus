package com.anwen.mongo.conditions.interfaces;

import com.anwen.mongo.conditions.interfaces.condition.CompareCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.enums.TypeEnum;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import org.bson.conversions.Bson;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

/**
 * 查询条件封装
 * @author JiaChaoYang
 * @date 2023/6/24/024 1:37
*/
public interface Compare<T,Children> extends Serializable {

    /**
     * 等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children eq(boolean condition, SFunction<T,Object> column, Object value);

    /**
     * 等于
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children eq(SFunction<T,Object> column, Object value);

    /**
     * 等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children eq(boolean condition, String column, Object value);

    /**
     * 等于
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children eq(String column, Object value);

    /**
     * 不等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children ne(boolean condition , SFunction<T,Object> column, Object value);

    /**
     * 不等于
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children ne(SFunction<T,Object> column, Object value);

    /**
     * 不等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children ne(boolean condition , String column, Object value);

    /**
     * 等于
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children ne(String column, Object value);

    /**
     * 小于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children lt(boolean condition, SFunction<T,Object> column, Object value);

    /**
     * 小于
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children lt(SFunction<T,Object> column, Object value);

    /**
     * 等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children lt(boolean condition, String column, Object value);

    /**
     * 等于
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children lt(String column, Object value);

    /**
     * 小于等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children lte(boolean condition, SFunction<T,Object> column, Object value);

    /**
     * 小于等于
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children lte(SFunction<T,Object> column, Object value);

    /**
     * 小于等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children lte(boolean condition, String column, Object value);

    /**
     * 小于等于
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children lte(String column, Object value);

    /**
     * 大于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children gt(boolean condition, SFunction<T,Object> column, Object value);

    /**
     * 大于
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children gt(SFunction<T,Object> column, Object value);

    /**
     * 大于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children gt(boolean condition, String column, Object value);

    /**
     * 大于
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children gt(String column, Object value);
    /**
     * 大于等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children gte(boolean condition, SFunction<T,Object> column, Object value);

    /**
     * 大于等于
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children gte(SFunction<T,Object> column, Object value);

    /**
     * 大于等于
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children gte(boolean condition, String column, Object value);

    /**
     * 大于等于
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children gte(String column, Object value);

    /**
     * 包含（模糊查询）
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children like(boolean condition, SFunction<T,Object> column, Object value);

    /**
     * 包含（模糊查询）
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children like(SFunction<T,Object> column, Object value);

    /**
     * 包含（模糊查询）
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children like(boolean condition, String column, Object value);

    /**
     * 包含（模糊查询）
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children like(String column, Object value);

    /**
     * 左包含（模糊查询）
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children likeLeft(boolean condition , SFunction<T,Object> column, Object value);

    /**
     * 左包含（模糊查询）
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children likeLeft(SFunction<T,Object> column, Object value);

    /**
     * 左包含（模糊查询）
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children likeLeft(boolean condition , String column, Object value);

    /**
     * 左包含（模糊查询）
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children likeLeft(String column, Object value);

    /**
     * 右包含（模糊查询）
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children likeRight(boolean condition , SFunction<T,Object> column, Object value);

    /**
     * 右包含（模糊查询）
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children likeRight(SFunction<T,Object> column, Object value);

    /**
     * 右包含（模糊查询）
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children likeRight(boolean condition , String column, Object value);

    /**
     * 右包含（模糊查询）
     * @param column 列名、字段名，lambda方式
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children likeRight(String column, Object value);

    /**
     * 多值查询
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名，lambda方式
     * @param valueList 值的集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children in(boolean condition, SFunction<T,Object> column, Collection<?> valueList);

    /**
     * 多值查询
     * @param column 列名、字段名，lambda方式
     * @param valueList 值的集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children in(SFunction<T,Object> column, Collection<?> valueList);

    /**
     * 多值查询
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param valueList 值的集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children in(boolean condition, String column, Collection<?> valueList);

    /**
     * 多值查询
     * @param column 列名、字段名
     * @param valueList 值的集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children in(String column, Collection<?> valueList);

    /**
     * 不包含
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param valueList 值的集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/15 15:57
    */
    Children nin(boolean condition , SFunction<T,Object> column , Collection<?> valueList);

    /**
     * 不包含
     * @param column 列名、字段名
     * @param valueList 值的集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/15 15:57
    */
    Children nin(SFunction<T,Object> column , Collection<?> valueList);

    /**
     * 不包含
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param valueList 值的集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/15 15:58
    */
    Children nin(boolean condition , String column , Collection<?> valueList);

    /**
     * 不包含
     * @param column 列名、字段名
     * @param valueList 值的集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/15 15:58
    */
    Children nin(String column , Collection<?> valueList);

    /**
     * 并且 在or中使用
     * @param queryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 22:10
     */
    Children and(QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 并且 在or中使用
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param queryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 22:11
     */
    Children and(boolean condition,QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 或者
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param queryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:59
     */
    Children or(boolean condition , QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 或者
     * @param queryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:44
     */
    Children or(QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 或者
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:59
     */
    Children or(boolean condition , SFunction<T,Object> column,Object value);

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
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
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

    /**
     * 查询的文档必须不符合所有条件
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param queryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:59
     */
    Children nor(boolean condition , QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 查询的文档必须不符合所有条件
     * @param queryChainWrapper 链式查询
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:44
     */
    Children nor(QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 查询的文档必须不符合所有条件
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:59
     */
    Children nor(boolean condition , SFunction<T,Object> column,Object value);

    /**
     * 查询的文档必须不符合所有条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:48
     */
    Children nor(SFunction<T,Object> column,Object value);

    /**
     * 查询的文档必须不符合所有条件
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 21:01
     */
    Children nor(boolean condition,String column,Object value);

    /**
     * 查询的文档必须不符合所有条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/16 20:50
     */
    Children nor(String column , Object value);

    /**
     * 指定查询的字段类型
     * @param column 列名、字段名
     * @param value 枚举值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/18 0:10
    */
    Children type(SFunction<T,Object> column, TypeEnum value);

    /**
     * 指定查询的字段类型
     * @param column 列名、字段名
     * @param value 枚举值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/18 0:10
     */
    Children type(String column, TypeEnum value);

    /**
     * 指定查询的字段类型
     * @param column 列名、字段名
     * @param value 类型，参考{@link TypeEnum}的枚举
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/18 0:10
     */
    Children type(SFunction<T,Object> column, String value);

    /**
     * 指定查询的字段类型
     * @param column 列名、字段名
     * @param value 类型，参考{@link TypeEnum}的枚举
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/18 0:10
     */
    Children type(String column, String value);

    /**
     * 指定查询的字段类型
     * @param column 列名、字段名
     * @param value 类型，参考{@link TypeEnum}的枚举
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/18 0:10
     */
    Children type(SFunction<T,Object> column, Integer value);

    /**
     * 指定查询的字段类型
     * @param column 列名、字段名
     * @param value 类型，参考{@link TypeEnum}的枚举
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/18 0:10
     */
    Children type(String column, Integer value);

    /**
     * 字段是否存在
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 22:46
    */
    Children exists(boolean condition,SFunction<T,Object> column,Boolean value);

    /**
     * 字段是否存在
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 22:47
    */
    Children exists(SFunction<T,Object> column,Boolean value);

    /**
     * 字段是否存在
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 22:46
     */
    Children exists(boolean condition,String column,Boolean value);

    /**
     * 字段是否存在
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 22:47
     */
    Children exists(String column,Boolean value);

    Children not(CompareCondition compareCondition);


    Children not(boolean condition,CompareCondition compareCondition);

    /**
     * 进行计算的表达式
     * @author JiaChaoYang
     * @date 2023/7/19 23:04
    */
    Children expr(CompareCondition compareCondition);

    /**
     * 进行计算的表达式
     * @author JiaChaoYang
     * @date 2023/7/19 23:04
     */
    Children expr(boolean condition,CompareCondition compareCondition);

    /**
     * 字段值符合余数
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param divide 被除数
     * @param remain 余数
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:13
    */
    Children mod(boolean condition,SFunction<T,Object> column,long divide,long remain);

    /**
     * 字段值符合余数
     * @param column 列名、字段名
     * @param divide 被除数
     * @param remain 余数
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:12
    */
    Children mod(SFunction<T,Object> column,long divide,long remain);

    /**
     * 字段值符合余数
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 传入集合，第一个值为除数，第二个值为余数
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:13
    */
    Children mod(boolean condition,SFunction<T,Object> column,Collection<Long> value);

    /**
     * 字段值符合余数
     * @param column 列名、字段名
     * @param value 传入集合，第一个值为除数，第二个值为余数
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:14
    */
    Children mod(SFunction<T,Object> column,Collection<Long> value);

    /**
     * 字段值符合余数
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param divide 被除数
     * @param remain 余数
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:14
    */
    Children mod(boolean condition,String column , long divide,long remain);

    /**
     * 字段值符合余数
     * @param column 列名、字段名
     * @param divide 被除数
     * @param remain 余数
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:14
    */
    Children mod(String column , long divide,long remain);

    /**
     * 字段值符合余数
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 传入集合，第一个值为除数，第二个值为余数
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:15
    */
    Children mod(boolean condition,String column,Collection<Long> value);

    /**
     * 字段值符合余数
     * @param column 列名、字段名
     * @param value 传入集合，第一个值为除数，第二个值为余数
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:15
    */
    Children mod(String column,Collection<Long> value);

    /**
     * 匹配数组中的值
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param queryChainWrapper 查询条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:29
    */
    Children elemMatch(boolean condition,SFunction<T,Object> column , QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 匹配数组中的值
     * @param column 列名、字段名
     * @param queryChainWrapper 查询条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:31
    */
    Children elemMatch(SFunction<T,Object> column , QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 匹配数组中的值
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param queryChainWrapper 查询条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:29
     */
    Children elemMatch(boolean condition,String column , QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 匹配数组中的值
     * @param column 列名、字段名
     * @param queryChainWrapper 查询条件
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:31
     */
    Children elemMatch(String column , QueryChainWrapper<?,?> queryChainWrapper);

    /**
     * 匹配数组中的值 必须同时包含指定的多个元素
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:36
    */
    Children all(boolean condition,SFunction<T,Object> column,Collection<?> value);

    /**
     * 匹配数组中的值 必须同时包含指定的多个元素
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:36
    */
    Children all(SFunction<T,Object> column,Collection<?> value);

    /**
     * 匹配数组中的值 必须同时包含指定的多个元素
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:37
    */
    Children all(boolean condition,String column,Collection<?> value);

    /**
     * 匹配数组中的值 必须同时包含指定的多个元素
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/19 23:37
    */
    Children all(String column,Collection<?> value);

    /**
     * 正则表达式查询
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值（可传入{@link java.util.regex.Pattern}对象）
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/30 0:18
    */
    Children regex(boolean condition, SFunction<T,Object> column, Object value);

    /**
     * 正则表达式查询
     * @param column 列名、字段名
     * @param value 值（可传入{@link java.util.regex.Pattern}对象）
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/30 0:19
    */
    Children regex(SFunction<T,Object> column,Object value);

    /**
     * 正则表达式查询
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值（可传入{@link java.util.regex.Pattern}对象）
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/30 0:18
     */
    Children regex(boolean condition,String column,Object value);

    /**
     * 正则表达式查询
     * @param column 列名、字段名
     * @param value 值（可传入{@link java.util.regex.Pattern}对象）
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/30 0:19
     */
    Children regex(String column,Object value);

    /**
     * 文本查询
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/30 1:06
    */
    Children text(boolean condition , SFunction<T,Object> column, Object value);

    /**
     * 文本查询
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/30 1:06
     */
    Children text(SFunction<T,Object> column, Object value);

    /**
     * 文本查询
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/30 1:06
     */
    Children text(boolean condition , String column, Object value);

    /**
     * 文本查询
     * @param column 列名、字段名
     * @param value 值
     * @return Children
     * @author JiaChaoYang
     * @date 2023/7/30 1:06
     */
    Children text(String column, Object value);

    /**
     * 在。。。之间
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param gte 大于等于
     * @param lte 小于等于
     * @param convertGtOrLt 设置为true，则转换为大于-小于，默认为大于等于和小于等于
     * @return Children
     * @author JiaChaoYang
     * @date 2023/11/14 11:02
    */
    Children between(boolean condition , SFunction<T,Object> column,Object gte,Object lte,boolean convertGtOrLt);

    /**
     * 在。。。之间
     * @param column 列名、字段名
     * @param gte 大于等于
     * @param lte 小于等于
     * @param convertGtOrLt 设置为true，则转换为大于-小于，默认为大于等于和小于等于
     * @return Children
     * @author JiaChaoYang
     * @date 2023/11/14 11:02
     */
    Children between(SFunction<T,Object> column,Object gte,Object lte,boolean convertGtOrLt);

    /**
     * 在。。。之间
     * @param condition 判断如果为true，则加入此条件，可做判空，即不为空就加入这个条件
     * @param column 列名、字段名
     * @param gte 大于等于
     * @param lte 小于等于
     * @param convertGtOrLt 设置为true，则转换为大于-小于，默认为大于等于和小于等于
     * @return Children
     * @author JiaChaoYang
     * @date 2023/11/14 11:02
     */
    Children between(boolean condition,String column,Object gte,Object lte,boolean convertGtOrLt);

    /**
     * 在。。。之间
     * @param column 列名、字段名
     * @param gte 大于等于
     * @param lte 小于等于
     * @param convertGtOrLt 设置为true，则转换为大于-小于，默认为大于等于和小于等于
     * @return Children
     * @author JiaChaoYang
     * @date 2023/11/14 11:02
     */
    Children between(String column,Object gte,Object lte,boolean convertGtOrLt);

    Children custom(BasicDBObject basicDBObject);

    Children custom(Bson bson);

    Children custom(List<BasicDBObject> basicDBObjectList);
}
