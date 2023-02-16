package com.anwen.mongo.sql;

import com.anwen.mongo.annotation.CutInID;
import com.anwen.mongo.sql.interfaces.Compare;
import com.anwen.mongo.sql.interfaces.Order;
import com.anwen.mongo.sql.query.LambdaQueryMongoWrapper;
import com.anwen.mongo.sql.support.SFunction;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;

/**
 * @author JiaChaoYang
 * 增删改接口
 * @since 2023-02-09 13:25
 **/
public interface IService<T> {

    /**
     * 添加
     * @param entity 添加的对象
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:27
    */
    @CutInID
    Boolean save(T entity);

    /**
     * 添加多个
     * @param entityList 对象集合
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:56
    */
    Boolean saveBatch(Collection<T> entityList);

    /**
     * 添加或修改
     * @param entity 对象
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:57
    */
    Boolean saveOrUpdate(T entity);

    /**
     * 批量添加或修改
     * @param entityList 对象集合
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:57
    */
    Boolean saveOrUpdateBatch(Collection<T> entityList);

    /**
     * 修改
     * @param entity 修改的对象，需要包含id
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:28
    */
    Boolean updateById(T entity);

    Boolean updateBatchByIds(Collection<T> entityList);

    /**
     * 通过列进行修改
     * @param entity 修改的实体
     * @param column 根据什么列修改
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:46
    */
    Boolean updateByColumn(T entity, SFunction<T, Object> column);

    Boolean updateByColumn(T entity, String column);

    /**
     * 根据id删除
     * @param id 数据id
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:47
    */
    Boolean removeById(Serializable id);

    /**
     * 根据字段删除
     * @param column 字段名
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 14:01
    */
    Boolean removeByColumn(Function<T, Object> column,String value);

    /**
     * 根据字段删除
     * @param column 字段
     * @param value 值
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 14:05
    */
    Boolean removeByColumn(String column,String value);

    /**
     * 根据id批量删除
     * @param idList id集合
     * @return java.lang.Boolean
     * @author JiaChaoYang
     * @since 2023/2/9 13:59
    */
    Boolean removeBatchByIds(Collection<Object> idList);

    /**
     * 查询所有
     * @return java.util.List<T>
     * @author JiaChaoYang
     * @since 2023/2/10 9:48
    */
    List<T> list();

    List<T> list(List<Compare> compareList, List<Order> orderList);

    T one(List<Compare> compareList, List<Order> orderList);

    T getById(Serializable id);

    /**
     * 自定义构建查询
     * @return com.qihengyun.mongon.orm.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @since 2023/2/10 9:49
     */
    LambdaQueryMongoWrapper<T> lambdaQuery();
}
