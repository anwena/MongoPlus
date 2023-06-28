package com.anwen.mongo.sql;

import com.anwen.mongo.annotation.CutInID;
import com.anwen.mongo.sql.conditions.query.LambdaQueryChainWrapper;
import com.anwen.mongo.sql.conditions.update.LambdaUpdateChainWrapper;
import com.anwen.mongo.sql.model.PageParam;
import com.anwen.mongo.sql.model.PageResult;
import com.anwen.mongo.sql.support.SFunction;
import com.anwen.mongo.toolkit.ChainWrappers;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;


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
    Boolean removeByColumn(SFunction<T, Object> column, String value);

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

    T one(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper);

    List<T> list(LambdaQueryChainWrapper<T> lambdaQueryChainWrapper);

    /**
     * 分页查询
     * @param pageParam 分页参数对象
     * @return com.anwen.mongo.sql.model.PageResult<T>
     * @author JiaChaoYang
     * @date 2023/6/25/025
    */
    PageResult<T> page(PageParam pageParam);

    /**
     * 分页查询
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return com.anwen.mongo.sql.model.PageResult<T>
     * @author JiaChaoYang
     * @date 2023/6/25/025
    */
    PageResult<T> page(Integer pageNum,Integer pageSize);

    /**
     * 根据id查询单个
     * @param id id
     * @return T
     * @author JiaChaoYang
     * @date 2023/6/25/025
    */
    T getById(Serializable id);

    SqlOperation<T> getSqlOperation();

    <T> Class<T> getEClass();


    default LambdaQueryChainWrapper<T> lambdaQuery(){
        return ChainWrappers.lambdaQueryChain(getEClass(),getSqlOperation());
    }

    default LambdaUpdateChainWrapper<T> lambdaUpdate(){
        return ChainWrappers.lambdaUpdateChain(getSqlOperation());
    }

}
