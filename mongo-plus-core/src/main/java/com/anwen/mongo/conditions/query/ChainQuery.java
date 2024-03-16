package com.anwen.mongo.conditions.query;

import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;

import java.util.List;

/**
 * 查询方法定义
 * @author JiaChaoYang
 * @date 2023/6/24/024 2:01
*/
public interface ChainQuery<T> {

    /**
     * 获取列表 返回T类型的List
     * @return {@link List<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:13
    */
    List<T> list();

    /**
     * 获取单个，返回T类型的对象
     * <p>注：如果查询到大于一条数据，会抛出{@link com.anwen.mongo.domain.MongoQueryException}异常</p>
     * @return T
     * @author JiaChaoYang
     * @date 2023/7/20 23:13
    */
    T one();

    /**
     * 获取单个，返回T类型的对象
     * <p>注：如果查询到大于一条数据，会取第一条返回</p>
     * @author JiaChaoYang
     * @date 2023/7/20 23:12
    */
    @Deprecated
    T limitOne();

    /**
     * 分页
     * @param pageParam 分页参数对象
     * @return {@link PageResult<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
    */
    PageResult<T> page(PageParam pageParam);

    /**
     * 分页
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link PageResult<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
    */
    PageResult<T> page(Integer pageNum, Integer pageSize);

    /**
     * 分页
     * @param pageParam 分页参数对象
     * @param recentPageNum 查询最近n页的数据  {参数=null 表示仅查询当前页数据}  {参数取值[5-50] 表示查询最近[5-50]页的数据 建议recentPageNum等于10 参考 百度分页检索}
     * @return {@link PageResult<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
    */
    PageResult<T> page(PageParam pageParam, Integer recentPageNum);

    /**
     * 分页
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @param recentPageNum 查询最近n页的数据  {参数=null 表示仅查询当前页数据}  {参数取值[5-50] 表示查询最近[5-50]页的数据 建议recentPageNum等于10 参考 百度分页检索}
     * @return {@link PageResult<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
    */
    PageResult<T> page(Integer pageNum, Integer pageSize, Integer recentPageNum);

    long count();
}
