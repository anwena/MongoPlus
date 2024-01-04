package com.anwen.mongo.conditions.query;

import com.anwen.mongo.model.PageParam;
import com.anwen.mongo.model.PageResult;
import com.mongodb.client.ClientSession;

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
     * 获取列表 返回T类型的List
     * @return {@link List<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:13
     */
    @Deprecated
    List<T> list(ClientSession clientSession);

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
     * <p>注：如果查询到大于一条数据，会抛出{@link com.anwen.mongo.domain.MongoQueryException}异常</p>
     * @return T
     * @author JiaChaoYang
     * @date 2023/7/20 23:13
     */
    @Deprecated
    T one(ClientSession clientSession);

    /**
     * 获取单个，返回T类型的对象
     * <p>注：如果查询到大于一条数据，会取第一条返回</p>
     * @author JiaChaoYang
     * @date 2023/7/20 23:12
    */
    T limitOne();

    /**
     * 获取单个，返回T类型的对象
     * <p>注：如果查询到大于一条数据，会取第一条返回</p>
     * @author JiaChaoYang
     * @date 2023/7/20 23:12
     */
    @Deprecated
    T limitOne(ClientSession clientSession);

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
     * @param pageParam 分页参数对象
     * @return {@link PageResult<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
     */
    @Deprecated
    PageResult<T> page(ClientSession clientSession,PageParam pageParam);

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
     * @param pageNum 当前页
     * @param pageSize 每页显示行数
     * @return {@link PageResult<T>}
     * @author JiaChaoYang
     * @date 2023/7/20 23:17
     */
    @Deprecated
    PageResult<T> page(ClientSession clientSession,Integer pageNum, Integer pageSize);

    long count();

    @Deprecated
    long count(ClientSession clientSession);

}
